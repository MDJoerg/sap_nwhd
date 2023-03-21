CLASS zcl_nwhd_col_strust DEFINITION
  PUBLIC
  INHERITING FROM zcl_nwhd_col
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF psetype,
             applic   TYPE strustssl-applic,
             descript TYPE strustsslt-descript,
             context  TYPE psecontext,
           END OF psetype.


    METHODS zif_nwhd_mod~get_name
        REDEFINITION .


  PROTECTED SECTION.

    DATA:
* --------- local data
      mt_pse TYPE TABLE OF psetype .
* -------- counters
    DATA mv_cnt_pse_found TYPE i .
    DATA mv_cnt_pse_error TYPE i .
    DATA mv_cnt_pse_cert_err TYPE i .
    DATA mv_cnt_pse_cert_invalid TYPE i .
    DATA mv_cnt_pse_cert_today TYPE i .
    DATA mv_cnt_pse_cert_24h TYPE i .
    DATA mv_cnt_pse_cert_week TYPE i .
    DATA mv_cnt_pse_cert_month TYPE i .
    DATA mv_cnt_pse_cert_year TYPE i .
    DATA mv_cnt_cert_all TYPE i .
    DATA mv_cnt_cert_error TYPE i .
    DATA mv_cnt_cert_invalid TYPE i .
    DATA mv_cnt_cert_today TYPE i .
    DATA mv_cnt_cert_24h TYPE i .
    DATA mv_cnt_cert_week TYPE i .
    DATA mv_cnt_cert_month TYPE i .
    DATA mv_cnt_cert_year TYPE i .

    METHODS check_cert
      IMPORTING
        !iv_cert          TYPE xstring
      RETURNING
        VALUE(rv_success) TYPE abap_bool .
    METHODS check_pse
      IMPORTING
        !is_pse           TYPE psetype
      RETURNING
        VALUE(rv_success) TYPE abap_bool .
    METHODS get_date_delta_from_utc
      IMPORTING
        !iv_utc              TYPE c
      RETURNING
        VALUE(rv_delta_days) TYPE i .
    METHODS publish
      RETURNING
        VALUE(rv_success) TYPE abap_bool .

    METHODS collect_data
        REDEFINITION .

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_NWHD_COL_STRUST IMPLEMENTATION.


  METHOD check_cert.
* --------- local data
    DATA:  lv_cert      TYPE xstring.
    DATA:  lv_subject(512) TYPE c.
    DATA:  lv_issuer(512)  TYPE c.
    DATA:  lv_utctime(15)  TYPE c. "ssfutc.

    rv_success = abap_true.

* ------- check certificate
    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate         = iv_cert
      IMPORTING
        subject             = lv_subject
        issuer              = lv_issuer
*       SERIALNO            =
*       VALIDFROM           =
        validto             = lv_utctime
*       ALGID               =
*       FINGERPRINT         =
*       SUMMARY             =
*       ALL                 =
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      ADD 1 TO mv_cnt_cert_error.
      RETURN.
    ENDIF.

* ------------ check pse validity
    DATA(lv_delta) = get_date_delta_from_utc( lv_utctime ).
    IF lv_delta < 0.
      ADD 1 TO mv_cnt_cert_invalid.
    ELSEIF lv_delta = 0.
      ADD 1 TO mv_cnt_cert_today.
    ELSEIF lv_delta = 1.
      ADD 1 TO mv_cnt_cert_24h.
    ELSEIF lv_delta <= 7.
      ADD 1 TO mv_cnt_cert_week.
    ELSEIF lv_delta <= 30.
      ADD 1 TO mv_cnt_cert_month.
    ELSEIF lv_delta <= 365.
      ADD 1 TO mv_cnt_cert_year.
    ENDIF.


  ENDMETHOD.


  METHOD check_pse.

* --------- local data
    DATA:  lv_cert      TYPE xstring.
    DATA:  lt_pklist   TYPE ssfbintab.
    DATA:  lv_subject(512) TYPE c.
    DATA:  lv_issuer(512)  TYPE c.
    DATA:  lv_utctime(15)  TYPE c. "ssfutc.
*    DATA:  lv_date         TYPE d.
*    DATA:  lv_diff         TYPE i.

    rv_success = abap_true.

* --------- check pse
    CALL FUNCTION 'SSFP_GET_PSEINFO'
      EXPORTING
        context           = is_pse-context
        applic            = is_pse-applic
        accept_no_cert    = 'X'
      IMPORTING
        certificate       = lv_cert
        certificatelist   = lt_pklist
      EXCEPTIONS
        ssf_no_ssflib     = 1
        ssf_krn_error     = 2
        ssf_invalid_par   = 3 "PSE does not exist
        ssf_unknown_error = 4
        OTHERS            = 5.
    IF sy-subrc NE 0.
      ADD 1 TO mv_cnt_pse_error.
      RETURN.
    ENDIF.


* ----------- check pse certificate
    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate         = lv_cert
      IMPORTING
        subject             = lv_subject
        issuer              = lv_issuer
*       SERIALNO            =
*       VALIDFROM           =
        validto             = lv_utctime
*       ALGID               =
*       FINGERPRINT         =
*       SUMMARY             =
*       ALL                 =
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      ADD 1 TO mv_cnt_pse_cert_err.
      RETURN.
    ENDIF.

* ------------ check pse validity
    DATA(lv_delta) = get_date_delta_from_utc( lv_utctime ).
    IF lv_delta < 0.
      ADD 1 TO mv_cnt_pse_cert_invalid.
    ELSEIF lv_delta = 0.
      ADD 1 TO mv_cnt_pse_cert_today.
    ELSEIF lv_delta = 1.
      ADD 1 TO mv_cnt_pse_cert_24h.
    ELSEIF lv_delta <= 7.
      ADD 1 TO mv_cnt_pse_cert_week.
    ELSEIF lv_delta <= 30.
      ADD 1 TO mv_cnt_pse_cert_month.
    ELSEIF lv_delta <= 365.
      ADD 1 TO mv_cnt_pse_cert_year.
    ENDIF.


* ------------ check given certifcates list
    DATA(lv_lin) = lines( lt_pklist ).
    IF lv_lin = 0.
      RETURN. "nothing to do
    ELSE.
      ADD lv_lin TO mv_cnt_cert_all.
    ENDIF.



* ------------ check all certificates
    LOOP AT lt_pklist ASSIGNING FIELD-SYMBOL(<lv_cert>).
      check_cert( <lv_cert> ).
    ENDLOOP.


* ------------ return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD collect_data.

* --------- local data
    DATA:  lv_cert      TYPE xstring.
    DATA:  lt_pklist   TYPE ssfbintab.
    DATA:  lv_subject(512) TYPE c.
    DATA:  lv_issuer(512)  TYPE c.
    DATA:  lv_utctime(15)  TYPE c. "ssfutc.
    DATA:  lv_date         TYPE d.
    DATA:  lv_diff         TYPE i.


* --------- get all pse
    PERFORM create_pse_list IN PROGRAM ssf_alert_certexpire
      CHANGING mt_pse IF FOUND.

    mv_cnt_pse_found = lines( mt_pse ).

    append_number_value(
        iv_category            = 'PSE'
        iv_key                 = 'CountAll'
        iv_value               = mv_cnt_pse_found
        iv_is_detail_level     =  zif_nwhd_c=>c_detail_level-very_important
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    IF mv_cnt_pse_found = 0.
      rv_success = abap_true. " nothing to do
      RETURN.
    ENDIF.



* ---------- loop and check pse
    LOOP AT mt_pse ASSIGNING FIELD-SYMBOL(<ls_pse>).
      IF check_pse( <ls_pse> ) EQ abap_false.
        "return.
      ENDIF.
    ENDLOOP.

* ----------- publish
    rv_success = publish( ).

  ENDMETHOD.


  METHOD get_date_delta_from_utc.

    DATA:  lv_utctime(15)  TYPE c. "ssfutc.
    DATA:  lv_date         TYPE d.

    lv_utctime = iv_utc.
    lv_date    = lv_utctime(8).
    rv_delta_days = lv_date - sy-datum.

  ENDMETHOD.


  METHOD publish.

* --------- local data/const
    CONSTANTS c_cat_pse  TYPE string VALUE 'PSE'.
    CONSTANTS c_cat_cert TYPE string VALUE 'Cert'.


* --------- pse
    append_number_value(
      EXPORTING
        iv_category            = c_cat_pse
        iv_key                 = 'CountError'
        iv_value               = mv_cnt_pse_cert_err
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_pse
        iv_key                 = 'Invalid'
        iv_value               = mv_cnt_pse_cert_invalid
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_pse
        iv_key                 = 'ValidTodayOnly'
        iv_value               = mv_cnt_pse_cert_today
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_hour                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_pse
        iv_key                 = 'ValidLastDay'
        iv_value               = mv_cnt_pse_cert_24h
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_24h                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_pse
        iv_key                 = 'ValidLastWeek'
        iv_value               = mv_cnt_pse_cert_week
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_week                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_pse
        iv_key                 = 'ValidLastMonth'
        iv_value               = mv_cnt_pse_cert_month
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_month                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_pse
        iv_key                 = 'ValidLastYear'
        iv_value               = mv_cnt_pse_cert_year
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-low_importance                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_year             " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

* --------- certificates
    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'CountAll'
        iv_value               = mv_cnt_cert_all
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'CountError'
        iv_value               = mv_cnt_cert_error
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'Invalid'
        iv_value               = mv_cnt_cert_invalid
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'ValidTodayOnly'
        iv_value               = mv_cnt_cert_today
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_hour                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'ValidLastDay'
        iv_value               = mv_cnt_cert_24h
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_24h                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'ValidLastWeek'
        iv_value               = mv_cnt_cert_week
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_week                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'ValidLastMonth'
        iv_value               = mv_cnt_cert_month
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_month                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = c_cat_cert
        iv_key                 = 'ValidLastYear'
        iv_value               = mv_cnt_cert_year
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-low_importance                " NWHD: detail level for collectors
        iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_year             " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    rv_success = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_mod~get_name.
    rv_name = 'STRUST'.
  ENDMETHOD.
ENDCLASS.
