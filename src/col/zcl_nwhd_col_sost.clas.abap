class ZCL_NWHD_COL_SOST definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  methods ZIF_NWHD_MOD~GET_NAME
    redefinition .
  PROTECTED SECTION.

    METHODS select_and_publish
      IMPORTING
        !iv_date          TYPE sy-datum
        !iv_time          TYPE sy-uzeit
        !iv_category      TYPE data
        !iv_timeint_level TYPE znwhd_col_timeint_level
      RETURNING
        VALUE(rv_success) TYPE abap_bool .

    METHODS collect_data
        REDEFINITION .

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_NWHD_COL_SOST IMPLEMENTATION.


METHOD collect_data.
* -------- local data
  DATA lv_date_from TYPE sydatum.
  DATA lv_time_from TYPE syuzeit.

* -------- last hour
  IF is_valid_info(
           iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important
           iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_hour
           iv_is_system_wide_info = abap_true
           iv_is_client_specific  = abap_false
         ) EQ abap_true.

    get_datetime_last_hour(
      EXPORTING
        iv_date = sy-datum
        iv_time = sy-uzeit
      IMPORTING
        ev_time = lv_time_from
      RECEIVING
        rv_date = lv_date_from
    ).

    IF select_and_publish(
        iv_date           = lv_date_from
        iv_time           = lv_time_from
        iv_category       = zif_nwhd_c=>c_category-last_hour
        iv_timeint_level  = zif_nwhd_c=>c_timeint_level-last_hour
    ) EQ abap_false.
      RETURN.
    ENDIF.
  ENDIF.


* -------- 24h
  IF is_valid_info(
           iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important
           iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_24h
           iv_is_system_wide_info = abap_true
           iv_is_client_specific  = abap_false
         ) EQ abap_true.

    get_datetime_last_24h(
      EXPORTING
        iv_date = sy-datum
        iv_time = sy-uzeit
      IMPORTING
        ev_time = lv_time_from
      RECEIVING
        rv_date = lv_date_from
    ).

    IF select_and_publish(
        iv_date           = lv_date_from
        iv_time           = lv_time_from
        iv_category       = zif_nwhd_c=>c_category-last_24h
        iv_timeint_level  = zif_nwhd_c=>c_timeint_level-last_24h
    ) EQ abap_false.
      RETURN.
    ENDIF.
  ENDIF.



* -------- week
  IF is_valid_info(
           iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance
           iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_week
           iv_is_system_wide_info = abap_true
           iv_is_client_specific  = abap_false
         ) EQ abap_true.

    get_datetime_last_week(
      EXPORTING
        iv_date = sy-datum
        iv_time = sy-uzeit
      IMPORTING
        ev_time = lv_time_from
      RECEIVING
        rv_date = lv_date_from
    ).

    IF select_and_publish(
        iv_date           = lv_date_from
        iv_time           = lv_time_from
        iv_category       = zif_nwhd_c=>c_category-last_week
        iv_timeint_level  = zif_nwhd_c=>c_timeint_level-last_week
    ) EQ abap_false.
      RETURN.
    ENDIF.
  ENDIF.


* -------- month
  IF is_valid_info(
           iv_is_detail_level     = zif_nwhd_c=>c_detail_level-low_importance
           iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_month
           iv_is_system_wide_info = abap_true
           iv_is_client_specific  = abap_false
         ) EQ abap_true.

    get_datetime_last_month(
      EXPORTING
        iv_date = sy-datum
        iv_time = sy-uzeit
      IMPORTING
        ev_time = lv_time_from
      RECEIVING
        rv_date = lv_date_from
    ).

    IF select_and_publish(
        iv_date           = lv_date_from
        iv_time           = lv_time_from
        iv_category       = zif_nwhd_c=>c_category-last_month
        iv_timeint_level  = zif_nwhd_c=>c_timeint_level-last_month
    ) EQ abap_false.
      RETURN.
    ENDIF.
  ENDIF.


* -------- year
  IF is_valid_info(
           iv_is_detail_level     = zif_nwhd_c=>c_detail_level-lowest_importance
           iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-last_year
           iv_is_system_wide_info = abap_true
           iv_is_client_specific  = abap_false
         ) EQ abap_true.

    get_datetime_last_year(
      EXPORTING
        iv_date = sy-datum
        iv_time = sy-uzeit
      IMPORTING
        ev_time = lv_time_from
      RECEIVING
        rv_date = lv_date_from
    ).

    IF select_and_publish(
        iv_date           = lv_date_from
        iv_time           = lv_time_from
        iv_category       = zif_nwhd_c=>c_category-last_year
        iv_timeint_level  = zif_nwhd_c=>c_timeint_level-last_year
    ) EQ abap_false.
      RETURN.
    ENDIF.
  ENDIF.



* ---------- finally true
  rv_success = abap_true.


ENDMETHOD.


  METHOD select_and_publish.

* ------ local data
    DATA: lt_snd_date TYPE  sxdatrngt.
    DATA: lt_snd_time TYPE  sxtimrngt.
    DATA: ls_snd_date LIKE LINE OF lt_snd_date.
    DATA: ls_snd_time LIKE LINE OF lt_snd_time.
    DATA: ls_status TYPE  soststatus.
    DATA: lt_recs TYPE soxsp2tab.


* ------- prepare
    ls_status-error   = 'X'.
    ls_status-wait    = 'X'.
    ls_status-ok      = 'X'.
    ls_status-retry   = 'X'.
    ls_status-transit = 'X'.
    ls_status-active  = 'X'.


* ------- check range
    IF iv_date = sy-datum.
      ls_snd_date-sign    = 'I'.
      ls_snd_date-option  = 'EQ'.
      ls_snd_date-low     = iv_date.
      APPEND ls_snd_date TO lt_snd_date.

      ls_snd_time-sign    = 'I'.
      ls_snd_time-option  = 'BT'.
      ls_snd_time-low     = iv_time.
      ls_snd_time-high    = sy-uzeit.
      APPEND ls_snd_time TO lt_snd_time.
    ELSE.
      ls_snd_date-sign    = 'I'.
      ls_snd_date-option  = 'BT'.
      ls_snd_date-low     = iv_date.
      ls_snd_date-high     = sy-datum.
      APPEND ls_snd_date TO lt_snd_date.
    ENDIF.



* ------- call
    CALL FUNCTION 'SX_SNDREC_SELECT'
      EXPORTING
*       SND_ART  =
        snd_date = lt_snd_date
        snd_time = lt_snd_time
*       DEL_DATE =
*       DEL_TIME =
        status   = ls_status
*       NOTIFICATIONS       =
*       SENDER   =
*       MAXSEL   =
*       ALL_WAITING         = 'X'
*       DESCRIPTION         =
      IMPORTING
        sndrecs  = lt_recs.


* ---------- publish
    DATA(lv_lin) = lines( lt_recs ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Count'
        iv_value               = lv_lin                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).


* -------- count
    DATA(lv_lin_success) = 0.
    DATA(lv_lin_error) = 0.
    DATA(lv_lin_wait) = 0.
*    DATA(lv_lin_retry) = 0.
*    DATA(lv_lin_active) = 0.
    DATA(lv_lin_sent) = 0.
    DATA(lv_lin_unknown) = 0.


    LOOP AT lt_recs ASSIGNING FIELD-SYMBOL(<ls_rec>).

* ------ calc main info (msgid SO)
      CASE <ls_rec>-status.
        WHEN 'W' OR '672'.
          lv_lin_wait    = lv_lin_wait    + 1.
        WHEN '718'.
          lv_lin_sent    = lv_lin_sent    + 1.
        WHEN '73'.
          lv_lin_success = lv_lin_success + 1.
        WHEN '826'.
          lv_lin_error   = lv_lin_error   + 1.
        WHEN OTHERS.
          IF <ls_rec>-status(1) = '8'.
            lv_lin_error   = lv_lin_error   + 1.
          ELSEIF <ls_rec>-status(1) = '7'.
            lv_lin_sent   = lv_lin_sent   + 1.
          ELSEIF <ls_rec>-status(1) = '6'.
            lv_lin_wait   = lv_lin_wait   + 1.
          ELSE.
            lv_lin_unknown = lv_lin_unknown + 1.
          ENDIF.
      ENDCASE.


    ENDLOOP.


* ----------- publish

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Errors'
        iv_value               = lv_lin_error                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).


    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Wait'
        iv_value               = lv_lin_wait                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).



    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Sent'
        iv_value               = lv_lin_sent                " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Success'
        iv_value               = lv_lin_success                " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).


*    append_number_value(
*      EXPORTING
*        iv_category            = iv_category
*        iv_key                 = 'Active'
*        iv_value               = lv_lin_active               " NWHD Number data type
*        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-low_importance              " NWHD: detail level for collectors
*        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
*        iv_is_system_wide_info = abap_true
*        iv_is_client_specific  = abap_false
*    ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Unknown'
        iv_value               = lv_lin_unknown               " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance              " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).


* ----------- finally success
    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_MOD~GET_NAME.
    rv_name = 'SOST'.
  ENDMETHOD.
ENDCLASS.
