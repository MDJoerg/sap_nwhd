class ZCL_NWHD_COL_BAL definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'BALog' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods SELECT_AND_PUBLISH
    importing
      !IV_DATE type SY-DATUM
      !IV_TIME type SY-UZEIT
      !IV_CATEGORY type DATA
      !IV_TIMEINT_LEVEL type ZNWHD_COL_TIMEINT_LEVEL
    returning
      value(RV_SUCCESS) type ABAP_BOOL .

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_BAL IMPLEMENTATION.


  METHOD collect_data.

* -------- local data
    DATA lv_date_from TYPE sydatum.
    DATA lv_time_from TYPE syuzeit.
    DATA lt_bal TYPE znwhd_t_col_bal_grb_stat.
    DATA lv_db_count TYPE sydbcnt.

* -------- select and publish db count
    SELECT COUNT( * )
      FROM balhdr
      INTO lv_db_count.

    append_number_value(
      EXPORTING
        iv_category             = 'DB'
        iv_key                  = 'Count'
        iv_value                = lv_db_count                 " NWHD Number data type
        iv_is_system_wide_info  = abap_true
        iv_is_client_specific   = abap_false
        iv_is_timeint_level     = zif_nwhd_c=>c_timeint_level-unspecified
        iv_is_detail_level      = zif_nwhd_c=>c_detail_level-very_important
    ).


* -------- BAP last hour
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


* -------- BAL 24 h
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

* -------- BAL week
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


* -------- BAL month
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

* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD select_and_publish.

* --------- local data
    DATA lt_bal TYPE znwhd_t_col_bal_grb_stat.
    DATA ls_sum LIKE LINE OF lt_bal.


* --------- select
    SELECT probclass,
           COUNT( * ) AS cnt_all,
           SUM( CAST( msg_cnt_al AS INT4 ) ) AS cnt_msg_all,
           SUM( CAST( msg_cnt_w AS INT4 ) ) AS cnt_msg_warn,
           SUM( CAST( msg_cnt_e AS INT4 ) ) AS cnt_msg_error,
           SUM( CAST( msg_cnt_a AS INT4 ) ) AS cnt_msg_abort
      FROM balhdr
      WHERE aldate = @iv_date AND altime >= @iv_time
         OR aldate > @iv_date
      GROUP BY probclass
      ORDER BY probclass
      INTO TABLE @lt_bal.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.


* ---------- cal sum
    CLEAR ls_sum.
    ls_sum-problclass = '*'.
    LOOP AT lt_bal ASSIGNING FIELD-SYMBOL(<ls_bal>).
      ls_sum-cnt_all        = ls_sum-cnt_all          + <ls_bal>-cnt_all.
      ls_sum-cnt_msg_all    = ls_sum-cnt_msg_all      + <ls_bal>-cnt_msg_all.
      ls_sum-cnt_msg_warn   = ls_sum-cnt_msg_all      + <ls_bal>-cnt_msg_warn.
      ls_sum-cnt_msg_error  = ls_sum-cnt_msg_error    + <ls_bal>-cnt_msg_error.
      ls_sum-cnt_msg_aborts = ls_sum-cnt_msg_aborts   + <ls_bal>-cnt_msg_aborts.
    ENDLOOP.
    APPEND ls_sum TO lt_bal.


* ---------- publish
    LOOP AT lt_bal ASSIGNING <ls_bal>.

      DATA(lv_detail_level) = zif_nwhd_c=>c_detail_level-medium_importance.

      DATA(lv_type) = |Type{ <ls_bal>-problclass }|.
      IF <ls_bal>-problclass IS INITIAL.
        lv_type = |TypeUnknown|.
      ELSEIF <ls_bal>-problclass = '*'.
        lv_type = 'AllType'.
      ELSE.
        lv_detail_level = <ls_bal>-problclass + 1.
        IF is_valid_info(
          iv_is_detail_level     = lv_detail_level
          iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
        ) EQ abap_false.
          CONTINUE.
        ENDIF.
      ENDIF.

      append_number_value(
        EXPORTING
          iv_category            = iv_category
          iv_key                 = |{ lv_type }Count|
          iv_value               = <ls_bal>-cnt_all                  " NWHD Number data type
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
          iv_is_timeint_level    = iv_timeint_level
          iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important
      ).

      append_number_value(
        EXPORTING
          iv_category = iv_category
          iv_key      = |{ lv_type }MsgAbort|
          iv_value    = <ls_bal>-cnt_msg_aborts                  " NWHD Number data type
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
          iv_is_timeint_level    = iv_timeint_level
          iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important
      ).


      append_number_value(
        EXPORTING
          iv_category = iv_category
          iv_key      = |{ lv_type }MsgError|
          iv_value    = <ls_bal>-cnt_msg_error                  " NWHD Number data type
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
          iv_is_timeint_level    = iv_timeint_level
          iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important
      ).


      append_number_value(
        EXPORTING
          iv_category = iv_category
          iv_key      = |{ lv_type }MsgCount|
          iv_value    = <ls_bal>-cnt_msg_all                  " NWHD Number data type
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
          iv_is_timeint_level    = iv_timeint_level
          iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important
      ).

      append_number_value(
        EXPORTING
          iv_category = iv_category
          iv_key      = |{ lv_type }MsgWarning|
          iv_value    = <ls_bal>-cnt_msg_warn                  " NWHD Number data type
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
          iv_is_timeint_level    = iv_timeint_level
          iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance
      ).

      append_number_value(
        EXPORTING
          iv_category = iv_category
          iv_key      = |{ lv_type }MsgError|
          iv_value    = <ls_bal>-cnt_msg_error                  " NWHD Number data type
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
          iv_is_timeint_level    = iv_timeint_level
          iv_is_detail_level     = zif_nwhd_c=>c_detail_level-low_importance
      ).

      append_number_value(
        EXPORTING
          iv_category = iv_category
          iv_key      = |{ lv_type }MsgAbort|
          iv_value    = <ls_bal>-cnt_msg_aborts                  " NWHD Number data type
          iv_is_system_wide_info = abap_true
          iv_is_client_specific  = abap_false
          iv_is_timeint_level    = iv_timeint_level
          iv_is_detail_level     = zif_nwhd_c=>c_detail_level-low_importance
      ).

    ENDLOOP.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_COL~GET_NAME.
    rv_name = zcl_nwhd_col_bal=>c_name.
  ENDMETHOD.
ENDCLASS.
