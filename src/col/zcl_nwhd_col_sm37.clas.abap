CLASS zcl_nwhd_col_sm37 DEFINITION
  PUBLIC
  INHERITING FROM zcl_nwhd_col
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_nwhd_mod~get_name
        REDEFINITION .

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



CLASS ZCL_NWHD_COL_SM37 IMPLEMENTATION.


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


* ---------- finally true
    rv_success = abap_true.

  ENDMETHOD.


  METHOD select_and_publish.

* --------- local data
    DATA lt_sm37 TYPE TABLE OF v_op.


* --------- select
    SELECT *
      FROM v_op
      WHERE strtdate = @iv_date AND strttime >= @iv_time
         OR strtdate > @iv_date
        AND strtdate <= @sy-datum
      INTO TABLE @lt_sm37.
    IF sy-subrc NE 0.
      rv_success = abap_true.
      RETURN.
    ENDIF.


* ---------- publish
    DATA(lv_lin) = lines( lt_sm37 ).

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
    DATA(lv_lin_abort) = 0.
    DATA(lv_lin_finished) = 0.
    DATA(lv_lin_released) = 0.
    DATA(lv_lin_scheduled) = 0.

    DATA lv_ts_scheduled TYPE timestampl.
    DATA lv_ts_started TYPE timestampl.
    DATA lv_ts_finished TYPE timestampl.

    DATA(lv_delay_all) = 0.
    DATA(lv_runtime_all) = 0.

    DATA(lv_delay_cnt) = 0.
    DATA(lv_runtime_cnt) = 0.

    DATA(lv_delay_max) = 0.
    DATA(lv_runtime_max) = 0.



    LOOP AT lt_sm37 ASSIGNING FIELD-SYMBOL(<ls_sm37>).

* ------ calc main info
      CASE <ls_sm37>-status.
        WHEN 'F'.
          lv_lin_finished = lv_lin_finished + 1.
        WHEN 'A'.
          lv_lin_abort   = lv_lin_abort + 1.
        WHEN 'S'.
          lv_lin_scheduled = lv_lin_scheduled + 1.
        WHEN 'R'.
          lv_lin_released = lv_lin_released + 1.
*        WHEN OTHERS.
      ENDCASE.


* ------ calc delay and runtime
      IF <ls_sm37>-status CA 'FA'.
        CONVERT DATE <ls_sm37>-strtdate
                TIME <ls_sm37>-strttime
                INTO TIME STAMP lv_ts_started
                TIME ZONE sy-zonlo.

        CONVERT DATE <ls_sm37>-sdldate
                TIME <ls_sm37>-sdltime
                INTO TIME STAMP lv_ts_scheduled
                TIME ZONE sy-zonlo.

        CONVERT DATE <ls_sm37>-enddate
                TIME <ls_sm37>-endtime
                INTO TIME STAMP lv_ts_finished
                TIME ZONE sy-zonlo.

        DATA(lv_runtime) = cl_abap_tstmp=>subtract(
            tstmp1 = lv_ts_finished                 " UTC Time Stamp
            tstmp2 = lv_ts_started                 " UTC Time Stamp
        ).

        DATA(lv_delayed) = cl_abap_tstmp=>subtract(
            tstmp1 = lv_ts_started                 " UTC Time Stamp
            tstmp2 = lv_ts_scheduled                 " UTC Time Stamp
        ).


*       calc in seconds
        IF lv_delayed > 0.
          lv_delay_all = lv_delay_all + lv_delayed.
          ADD 1 TO lv_delay_cnt.
          IF lv_delay_max < lv_delayed.
            lv_delay_max = lv_delayed.
          ENDIF.
        ENDIF.

        IF lv_runtime > 0.
          lv_runtime_all = lv_runtime_all + lv_runtime.
          ADD 1 TO lv_runtime_cnt.
          IF lv_runtime_max < lv_runtime.
            lv_runtime_max = lv_runtime.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

* ----------- publish
    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'RunTime'
        iv_value               = lv_runtime_all               " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'RunTimeCount'
        iv_value               = lv_runtime_cnt               " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'RunTimeMax'
        iv_value               = lv_runtime_max               " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).


    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'DelayedTime'
        iv_value               = lv_delay_all                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'DelayedTimeCount'
        iv_value               = lv_delay_cnt                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'DelayedTimeMax'
        iv_value               = lv_delay_max                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Aborted'
        iv_value               = lv_lin_abort                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).


    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Finished'
        iv_value               = lv_lin_finished                 " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).



    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Scheduled'
        iv_value               = lv_lin_scheduled                " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance                " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).


    append_number_value(
      EXPORTING
        iv_category            = iv_category
        iv_key                 = 'Released'
        iv_value               = lv_lin_released               " NWHD Number data type
        iv_is_detail_level     = zif_nwhd_c=>c_detail_level-low_importance              " NWHD: detail level for collectors
        iv_is_timeint_level    = iv_timeint_level                " NWHD: detail level for time interval
        iv_is_system_wide_info = abap_true
        iv_is_client_specific  = abap_false
    ).

* ----------- finally success
    rv_success = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_mod~get_name.
    rv_name = 'SM37'.
  ENDMETHOD.
ENDCLASS.
