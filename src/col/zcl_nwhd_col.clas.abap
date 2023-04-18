CLASS zcl_nwhd_col DEFINITION
  PUBLIC
  INHERITING FROM zcl_nwhd_mod
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_nwhd_col .
protected section.

  data MS_DATA type ZNWHD_S_DATA_COL .
  data MS_COL_PARAMS type ZNWHD_S_PARAM_JOB_COL .

  methods APPEND_NUMBER_VALUE
    importing
      !IV_CATEGORY type DATA optional
      !IV_KEY type DATA
      !IV_VALUE type DATA
      !IV_IS_DETAIL_LEVEL type ZNWHD_COL_DETAIL_LEVEL default 0
      !IV_IS_TIMEINT_LEVEL type ZNWHD_COL_TIMEINT_LEVEL default 0
      !IV_IS_SYSTEM_WIDE_INFO type ABAP_BOOL default ABAP_FALSE
      !IV_IS_CLIENT_SPECIFIC type ABAP_BOOL default ABAP_FALSE .
  methods APPEND_TEXT_VALUE
    importing
      !IV_CATEGORY type STRING optional
      !IV_KEY type STRING
      !IV_VALUE type DATA
      !IV_IS_DETAIL_LEVEL type ZNWHD_COL_DETAIL_LEVEL default 0
      !IV_IS_TIMEINT_LEVEL type ZNWHD_COL_TIMEINT_LEVEL default 0
      !IV_IS_SYSTEM_WIDE_INFO type ABAP_BOOL default ABAP_FALSE
      !IV_IS_CLIENT_SPECIFIC type ABAP_BOOL default ABAP_FALSE .
  methods CLOSE_DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods COLLECT_DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods GET_DATETIME_LAST_24H
    importing
      !IV_DATE type SYDATUM default SY-DATUM
      !IV_TIME type SYUZEIT default SY-UZEIT
    exporting
      !EV_TIME type SYUZEIT
    returning
      value(RV_DATE) type SY-DATUM .
  methods GET_DATETIME_LAST_HOUR
    importing
      !IV_DATE type SYDATUM default SY-DATUM
      !IV_TIME type SYUZEIT default SY-UZEIT
    exporting
      !EV_TIME type SYUZEIT
    returning
      value(RV_DATE) type SY-DATUM .
  methods GET_DATETIME_LAST_MONTH
    importing
      !IV_DATE type SYDATUM default SY-DATUM
      !IV_TIME type SYUZEIT default SY-UZEIT
    exporting
      !EV_TIME type SYUZEIT
    returning
      value(RV_DATE) type SY-DATUM .
  methods GET_DATETIME_LAST_WEEK
    importing
      !IV_DATE type SYDATUM default SY-DATUM
      !IV_TIME type SYUZEIT default SY-UZEIT
    exporting
      !EV_TIME type SYUZEIT
    returning
      value(RV_DATE) type SY-DATUM .
  methods GET_DATETIME_LAST_YEAR
    importing
      !IV_DATE type SYDATUM default SY-DATUM
      !IV_TIME type SYUZEIT default SY-UZEIT
    exporting
      !EV_TIME type SYUZEIT
    returning
      value(RV_DATE) type SY-DATUM .
  methods INIT_DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods IS_VALID_INFO
    importing
      !IV_IS_DETAIL_LEVEL type ZNWHD_COL_DETAIL_LEVEL
      !IV_IS_SYSTEM_WIDE_INFO type ABAP_BOOL
      !IV_IS_CLIENT_SPECIFIC type ABAP_BOOL
      !IV_IS_TIMEINT_LEVEL type ZNWHD_COL_TIMEINT_LEVEL
    returning
      value(RV_VALID) type ABAP_BOOL .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_NWHD_COL IMPLEMENTATION.


  METHOD append_text_value.
* ------ check filter
    IF is_valid_info(
           iv_is_detail_level     = iv_is_detail_level
           iv_is_timeint_level    = iv_is_timeint_level
           iv_is_system_wide_info = iv_is_system_wide_info
           iv_is_client_specific  = iv_is_client_specific
    ) EQ abap_false.
      RETURN.
    ENDIF.

* ------- append text value
    DATA(lv_value) = CONV znwhd_value_text( iv_value ).
    APPEND INITIAL LINE TO ms_data-fields ASSIGNING FIELD-SYMBOL(<ls_new>).
    <ls_new>-category     = iv_category.
    <ls_new>-key          = iv_key.
    <ls_new>-value_number = lv_value.
  ENDMETHOD.


  METHOD get_datetime_last_24h.
    ev_time = iv_time.
    rv_date = iv_date - 1.
  ENDMETHOD.


  METHOD append_number_value.

* ------ check filter
    IF is_valid_info(
           iv_is_detail_level     = iv_is_detail_level
           iv_is_timeint_level    = iv_is_timeint_level
           iv_is_system_wide_info = iv_is_system_wide_info
           iv_is_client_specific  = iv_is_client_specific
    ) EQ abap_false.
      RETURN.
    ENDIF.

* ------- append num value
    DATA(lv_value) = CONV znwhd_value_number( iv_value ).
    APPEND INITIAL LINE TO ms_data-fields ASSIGNING FIELD-SYMBOL(<ls_new>).
    <ls_new>-category     = iv_category.
    <ls_new>-key          = iv_key.
    <ls_new>-value_number = lv_value.
  ENDMETHOD.


  METHOD close_data.
    IF ms_data IS INITIAL.
      RETURN.
    ELSE.
      GET TIME STAMP FIELD ms_data-finished_at.
      rv_success = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD collect_data.
    rv_success = abap_false.
  ENDMETHOD.


  METHOD init_data.
    CLEAR ms_data.
    ms_data-collector = zif_nwhd_col~get_name( ).
    GET TIME STAMP FIELD ms_data-started_at.
    rv_success = abap_true.
  ENDMETHOD.


  METHOD get_datetime_last_week.
    ev_time = iv_time.
    rv_date = iv_date - 7.
  ENDMETHOD.


  METHOD get_datetime_last_year.
    DATA lv_year(4) TYPE n.
    lv_year = iv_date(4).
    lv_year = lv_year - 1.

    rv_date = iv_date.
    rv_date(4) = lv_year.
    ev_time = iv_time.

  ENDMETHOD.


  METHOD get_datetime_last_hour.
    IF iv_time < '010000'.
      DATA lv_delta TYPE syuzeit.
      lv_delta = '010000'.
      lv_delta = lv_delta - iv_time.
      ev_time  = '240000'.
      ev_time  = ev_time - lv_delta.
*      ev_time = iv_time - 36000.
      rv_date = iv_date - 1.
    ELSE.
      ev_time = iv_time - 3600.
      rv_date = iv_date.
    ENDIF.
  ENDMETHOD.


  METHOD get_datetime_last_month.
    ev_time = iv_time.
    rv_date = iv_date - 30.
  ENDMETHOD.


  METHOD is_valid_info.

* ----- check given col params
    IF ms_col_params IS INITIAL.
      rv_valid = abap_true.
    ENDIF.

* ----- check system wide
    IF iv_is_system_wide_info EQ abap_true
      AND ms_col_params-flag_no_system_wide EQ abap_true.
      RETURN.
    ENDIF.

* ----- check client specific
    IF iv_is_client_specific EQ abap_true
      AND ms_col_params-flag_no_client_specific EQ abap_true.
      RETURN.
    ENDIF.

* ----- check detail level
    IF iv_is_detail_level > 0
      AND ms_col_params-detail_level > 0
      AND iv_is_detail_level > ms_col_params-detail_level.
      RETURN.
    ENDIF.

* ----- check timeint level
    IF iv_is_timeint_level > 0
      AND ms_col_params-timeint_level > 0
      AND iv_is_timeint_level > ms_col_params-timeint_level.
      RETURN.
    ENDIF.

* ------ otherwise
    rv_valid = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_col~collect.

* ------ init
    ms_col_params = is_col_params.
    IF init_data( ) EQ abap_false.
      RETURN.
    ENDIF.

* ------ collect
    IF collect_data( ) EQ abap_false.
      RETURN.
    ENDIF.

* ------ close
    IF close_data( ) EQ abap_false.
      RETURN.
    ENDIF.

* ------ success
    rv_success = abap_true.
    es_data    = ms_data.

  ENDMETHOD.
ENDCLASS.
