class ZCL_NWHD_COL_DUMPS definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'Dumps' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods SELECT_AND_PUBLISH
    importing
      !IV_DATE type SYDATUM
      !IV_TIME type SYUZEIT
      !IV_CATEGORY type DATA .

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_DUMPS IMPLEMENTATION.


  METHOD collect_data.

* -------- init
    GET TIME.
    DATA(lv_from_date) = sy-datum.
    DATA(lv_from_time) = sy-uzeit.


* ------------- 1h
    IF ms_col_params-detail_level IS INITIAL
       OR ms_col_params-detail_level >= zif_nwhd_c=>c_detail_level-last_hour.

      get_datetime_last_hour(
        IMPORTING
          ev_time = lv_from_time                  " System Time
        RECEIVING
          rv_date = lv_from_date                 " ABAP system field: Current date of application server
      ).

      select_and_publish(
          iv_date     = lv_from_date                 " System Date
          iv_time     = lv_from_time                 " System Time
          iv_category = zif_nwhd_c=>c_category-last_hour
      ).
    ENDIF.

* ------------- 24h
    IF ms_col_params-detail_level IS INITIAL
       OR ms_col_params-detail_level >= zif_nwhd_c=>c_detail_level-last_24h.

      get_datetime_last_24h(
        IMPORTING
          ev_time = lv_from_time                  " System Time
        RECEIVING
          rv_date = lv_from_date                 " ABAP system field: Current date of application server
      ).

      select_and_publish(
          iv_date     = lv_from_date                 " System Date
          iv_time     = lv_from_time                 " System Time
          iv_category = zif_nwhd_c=>c_category-last_24h
      ).
    ENDIF.


* ------------- week
    IF ms_col_params-detail_level IS INITIAL
       OR ms_col_params-detail_level >= zif_nwhd_c=>c_detail_level-last_week.

      get_datetime_last_week(
        IMPORTING
          ev_time = lv_from_time                  " System Time
        RECEIVING
          rv_date = lv_from_date                 " ABAP system field: Current date of application server
      ).

      select_and_publish(
          iv_date     = lv_from_date                 " System Date
          iv_time     = lv_from_time                 " System Time
          iv_category = zif_nwhd_c=>c_category-last_week
      ).
    ENDIF.

* ------------- month
    IF ms_col_params-detail_level IS INITIAL
       OR ms_col_params-detail_level >= zif_nwhd_c=>c_detail_level-last_month.

      get_datetime_last_month(
        IMPORTING
          ev_time = lv_from_time                  " System Time
        RECEIVING
          rv_date = lv_from_date                 " ABAP system field: Current date of application server
      ).

      select_and_publish(
          iv_date     = lv_from_date                 " System Date
          iv_time     = lv_from_time                 " System Time
          iv_category = zif_nwhd_c=>c_category-last_month
      ).
    ENDIF.


* ------------- year
    IF ms_col_params-detail_level IS INITIAL
       OR ms_col_params-detail_level >= zif_nwhd_c=>c_detail_level-last_year.

      get_datetime_last_year(
        IMPORTING
          ev_time = lv_from_time                  " System Time
        RECEIVING
          rv_date = lv_from_date                 " ABAP system field: Current date of application server
      ).

      select_and_publish(
          iv_date     = lv_from_date                 " System Date
          iv_time     = lv_from_time                 " System Time
          iv_category = zif_nwhd_c=>c_category-last_year
      ).
    ENDIF.


* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_col~get_name.
    rv_name = zcl_nwhd_col_dumps=>c_name.
  ENDMETHOD.


  METHOD select_and_publish.

    SELECT COUNT(*)
      FROM snap
      INTO @DATA(lv_count)
     WHERE seqno EQ '000'
       AND ( datum = @iv_date AND  uzeit >= @iv_time OR datum > @iv_date ).

    append_number_value(
        iv_category = iv_category
        iv_key      = 'Count'
        iv_value    = lv_count
    ).
  ENDMETHOD.
ENDCLASS.
