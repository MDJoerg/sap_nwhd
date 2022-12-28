class ZCL_NWHD_COL_QRFC definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'QRFC' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_QRFC IMPLEMENTATION.


  METHOD collect_data.

* ---------- local data
    DATA: lt_qin_err    TYPE TABLE OF trfcqview.
    DATA: lt_qout_err   TYPE TABLE OF trfcqview.
    DATA: lv_deep       TYPE syindex.


* ---------- system wide
    DATA lv_client TYPE symandt.
    IF ms_col_params-flag_no_system_wide EQ abap_true.
      lv_client = sy-mandt.
    ELSE.
      lv_client = '*'.
    ENDIF.


* ---------- call api inbound
    CALL FUNCTION 'TRFC_QIN_GET_HANGING_QUEUES'
      EXPORTING
        client    = lv_client
      TABLES
        err_queue = lt_qin_err.

    DESCRIBE TABLE lt_qin_err LINES DATA(lv_cnt_qin_err).

    DATA(lv_cnt_qin_tab) = 0.
    LOOP AT lt_qin_err ASSIGNING FIELD-SYMBOL(<ls_qin_err>).
      CALL FUNCTION 'TRFC_GET_QIN_INFO'
        EXPORTING
          qname  = <ls_qin_err>-qname
          client = <ls_qin_err>-mandt
        IMPORTING
          qdeep  = lv_deep.
      lv_cnt_qin_tab = lv_cnt_qin_tab + lv_deep.
    ENDLOOP.



* --------- call api outbound
    CALL FUNCTION 'TRFC_QOUT_GET_HANGING_QUEUES'
      EXPORTING
        client    = lv_client
      TABLES
        err_queue = lt_qout_err.

    DESCRIBE TABLE lt_qout_err LINES DATA(lv_cnt_qout_err).
    DATA(lv_cnt_qout_tab) = 0.

    LOOP AT lt_qout_err ASSIGNING FIELD-SYMBOL(<ls_qout_err>).
      CALL FUNCTION 'TRFC_GET_QUEUE_INFO'
        EXPORTING
          qname  = <ls_qout_err>-qname
          dest   = <ls_qout_err>-dest
          client = <ls_qout_err>-mandt
*         DIST_GET_QUEUE       = ' '
*         QDTABNAME            = ' '
        IMPORTING
          qdeep  = lv_deep.
      lv_cnt_qout_tab = lv_cnt_qout_tab + lv_deep.
    ENDLOOP.


* --------- calc
    DATA(lv_cnt_err_all) = lv_cnt_qin_err + lv_cnt_qout_err.
    DATA(lv_cnt_tab_all) = lv_cnt_qin_tab + lv_cnt_qout_tab.


* --------- publish sum of queues
    append_number_value(
        iv_category = 'All'
        iv_key      = 'Hanging'
        iv_value    = lv_cnt_err_all
    ).

    append_number_value(
        iv_category = 'Inbound'
        iv_key      = 'Hanging'
        iv_value    = lv_cnt_qin_err
    ).

    append_number_value(
        iv_category = 'Outbound'
        iv_key      = 'Hanging'
        iv_value    = lv_cnt_qout_err
    ).

* --------- publish sum of entries
    append_number_value(
        iv_category = 'All'
        iv_key      = 'HangingPkg'
        iv_value    = lv_cnt_tab_all
    ).

    append_number_value(
        iv_category = 'Inbound'
        iv_key      = 'HangingPkg'
        iv_value    = lv_cnt_qin_tab
    ).

    append_number_value(
        iv_category = 'Outbound'
        iv_key      = 'HangingPkg'
        iv_value    = lv_cnt_qout_tab
    ).

* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_COL~GET_NAME.
    rv_name = zcl_nwhd_col_qrfc=>c_name.
  ENDMETHOD.
ENDCLASS.
