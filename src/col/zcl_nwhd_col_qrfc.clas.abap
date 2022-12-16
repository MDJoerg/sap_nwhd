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

* ---------- call api
    CALL FUNCTION 'TRFC_QIN_GET_HANGING_QUEUES'
      TABLES
        err_queue = lt_qin_err.
    DESCRIBE TABLE lt_qin_err LINES DATA(lv_cnt_qin_err).

    CALL FUNCTION 'TRFC_QOUT_GET_HANGING_QUEUES'
      TABLES
        err_queue = lt_qout_err.
    DESCRIBE TABLE lt_qout_err LINES DATA(lv_cnt_qout_err).

* --------- calc
    DATA(lv_cnt_all) = lv_cnt_qin_err + lv_cnt_qout_err.


* --------- publish sums
    append_number_value(
        iv_category = 'All'
        iv_key      = 'Hanging'
        iv_value    = lv_cnt_all
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


* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_COL~GET_NAME.
    rv_name = zcl_nwhd_col_qrfc=>c_name.
  ENDMETHOD.
ENDCLASS.
