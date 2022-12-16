class ZCL_NWHD_COL_DUMPS definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'Dumps' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_DUMPS IMPLEMENTATION.


  METHOD collect_data.

* -------- select
    GET TIME.
    DATA(lv_60m_before) = sy-uzeit - 3600.
    DATA(lv_lastweek)   = sy-datum - 7.

    SELECT COUNT(*)
      FROM snap
      INTO @DATA(lv_today)
     WHERE datum EQ @sy-datum
       AND seqno EQ '000'.

    append_number_value(
        iv_category = 'Today'
        iv_key      = 'All'
        iv_value    = lv_today
    ).

    SELECT COUNT(*)
      FROM snap
      INTO @DATA(lv_week)
     WHERE datum GE @lv_lastweek
       AND seqno EQ '000'.

    append_number_value(
        iv_category = 'Week'
        iv_key      = 'All'
        iv_value    = lv_week
    ).


    IF lv_60m_before > 0.
      SELECT COUNT(*)
        FROM snap
        INTO @DATA(lv_last60m)
       WHERE datum EQ @sy-datum
         AND uzeit GE @lv_60m_before
         AND seqno EQ '000'.


      append_number_value(
          iv_category = 'Today'
          iv_key      = 'Last60Min'
          iv_value    = lv_last60m
      ).

    ENDIF.



* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_col~get_name.
    rv_name = zcl_nwhd_col_dumps=>c_name.
  ENDMETHOD.
ENDCLASS.
