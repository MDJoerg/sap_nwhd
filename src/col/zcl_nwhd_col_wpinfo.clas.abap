class ZCL_NWHD_COL_WPINFO definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'WP' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_WPINFO IMPLEMENTATION.


  METHOD collect_data.


* ---------- local data
    DATA: lt_data TYPE TABLE OF wpinfos.
    DATA: BEGIN OF ls_calc,
            wp_typ     TYPE  wptyp,
            count      TYPE  i,
            used       TYPE  i,
            wp_eltime  TYPE  wpelzeit,
            wp_irestrt TYPE  wpirestart,
            wp_dumps   TYPE  wpdumps,
          END OF ls_calc.
    DATA: ls_sum  LIKE ls_calc.
    DATA: lt_calc LIKE TABLE OF ls_calc.
    FIELD-SYMBOLS: <lfs_calc> LIKE ls_calc.

* --------- local macro
    DEFINE collect_calc.
      <lfs_calc>-&1 = <lfs_calc>-&1 + <lfs_data>-&1.
    END-OF-DEFINITION.
    DEFINE collect_sum.
      ls_sum-&1 = ls_sum-&1 + <lfs_data>-&1.
    END-OF-DEFINITION.



* ---------- call api
    CALL FUNCTION 'TH_SYSTEMWIDE_WPINFO'
      TABLES
        wplist         = lt_data
      EXCEPTIONS
        argument_error = 1
        send_error     = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      rv_success = abap_false.
      RETURN.
    ENDIF.


* ---------- calc
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).

*     get index
      UNASSIGN <lfs_calc>.
      READ TABLE lt_calc ASSIGNING <lfs_calc>
        WITH KEY wp_typ = <lfs_data>-wp_typ.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO lt_calc ASSIGNING <lfs_calc>.
        <lfs_calc>-wp_typ = <lfs_data>-wp_typ.
      ENDIF.

*     collect data
      ADD 1 TO <lfs_calc>-count.
      collect_calc wp_dumps.
      collect_calc wp_eltime.
      collect_calc wp_irestrt.

*    collect sum
      ADD 1 TO ls_sum-count.
      collect_sum wp_dumps.
      collect_sum wp_eltime.
      collect_sum wp_irestrt.

*     collect used
      IF <lfs_data>-wp_istatus NE 2.
        ADD 1 TO <lfs_calc>-used.
        ADD 1 TO ls_sum-used.
      ENDIF.
    ENDLOOP.



* ---------- publish per work process type
    DATA(lv_prefix) = |/WP|.
    DATA(lv_error)  = abap_false.

    LOOP AT lt_calc INTO ls_calc.

      DATA(lv_usage) = 100 * ls_calc-used / ls_calc-count.
      append_number_value(
          iv_category = |Type{ ls_calc-wp_typ }|
          iv_key      = 'Usage'
          iv_value    = lv_usage
      ).

      append_number_value(
          iv_category = |Type{ ls_calc-wp_typ }|
          iv_key      = 'Runtime'
          iv_value    = ls_calc-wp_eltime
      ).

      append_number_value(
          iv_category = |Type{ ls_calc-wp_typ }|
          iv_key      = 'Dumps'
          iv_value    = ls_calc-wp_dumps
      ).

      append_number_value(
          iv_category = |Type{ ls_calc-wp_typ }|
          iv_key      = 'Restarts'
          iv_value    = ls_calc-wp_irestrt
      ).

    ENDLOOP.

* --------- publish sums
    lv_usage = 100 * ls_sum-used / ls_sum-count.

    append_number_value(
        iv_category = |All|
        iv_key      = 'Usage'
        iv_value    = lv_usage
    ).

    append_number_value(
        iv_category = |All|
        iv_key      = 'Runtime'
        iv_value    = ls_sum-wp_eltime
    ).

    append_number_value(
        iv_category = |All|
        iv_key      = 'Dumps'
        iv_value    = ls_sum-wp_dumps
    ).

    append_number_value(
        iv_category = |All|
        iv_key      = 'Restarts'
        iv_value    = ls_sum-wp_dumps
    ).


* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_COL~GET_NAME.
    rv_name = zcl_nwhd_col_wpinfo=>c_name.
  ENDMETHOD.
ENDCLASS.
