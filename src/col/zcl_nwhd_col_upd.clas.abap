class ZCL_NWHD_COL_UPD definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'UpdProc' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_UPD IMPLEMENTATION.


  METHOD collect_data.

* -------- select and publish db count
    SELECT  vbcliinfo,
            COUNT( * ) AS rec_count
      FROM vbhdr
      INTO TABLE @DATA(lt_vb)
     GROUP BY vbcliinfo.

* --------- calc
    DATA ls_sum LIKE LINE OF lt_vb.
    LOOP AT lt_vb ASSIGNING FIELD-SYMBOL(<ls_vb>).
      ls_sum-rec_count = ls_sum-rec_count + <ls_vb>-rec_count.
    ENDLOOP.
    APPEND ls_sum TO lt_vb.


* --------- publish
    LOOP AT lt_vb ASSIGNING <ls_vb>.
      DATA(lv_type) = |State{ <ls_vb>-vbcliinfo }|.
      IF <ls_vb>-vbcliinfo IS INITIAL.
        lv_type = 'AllState'.
      ENDIF.

      append_number_value(
        EXPORTING
          iv_category = 'Tasks'
          iv_key      = |{ lv_type }Count|
          iv_value    = <ls_vb>-rec_count                 " NWHD Number data type
      ).
    ENDLOOP.


* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_COL~GET_NAME.
    rv_name = zcl_nwhd_col_upd=>c_name.
  ENDMETHOD.
ENDCLASS.
