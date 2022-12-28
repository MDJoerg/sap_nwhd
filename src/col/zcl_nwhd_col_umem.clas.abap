class ZCL_NWHD_COL_UMEM definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'UserMemory' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_UMEM IMPLEMENTATION.


  METHOD collect_data.

* --------- check client specific data unwanted
    IF ms_col_params-flag_no_client_specific EQ abap_true.
      rv_success = abap_true.
      RETURN.
    ENDIF.


* ================ User Memory
    TRY.
        DATA(lt_sess) =  cl_system_info=>get_session_list(
          EXPORTING
            with_application_info = 0                " Boolean Value
            tenant                = sy-mandt               " Name of Tenant
            only_active_server    = 0
        ).

        DATA ls_sess LIKE LINE OF lt_sess.
        DATA lv_mem_sum LIKE ls_sess-memory.
        DATA lv_mem_ses TYPE i.
        DATA lv_mem_max LIKE ls_sess-memory.
        DATA lv_mem_avg LIKE ls_sess-memory.


        lv_mem_ses = lines( lt_sess ).
        LOOP AT lt_sess ASSIGNING FIELD-SYMBOL(<ls_sess>).
          ADD <ls_sess>-memory TO lv_mem_sum.
          IF lv_mem_max < <ls_sess>-memory.
            lv_mem_max = <ls_sess>-memory.
          ENDIF.
        ENDLOOP.

        IF lv_mem_ses > 0.
          lv_mem_avg = lv_mem_sum / lv_mem_ses.
        ENDIF.


      CATCH cx_root INTO DATA(lx_exc). " No authorization for this action
        RETURN.
    ENDTRY.

* --------- publish sums
    append_number_value(
        iv_category = 'Session'
        iv_key      = 'Count'
        iv_value    = lv_mem_ses
    ).

    append_number_value(
        iv_category = 'SessionMemory'
        iv_key      = 'Sum'
        iv_value    = lv_mem_sum
    ).

    append_number_value(
        iv_category = 'SessionMemory'
        iv_key      = 'Avg'
        iv_value    = lv_mem_avg
    ).


    append_number_value(
        iv_category = 'SessionMemory'
        iv_key      = 'Max'
        iv_value    = lv_mem_max
    ).

* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_COL~GET_NAME.
    rv_name = zcl_nwhd_col_umem=>c_name.
  ENDMETHOD.
ENDCLASS.
