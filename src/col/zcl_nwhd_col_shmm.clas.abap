class ZCL_NWHD_COL_SHMM definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  methods ZIF_NWHD_MOD~GET_NAME
    redefinition .
  PROTECTED SECTION.

    METHODS collect_data
        REDEFINITION .

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_NWHD_COL_SHMM IMPLEMENTATION.


  METHOD collect_data.

* -------- local data
    DATA ls_shm_info  TYPE shm_info.
    DATA lv_used      TYPE znwhd_value_number.
    DATA lx_exc       TYPE REF TO cx_shm_error.


* ------- call kernel param
    TRY.

*        call shmm monitor
         PERFORM prepare_shm_info IN PROGRAM sapmshm_monitor IF FOUND.
         if sy-subrc <> 0.
            return.
         else.
            ASSIGN ('(SAPMSHM_MONITOR)SHM_INFO') TO FIELD-SYMBOL(<ls_shm_info>).
            if <ls_shm_info> is NOT ASSIGNED.
              RETURN.
            else.
              MOVE-CORRESPONDING <ls_shm_info> to ls_shm_info.
            ENDIF.
        endif.

        IF ls_shm_info IS NOT INITIAL.
          append_number_value(
            EXPORTING
              iv_category            = 'SharedMemory'
              iv_key                 = 'TotalBytes'
              iv_value               = ls_shm_info-size                 " NWHD Number data type
              iv_is_detail_level     = zif_nwhd_c=>c_detail_level-medium_importance                " NWHD: detail level for collectors
              iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified                " NWHD: detail level for time interval
              iv_is_system_wide_info = abap_true
              iv_is_client_specific  = abap_false
          ).

          append_number_value(
            EXPORTING
              iv_category            = 'SharedMemory'
              iv_key                 = 'FreeBytes'
              iv_value               = ls_shm_info-free                 " NWHD Number data type
              iv_is_detail_level     = zif_nwhd_c=>c_detail_level-important                " NWHD: detail level for collectors
              iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified               " NWHD: detail level for time interval
              iv_is_system_wide_info = abap_true
              iv_is_client_specific  = abap_false
          ).


          lv_used = 100 * ( ls_shm_info-fill / ls_shm_info-size ).
          append_number_value(
            EXPORTING
              iv_category            = 'SharedMemory'
              iv_key                 = 'UsedPercent'
              iv_value               = lv_used                 " NWHD Number data type
              iv_is_detail_level     = zif_nwhd_c=>c_detail_level-very_important                " NWHD: detail level for collectors
              iv_is_timeint_level    = zif_nwhd_c=>c_timeint_level-unspecified              " NWHD: detail level for time interval
              iv_is_system_wide_info = abap_true
              iv_is_client_specific  = abap_false
          ).
        ENDIF.

        rv_success = abap_true.
      CATCH cx_shm_error INTO lx_exc.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_nwhd_mod~get_name.
    rv_name = 'SHMM'.
  ENDMETHOD.
ENDCLASS.
