FUNCTION z_nwhd_rec_inb_ldb.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_RESULT) TYPE  ZNWHD_S_DATA_JOB
*"     VALUE(IS_PARAMS) TYPE  ZNWHD_S_PARAM_JOB OPTIONAL
*"  EXCEPTIONS
*"      WRONG_DATA
*"      WRONG_INSTALLATION
*"----------------------------------------------------------------------


* -------- check
  IF is_result IS INITIAL
    OR is_result-source_id IS INITIAL.
    RAISE wrong_data.
  ENDIF.

* -------- get ldb publisher
  DATA(lr_ldb) = zcl_nwhd_factory=>create_publisher( 'ZCL_NWHD_LDB_PUB' ).
  IF lr_ldb IS INITIAL.
    RAISE wrong_installation.
  ENDIF.

* -------- publish now
  DATA(ls_result) = is_result.
  DATA(ls_params) = is_params.

  IF lr_ldb->publish(
    EXPORTING
      is_params  = ls_params                 " NWHD: Job Parameter
      is_result  = ls_result                 " NWHD: Job Data
  ) EQ abap_false.
    DATA(lr_qrfc) = zcl_nwhd_factory=>create_qrfc_util( ).
    lr_qrfc->restart_package( ).
  ENDIF.

ENDFUNCTION.
