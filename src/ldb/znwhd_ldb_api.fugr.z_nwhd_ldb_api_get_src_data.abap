FUNCTION z_nwhd_ldb_api_get_src_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SRC_GUID) TYPE  ZNWHD_GUID_SRC
*"     VALUE(IV_STARTED_AT) TYPE  ZNWHD_TIMESTAMPL_STARTED_AT
*"     VALUE(IV_FINISHED_AT) TYPE  ZNWHD_TIMESTAMPL_FINISHED_AT
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  ZNWHD_LDB_S_CTX_DB
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA(lr_ldb) = zcl_nwhd_factory=>create_ldb_bl( ).
  es_data = lr_ldb->source_export(
              iv_src_guid    = iv_src_guid
              iv_started_at  = iv_started_at
              iv_finished_at = iv_finished_at
            ).
  et_return = lr_ldb->get_logger( )->get_msg_tab( ).

ENDFUNCTION.
