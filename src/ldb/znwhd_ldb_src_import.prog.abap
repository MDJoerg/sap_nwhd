*&---------------------------------------------------------------------*
*& Report ZNWHD_LDB_SRC_DELETE
*&---------------------------------------------------------------------*
*& Import source data from remote system to local database
*&---------------------------------------------------------------------*
REPORT znwhd_ldb_src_import NO STANDARD PAGE HEADING.

PARAMETERS: p_rfc   TYPE rfcdest OBLIGATORY.
PARAMETERS: p_src   LIKE ztd_nwhdldb_msg-src_guid OBLIGATORY.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_dtfr  TYPE date_from.
PARAMETERS: p_dtto  TYPE date_to.


SELECTION-SCREEN: ULINE.
PARAMETERS: p_chck AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.



INITIALIZATION.
  DATA(lr_util) = zcl_nwhd_factory=>create_util( ).
  DATA(lr_ldb)  = zcl_nwhd_factory=>create_ldb_bl( ).

START-OF-SELECTION.

* --------- local data
  DATA lv_ts_from TYPE timestampl.
  DATA lv_ts_to   TYPE timestampl.
  DATA ls_data    TYPE znwhd_ldb_s_ctx_db.
  DATA lt_return  TYPE bapiret2_tab.
  DATA lv_error   TYPE bapi_msg.


* --------- check existing
  IF p_chck = abap_true.
    DATA(lv_lin) = lr_ldb->source_delete(
       iv_src_guid = p_src
       iv_commit   = abap_false
       iv_test     = abap_true
     ).
    IF lv_lin > 0.
      WRITE: / 'Local data for Source system exists:', lv_lin, 'records'.
    ENDIF.
  ENDIF.


* -------- prepare timestamps
  IF p_dtfr IS NOT INITIAL.
    CONVERT DATE p_dtfr
            TIME '000000'
       INTO TIME STAMP lv_ts_from
       TIME ZONE sy-zonlo.
  ENDIF.

  IF p_dtto IS NOT INITIAL.
    CONVERT DATE p_dtto
            TIME '000000'
       INTO TIME STAMP lv_ts_to
       TIME ZONE sy-zonlo.
  ENDIF.

* -------- get remote data
  lv_error = 'unknown'.
  CALL FUNCTION 'Z_NWHD_LDB_API_GET_SRC_DATA'
    DESTINATION p_rfc
    EXPORTING
      iv_src_guid           = p_src
      iv_started_at         = lv_ts_from
      iv_finished_at        = lv_ts_to
    IMPORTING
      es_data               = ls_data
      et_return             = lt_return
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_error
      system_failure        = 2 MESSAGE lv_error
      OTHERS                = 3.
  IF sy-subrc <> 0.
    WRITE: / 'Errors occured:', lv_error.
  ELSE.
    IF ls_data IS INITIAL
      OR ls_data-msg_tab[] IS INITIAL.
      WRITE: / 'No data selected in remote system'.
    ELSE.
      IF lr_ldb->source_import(
           is_data   = ls_data
           iv_commit = abap_false
         ) EQ abap_true.
        WRITE: / 'Data imported to local database'.
      ELSE.
        WRITE: / 'Error while saving data to local database'.
      ENDIF.
    ENDIF.
  ENDIF.


end-of-SELECTION.
