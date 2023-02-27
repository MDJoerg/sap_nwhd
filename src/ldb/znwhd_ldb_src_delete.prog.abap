*&---------------------------------------------------------------------*
*& Report ZNWHD_LDB_SRC_DELETE
*&---------------------------------------------------------------------*
*& Delete a source system from LDB
*&---------------------------------------------------------------------*
REPORT znwhd_ldb_src_delete NO STANDARD PAGE HEADING.

TABLES: ztd_nwhdldb_msg.
PARAMETERS: p_src LIKE ztd_nwhdldb_msg-src_guid OBLIGATORY.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.


INITIALIZATION.
  DATA(lr_util) = zcl_nwhd_factory=>create_util( ).
  DATA(lr_ldb)  = zcl_nwhd_factory=>create_ldb_bl( ).

START-OF-SELECTION.

* ------ get count of relevant records
  DATA(lv_lin) = lr_ldb->source_delete(
      iv_src_guid = p_src                 " Source GUID
      iv_commit   = abap_true
      iv_test     = p_test
  ).

end-of-SELECTION.

  WRITE: / 'Records found for deletion:', lv_lin.
  IF lv_lin > 0.
    IF p_test = abap_true.
      WRITE: / 'Test mode active.'.
    ELSE.
      WRITE: / 'Records deleted.'.
    ENDIF.
  ENDIF.
