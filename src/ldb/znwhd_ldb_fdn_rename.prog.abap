*&---------------------------------------------------------------------*
*& Report ZNWHD_LDB_FDN_RENAME
*&---------------------------------------------------------------------*
*& Rename Numeric Value Names after changes
*&---------------------------------------------------------------------*
REPORT znwhd_ldb_fdn_rename NO STANDARD PAGE HEADING.

TABLES: ztd_nwhdldb_fdn.
PARAMETERS: p_src   LIKE ztd_nwhdldb_msg-src_guid OBLIGATORY.
SELECTION-SCREEN: ULINE.
SELECT-OPTIONS: so_src FOR ztd_nwhdldb_fdn-src_guid.
SELECT-OPTIONS: so_cat FOR ztd_nwhdldb_fdn-created_at.
SELECTION-SCREEN: ULINE.
PARAMETERS:     p_ocol LIKE ztd_nwhdldb_fdn-collector.
PARAMETERS:     p_ocat LIKE ztd_nwhdldb_fdn-category.
PARAMETERS:     p_ofld LIKE ztd_nwhdldb_fdn-field.
SELECTION-SCREEN: ULINE.
PARAMETERS:     p_ncol LIKE ztd_nwhdldb_fdn-collector.
PARAMETERS:     p_ncat LIKE ztd_nwhdldb_fdn-category.
PARAMETERS:     p_nfld LIKE ztd_nwhdldb_fdn-field.
SELECTION-SCREEN: ULINE.
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


* ---------- select
  SELECT *
    FROM ztd_nwhdldb_fdn
    INTO TABLE @DATA(lt_fdn)
   WHERE src_guid    IN @so_src
     AND created_at  IN @so_cat
     AND collector   EQ @p_ocol
     AND category    EQ @p_ocat
     AND field       EQ @p_ofld.

* ---------- check
  DATA(lv_cnt) = lines( lt_fdn ).
  IF lv_cnt = 0.
    WRITE: / 'Nothing found'.
  ELSE.
* --------- check existing
    WRITE: / 'Records found: ', lv_cnt.

* -------- prepare update
    DATA(lv_modi) = 0.
    LOOP AT lt_fdn ASSIGNING FIELD-SYMBOL(<ls_fdn>).
      IF p_ncol IS NOT INITIAL.
        <ls_fdn>-collector = p_ncol.
        lv_modi = lv_modi + 1.
      ENDIF.

      IF p_ncat IS NOT INITIAL.
        <ls_fdn>-category = p_ncat.
        lv_modi = lv_modi + 1.
      ENDIF.

      IF p_nfld IS NOT INITIAL.
        <ls_fdn>-field = p_nfld.
        lv_modi = lv_modi + 1.
      ENDIF.
    ENDLOOP.

    WRITE: / 'Modified cells:', lv_modi.

* --------- test mode
    IF p_test = abap_true.
      WRITE: / 'Testmode - no database updates'.
    ELSE.
* --------- database update
      MODIFY ztd_nwhdldb_fdn FROM TABLE lt_fdn.
      COMMIT WORK AND WAIT.
      WRITE: / 'Database updated'.
    ENDIF.
  ENDIF.

end-of-SELECTION.
