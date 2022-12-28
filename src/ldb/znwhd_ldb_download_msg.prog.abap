*&---------------------------------------------------------------------*
*& Report ZNWHD_LDB_DOWNLOAD_MSG
*&---------------------------------------------------------------------*
*& Download LDB data as result in json format
*&---------------------------------------------------------------------*
REPORT znwhd_ldb_download_msg NO STANDARD PAGE HEADING.

TABLES: ztd_nwhdldb_msg.
SELECT-OPTIONS: so_src FOR ztd_nwhdldb_msg-src_guid OBLIGATORY NO INTERVALS.
SELECT-OPTIONS: so_msg FOR ztd_nwhdldb_msg-msg_guid NO INTERVALS.
SELECT-OPTIONS: so_cre FOR ztd_nwhdldb_msg-created_at.

INITIALIZATION.
  DATA(lr_ldb)  = zcl_nwhd_factory=>create_ldb_bl( ).
  DATA(lr_util) = zcl_nwhd_factory=>create_util( ).

START-OF-SELECTION.

* -------- select into database
  SELECT *
    FROM ztd_nwhdldb_msg
   WHERE msg_guid     IN @so_msg
     AND src_guid     IN @so_src
     AND created_at   IN @so_cre
    INTO TABLE @DATA(lt_msg).

  DATA(lv_lin) = lines( lt_msg ).
  CHECK lv_lin > 0.


* -------- transform database layout to job export
  DATA(lt_result) = VALUE znwhd_t_data_job( ).

  LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
    WRITE: / <ls_msg>-msg_guid.
    IF lr_ldb->read_msg_context( <ls_msg>-msg_guid ) EQ abap_true.
      DATA(ls_result) = lr_ldb->get_context_as_result( ).
      IF ls_result IS INITIAL.
        WRITE: 'No result' COLOR 6.
      ELSE.
        APPEND ls_result TO lt_result.
        WRITE: 'Msg Context loaded'.
      ENDIF.
    ELSE.
      WRITE: 'Errors occured' COLOR 6.
    ENDIF.
  ENDLOOP.


end-of-SELECTION.

* --------- check result and save
  CHECK lt_result[] IS NOT INITIAL.
  DATA(lv_json) = lr_util->to_json( is_data = lt_result ).
  DATA(lv_filename) = ||.
  IF lr_util->gui_download_string(
    EXPORTING
      iv_data     = lv_json
      iv_title    = 'Download NWHD Results from LDB'
      iv_filename = |{ sy-datum }_{ sy-uzeit }_nwhd_results.json|
      iv_ext      = 'json'
    IMPORTING
*      ev_filename =
*      ev_path     =
      ev_fullpath = lv_filename
  ) EQ abap_true AND lv_filename IS NOT INITIAL.
    WRITE: / 'Downloaded to:', lv_filename.
  ELSE.
    WRITE: / 'Not downloaded.'.
  ENDIF.
