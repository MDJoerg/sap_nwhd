class ZCL_NWHD_LDB_BL definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_LDB_BL .
protected section.

  data MS_LAST_SRC type ZNWHD_LDB_S_DB_SRC .
private section.
ENDCLASS.



CLASS ZCL_NWHD_LDB_BL IMPLEMENTATION.


  method ZIF_NWHD_LDB_BL~GET_LAST_SOURCE.
    rs_src = ms_last_src.
  endmethod.


  METHOD zif_nwhd_ldb_bl~is_message_valid.

* ------- check system first
    IF zif_nwhd_ldb_bl~is_source_valid(
         iv_type = iv_type
         iv_id   = iv_id
         iv_auto_create = abap_false
         iv_default_allowed = abap_false
       ) EQ abap_false.
      RETURN.
    ENDIF.


* -------- get last source
    IF ms_last_src IS INITIAL.
      RETURN.
    ENDIF.


* -------- select msg
    SELECT SINGLE *
      FROM ztd_nwhdldb_msg
      INTO CORRESPONDING FIELDS OF es_msg
     WHERE src_guid     = ms_last_src-src_guid
       AND source_ref   = iv_source_ref.
    IF sy-subrc EQ 0.
      RETURN.
    ELSE.
      rv_success = abap_true. " still unknown
    ENDIF.

  ENDMETHOD.


  METHOD zif_nwhd_ldb_bl~is_source_valid.

* ------- check cache
    IF ms_last_src IS INITIAL
      OR ms_last_src-source_type <> iv_type
      OR ms_last_src-source_id   <> iv_id.
      CLEAR ms_last_src.

      SELECT SINGLE *
        FROM ztd_nwhdldb_src
        INTO CORRESPONDING FIELDS OF ms_last_src
       WHERE source_type = iv_type
         AND source_id   = iv_id.
    ENDIF.

* ------- new
    IF ms_last_src IS INITIAL.
      IF iv_auto_create EQ abap_false.
        RETURN.
      ELSE.
        DATA ls_db TYPE ztd_nwhdldb_src.
        ls_db-client      = sy-mandt.
        ls_db-src_guid    = zcl_nwhd_factory=>create_guid( ).
        ls_db-source_type = iv_type.
        ls_db-source_id   = iv_id.

        IF iv_default_allowed EQ abap_true.
          ls_db-inactive = abap_false.
        ELSE.
          ls_db-inactive = abap_true.
        ENDIF.

        GET TIME STAMP FIELD ls_db-created_at.
        INSERT ztd_nwhdldb_src FROM ls_db.
        COMMIT WORK.

        MOVE-CORRESPONDING ls_db TO ms_last_src.

      ENDIF.

    ENDIF.

* ------ success
    rv_success = COND #( WHEN ms_last_src-inactive = abap_true
                         THEN abap_false
                         ELSE abap_true ).

  ENDMETHOD.
ENDCLASS.
