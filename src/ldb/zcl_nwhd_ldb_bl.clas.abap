class ZCL_NWHD_LDB_BL definition
  public
  inheriting from ZCL_NWHD_API
  create public .

public section.

  interfaces ZIF_NWHD_LDB_BL .

  aliases GET_LOGGER
    for ZIF_NWHD_API~GET_LOGGER .
  aliases SET_LOGGER
    for ZIF_NWHD_API~SET_LOGGER .
protected section.

  data MS_LAST_SRC type ZNWHD_LDB_S_DB_SRC .
  data MS_CTX_DB type ZNWHD_LDB_S_CTX_DB .
  data MS_RESULT type ZNWHD_S_DATA_JOB .

  methods PREPARE_FIELDS
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods PREPARE_FIELD_NUMERIC
    importing
      !IS_COL type ZNWHD_S_DATA_COL
      !IS_FLD type ZNWHD_S_DATA_FIELD
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods PREPARE_FIELD_TEXT
    importing
      !IS_COL type ZNWHD_S_DATA_COL
      !IS_FLD type ZNWHD_S_DATA_FIELD
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods PREPARE_SOURCE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods PREPARE_TAGS
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
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


  METHOD PREPARE_FIELDS.

* -------- check contect
    IF ms_ctx_db-src-src_guid IS INITIAL.
      get_logger( )->error( |wrong fields context| ).
      RETURN.
    ENDIF.

    IF ms_result-collected[] IS INITIAL.
      get_logger( )->warning( |no fields fields to process| ).
      rv_success = abap_true.
      RETURN.
    ENDIF.


* -------- select current active fields
    SELECT *
      FROM ztd_nwhdldb_fdn
      INTO CORRESPONDING FIELDS OF TABLE ms_ctx_db-fdn_tab
     WHERE src_guid = ms_ctx_db-src-src_guid
       AND active   = abap_true.
    DATA(lv_lin_fdn) = lines( ms_ctx_db-fdn_tab ).

    SELECT *
      FROM ztd_nwhdldb_fdt
      INTO CORRESPONDING FIELDS OF TABLE ms_ctx_db-fdt_tab
     WHERE src_guid = ms_ctx_db-src-src_guid
       AND active   = abap_true.
    DATA(lv_lin_fdt) = lines( ms_ctx_db-fdt_tab ).

    get_logger( )->trace( |selected { lv_lin_fdn } existing numeric and { lv_lin_fdt } text field values| ).


* ------- make all existing invalid
    LOOP AT ms_ctx_db-fdt_tab ASSIGNING FIELD-SYMBOL(<ls_fdt>).
      <ls_fdt>-active = abap_false.
    ENDLOOP.

    LOOP AT ms_ctx_db-fdn_tab ASSIGNING FIELD-SYMBOL(<ls_fdn>).
      <ls_fdn>-active = abap_false.
    ENDLOOP.


* -------- loop all collectors
    DATA(lv_cnt_ok)   = 0.
    DATA(lv_cnt_err)  = 0.
    DATA(lv_success)  = abap_false.

    LOOP AT ms_result-collected ASSIGNING FIELD-SYMBOL(<ls_col>).
      DATA(lv_lin_col) = lines( <ls_col>-fields ).
      ADD lv_lin_col TO ms_ctx_db-msg-cnt_fields.

      get_logger( )->trace( |process { lv_lin_col } fields from collector { <ls_col>-collector }| ).

      LOOP AT <ls_col>-fields ASSIGNING FIELD-SYMBOL(<ls_fld>).
        IF <ls_fld>-value_text IS NOT INITIAL.
          lv_success = prepare_field_text(
              is_col     = <ls_col>
              is_fld     = <ls_fld>
          ).
        ELSE.
          lv_success = prepare_field_numeric(
              is_col     = <ls_col>
              is_fld     = <ls_fld>
          ).
        ENDIF.

        IF lv_success EQ abap_true.
          ADD 1 TO lv_cnt_ok.
        ELSE.
          ADD 1 TO lv_cnt_err.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

* -------- prepare result
    get_logger( )->info( |processing result for { lv_lin_col } fields from collector { <ls_col>-collector }: { lv_cnt_ok } OK, { lv_cnt_err } errors| ).
    IF lv_cnt_ok > 0.
      rv_success = abap_true.
    ELSE.
      rv_success = abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD PREPARE_FIELD_NUMERIC.

* -------- check existing
    READ TABLE ms_ctx_db-fdn_tab ASSIGNING FIELD-SYMBOL(<ls_fld>)
      WITH KEY collector = is_col-collector
               category  = is_fld-category
               field     = is_fld-key
               value     = is_fld-value_number.

    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO ms_ctx_db-fdn_tab ASSIGNING <ls_fld>.
      <ls_fld>-fdn_guid       = zcl_nwhd_factory=>create_guid( ).
      <ls_fld>-src_guid       = ms_ctx_db-src-src_guid.
      <ls_fld>-created_at     = ms_ctx_db-src-last_contact_at.
      <ls_fld>-measured_at    = is_col-finished_at.

      <ls_fld>-collector    = is_col-collector.
      <ls_fld>-category     = is_fld-category.
      <ls_fld>-field        = is_fld-key.
      <ls_fld>-value        = is_fld-value_number.
    ELSE.
      ADD 1 TO <ls_fld>-confirmed_count.
      <ls_fld>-confirmed_at = is_col-finished_at.
    ENDIF.


* --------
    <ls_fld>-msg_guid_last  = ms_ctx_db-msg-msg_guid.
    <ls_fld>-active         = abap_true.


    rv_success = abap_true.

  ENDMETHOD.


  METHOD PREPARE_FIELD_TEXT.
* -------- check existing
    READ TABLE ms_ctx_db-fdt_tab ASSIGNING FIELD-SYMBOL(<ls_fld>)
      WITH KEY collector = is_col-collector
               category  = is_fld-category
               field     = is_fld-key
               value     = is_fld-value_number.

    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO ms_ctx_db-fdt_tab ASSIGNING <ls_fld>.
      <ls_fld>-fdt_guid       = zcl_nwhd_factory=>create_guid( ).
      <ls_fld>-src_guid       = ms_ctx_db-src-src_guid.
      <ls_fld>-created_at     = ms_ctx_db-src-last_contact_at.
      <ls_fld>-measured_at    = is_col-finished_at.

      <ls_fld>-collector    = is_col-collector.
      <ls_fld>-category     = is_fld-category.
      <ls_fld>-field        = is_fld-key.
      <ls_fld>-value        = is_fld-value_number.
    ELSE.
      ADD 1 TO <ls_fld>-confirmed_count.
      <ls_fld>-confirmed_at = is_col-finished_at.
    ENDIF.


* --------
    <ls_fld>-msg_guid_last  = ms_ctx_db-msg-msg_guid.
    <ls_fld>-active         = abap_true.


    rv_success = abap_true.

  ENDMETHOD.


  METHOD PREPARE_SOURCE.

* --------- reset db context
    CLEAR ms_ctx_db.

* ===================================== SOURCE
* --------- check source exists
    SELECT SINGLE *
      FROM ztd_nwhdldb_src
      INTO CORRESPONDING FIELDS OF ms_ctx_db-src
     WHERE source_type = ms_result-source_type
       AND source_id   = ms_result-source_id.

* ---------- new record
    IF sy-subrc NE 0.
      ms_ctx_db-src-src_guid      = zcl_nwhd_factory=>create_guid( ).
      ms_ctx_db-src-source_type   = ms_result-source_type.
      ms_ctx_db-src-source_id     = ms_result-source_id.
      GET TIME STAMP FIELD ms_ctx_db-src-created_at.
      get_logger( )->trace( |new source detected: { ms_result-source_type }-{ ms_result-source_id } | ).
    ELSE.
      get_logger( )->trace( |source known: { ms_result-source_type }-{ ms_result-source_id }| ).
    ENDIF.


* ---------- set update information
    GET TIME STAMP FIELD ms_ctx_db-src-last_contact_at.
    ms_ctx_db-src-last_source_ref = ms_result-source_ref.

* =================================== MEG
    MOVE-CORRESPONDING ms_result TO ms_ctx_db-msg.
    ms_ctx_db-msg-msg_guid      = zcl_nwhd_factory=>create_guid( ).
    ms_ctx_db-msg-src_guid      = ms_ctx_db-src-src_guid.
    ms_ctx_db-msg-cnt_tags      = lines( ms_result-tags ).
    ms_ctx_db-msg-created_at    = ms_ctx_db-src-last_contact_at.


* ----------
    rv_success = abap_true.

  ENDMETHOD.


  METHOD PREPARE_TAGS.
    get_logger( )->warning( |tags are not implemented yet| ).
    rv_success = abap_true.
  ENDMETHOD.


  METHOD zif_nwhd_ldb_bl~save.

* ------ prepare
    IF zif_nwhd_ldb_bl~save_prepare(
         is_result          = is_result
         iv_auto_create     = iv_auto_create
         iv_default_allowed = iv_default_allowed
       ) EQ abap_false.
      RETURN.
    ENDIF.


* ------- save db
    IF zif_nwhd_ldb_bl~save_to_ldb(
         iv_commit = iv_commit
         iv_wait   = iv_wait
       ) EQ abap_false.
      RETURN.
    ENDIF.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_ldb_bl~save_prepare.

* ------- check data
    CLEAR: ms_ctx_db, ms_result.
    IF zif_nwhd_ldb_bl~is_source_valid(
       iv_type = is_result-source_type
       iv_id   = is_result-source_id
       iv_auto_create = iv_auto_create
       iv_default_allowed = iv_default_allowed
      ) EQ abap_false.
      get_logger( )->error( |source type { is_result-source_type } id { is_result-source_id } is not valid| ).
      RETURN.
    ELSE.
      ms_result = is_result.
    ENDIF.

* ------- prepare source
    IF prepare_source( ) EQ abap_false.
      get_logger( )->error( |prepare source failed| ).
      RETURN.
    ENDIF.

* ------- prepare tags
    IF prepare_tags( ) EQ abap_false.
      get_logger( )->error( |prepare tags failed| ).
      RETURN.
    ENDIF.

* ------- prepare fields
    IF prepare_fields( ) EQ abap_false.
      get_logger( )->error( |prepare fields failed| ).
      RETURN.
    ENDIF.

* -------- final message
    get_logger( )->info( |LDB save date prepared| ).
    rv_success = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_ldb_bl~save_to_ldb.

* -------- check context
    IF ms_ctx_db IS INITIAL
      OR ms_ctx_db-src IS INITIAL
      OR ms_ctx_db-msg IS INITIAL.
      get_logger( )->error( |nothing to save to ldb| ).
      RETURN.
    ENDIF.


* ------- src + msg
    MODIFY ztd_nwhdldb_src FROM ms_ctx_db-src.
    MODIFY ztd_nwhdldb_msg FROM ms_ctx_db-msg.

* ------- numeric values
    UPDATE ztd_nwhdldb_fdn
       SET active   = abap_false
     WHERE src_guid = ms_ctx_db-src-src_guid
       AND active   = abap_true.
    IF ms_ctx_db-fdn_tab[] IS NOT INITIAL.
      MODIFY ztd_nwhdldb_fdn FROM TABLE ms_ctx_db-fdn_tab.
    ENDIF.

* ------- text values
    UPDATE ztd_nwhdldb_fdt
       SET active   = abap_false
     WHERE src_guid = ms_ctx_db-src-src_guid
       AND active   = abap_true.
    IF ms_ctx_db-fdt_tab[] IS NOT INITIAL.
      MODIFY ztd_nwhdldb_fdt FROM TABLE ms_ctx_db-fdt_tab.
    ENDIF.

* ------- commit
    IF iv_commit EQ abap_true.
      IF iv_wait EQ abap_true.
        COMMIT WORK AND WAIT.
      ELSE.
        COMMIT WORK.
      ENDIF.
      get_logger( )->info( |ldb updated| ).
    ELSE.
      get_logger( )->warning( |ldb updated without commit| ).
    ENDIF.

* -------- success
    rv_success = abap_true.

  ENDMETHOD.
ENDCLASS.
