class ZCL_NWHD_LDB_PUB definition
  public
  inheriting from ZCL_NWHD_PUB
  create public .

public section.

  methods ZIF_NWHD_PUB~PUBLISH
    redefinition .
protected section.

  data MS_CTX_DB type ZNWHD_LDB_S_CTX_DB .
  data MS_JOB_PARAMS type ZNWHD_S_PARAM_JOB .
  data MS_JOB_RESULT type ZNWHD_S_DATA_JOB .

  methods IS_SOURCE_VALID
    importing
      !IV_TYPE type DATA
      !IV_ID type DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
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
  methods SAVE_LDB
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_NWHD_LDB_PUB IMPLEMENTATION.


  method IS_SOURCE_VALID.
    get_logger( )->trace( |no whitelist activated| ).
    rv_success = abap_true.
  endmethod.


  METHOD PREPARE_FIELDS.

* -------- check contect
    IF ms_ctx_db-src-src_guid IS INITIAL.
      get_logger( )->error( |wrong fields context| ).
      RETURN.
    ENDIF.

    IF ms_job_result-collected[] IS INITIAL.
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

    LOOP AT ms_job_result-collected ASSIGNING FIELD-SYMBOL(<ls_col>).
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
     WHERE source_type = ms_job_result-source_type
       AND source_id   = ms_job_result-source_id.

* ---------- new record
    IF sy-subrc NE 0.
      ms_ctx_db-src-src_guid      = zcl_nwhd_factory=>create_guid( ).
      ms_ctx_db-src-source_type   = ms_job_result-source_type.
      ms_ctx_db-src-source_id     = ms_job_result-source_id.
      GET TIME STAMP FIELD ms_ctx_db-src-created_at.
      get_logger( )->trace( |new source detected: { ms_job_result-source_type }-{ ms_job_result-source_id } | ).
    ELSE.
      get_logger( )->trace( |source known: { ms_job_result-source_type }-{ ms_job_result-source_id }| ).
    ENDIF.


* ---------- set update information
    GET TIME STAMP FIELD ms_ctx_db-src-last_contact_at.
    ms_ctx_db-src-last_source_ref = ms_job_result-source_ref.

* =================================== MEG
    MOVE-CORRESPONDING ms_job_result TO ms_ctx_db-msg.
    ms_ctx_db-msg-msg_guid      = zcl_nwhd_factory=>create_guid( ).
    ms_ctx_db-msg-src_guid      = ms_ctx_db-src-src_guid.
    ms_ctx_db-msg-cnt_tags      = lines( ms_job_result-tags ).
    ms_ctx_db-msg-created_at    = ms_ctx_db-src-last_contact_at.


* ----------
    rv_success = abap_true.

  ENDMETHOD.


  METHOD PREPARE_TAGS.
    get_logger( )->warning( |tags are not implemented yet| ).
    rv_success = abap_true.
  ENDMETHOD.


  METHOD SAVE_LDB.

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
    COMMIT WORK AND WAIT.
    get_logger( )->info( |ldb updated| ).
    rv_success = abap_true.
  ENDMETHOD.


  METHOD ZIF_NWHD_PUB~PUBLISH.

* ------- init protocol
    get_logger( )->info( |starting publishing for source type { is_result-source_type } id { is_result-source_id } ref { is_result-source_ref }| ).
    IF is_result IS INITIAL.
      get_logger( )->error( |invalid data| ).
      RETURN.
    ENDIF.

* ------- check data
    IF is_source_valid(
       iv_type = is_result-source_type
       iv_id   = is_result-source_id
      ) EQ abap_false.
      get_logger( )->error( |source type { is_result-source_type } id { is_result-source_id } is not valid| ).
      RETURN.
    ELSE.
      ms_job_params = is_params.
      ms_job_result = is_result.
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

* ------- save ldb
    IF save_ldb( ) EQ abap_false.
      get_logger( )->error( |save local db failed| ).
      RETURN.
    ENDIF.


* -------- final message
    get_logger( )->info( |Published to LDB| ).
    rv_success = abap_true.
  ENDMETHOD.
ENDCLASS.
