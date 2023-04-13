INTERFACE zif_nwhd_ldb_bl
  PUBLIC .


  INTERFACES zif_nwhd_api .

  ALIASES get_logger
    FOR zif_nwhd_api~get_logger .
  ALIASES set_logger
    FOR zif_nwhd_api~set_logger .

  METHODS is_source_valid
    IMPORTING
      !iv_type            TYPE data
      !iv_id              TYPE data
      !iv_auto_create     TYPE abap_bool DEFAULT abap_true
      !iv_default_allowed TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_success)   TYPE abap_bool .
  METHODS is_message_valid
    IMPORTING
      !iv_type          TYPE data
      !iv_id            TYPE data
      !iv_source_ref    TYPE znwhd_source_ref
    EXPORTING
      !es_msg           TYPE znwhd_ldb_s_db_msg
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS get_last_source
    RETURNING
      VALUE(rs_src) TYPE znwhd_ldb_s_db_src .
  METHODS save
    IMPORTING
      !is_result          TYPE znwhd_s_data_job
      !iv_auto_create     TYPE abap_bool DEFAULT abap_true
      !iv_default_allowed TYPE abap_bool DEFAULT abap_true
      !iv_commit          TYPE abap_bool DEFAULT abap_true
      !iv_wait            TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_success)   TYPE abap_bool .
  METHODS save_prepare
    IMPORTING
      !is_result          TYPE znwhd_s_data_job
      !iv_auto_create     TYPE abap_bool DEFAULT abap_true
      !iv_default_allowed TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_success)   TYPE abap_bool .
  METHODS save_to_ldb
    IMPORTING
      !iv_commit        TYPE abap_bool DEFAULT abap_true
      !iv_wait          TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS read_msg_context
    IMPORTING
      !iv_msg_guid      TYPE znwhd_guid
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS get_context_as_result
    RETURNING
      VALUE(rs_result) TYPE znwhd_s_data_job .
  METHODS source_export
    IMPORTING
      !iv_src_guid     TYPE znwhd_guid_src
      !iv_started_at   TYPE znwhd_timestampl_started_at
      !iv_finished_at  TYPE znwhd_timestampl_finished_at
    RETURNING
      VALUE(rs_result) TYPE znwhd_ldb_s_ctx_db .
  METHODS source_delete
    IMPORTING
      !iv_src_guid      TYPE znwhd_guid_src
      !iv_commit        TYPE abap_bool DEFAULT abap_false
      !iv_test          TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_deleted) TYPE i .
  METHODS source_import
    IMPORTING
      !is_data          TYPE znwhd_ldb_s_ctx_db
      !iv_commit        TYPE abap_bool DEFAULT abap_false
      !iv_test          TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS get_active_sources
    RETURNING
      VALUE(rt_src) TYPE znwhd_ldb_t_src_rec .
  METHODS get_fdn_available
    IMPORTING
      !iv_src           TYPE data
      !iv_date_from     TYPE sydatum
      !iv_date_to       TYPE sydatum
    RETURNING
      VALUE(rt_fdn_cnt) TYPE znwhd_ldb_t_fdn_cnt .

  METHODS get_fdn_values
    IMPORTING
      !iv_src       TYPE data
      !iv_collector TYPE data
      !iv_category  TYPE data
      !iv_field     TYPE data
      !iv_date_from TYPE sydatum
      !iv_date_to   TYPE sydatum
      !iv_max_rows  TYPE i OPTIONAL
    RETURNING
      VALUE(rt_fdn) TYPE znwhd_t_data_fdn .


ENDINTERFACE.
