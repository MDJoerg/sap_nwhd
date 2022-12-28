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
ENDINTERFACE.
