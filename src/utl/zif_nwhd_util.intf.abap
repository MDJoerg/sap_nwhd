INTERFACE zif_nwhd_util
  PUBLIC .


  METHODS get_all_mod_col
    RETURNING
      VALUE(rt_col) TYPE string_table .
  METHODS get_all_mod_job
    RETURNING
      VALUE(rt_job) TYPE string_table .
  METHODS get_all_mod_pub
    RETURNING
      VALUE(rt_pub) TYPE string_table .
  METHODS webservice_post
    IMPORTING
      !iv_rfcdest       TYPE rfcdest
      !iv_content_type  TYPE string OPTIONAL
      !iv_payload       TYPE string
    EXPORTING
      !ev_response      TYPE string
      !ev_http_code     TYPE i
      !ev_http_status   TYPE string
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS to_json
    IMPORTING
      !is_data       TYPE data
    RETURNING
      VALUE(rv_json) TYPE string .
  METHODS from_json
    IMPORTING
      !iv_json          TYPE string
    CHANGING
      !cs_data          TYPE data
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS get_md5_string_hash
    IMPORTING
      !iv_data       TYPE string
    RETURNING
      VALUE(rv_hash) TYPE hash160x .
  METHODS get_md5_tags_hash
    IMPORTING
      !it_tag        TYPE znwhd_t_data_tag
    RETURNING
      VALUE(rv_hash) TYPE hash160x .
  METHODS gui_download_string
    IMPORTING
      !iv_data          TYPE string
      !iv_title         TYPE string OPTIONAL
      !iv_filename      TYPE string OPTIONAL
      !iv_ext           TYPE string DEFAULT 'json'
    EXPORTING
      !ev_filename      TYPE string
      !ev_path          TYPE string
      !ev_fullpath      TYPE string
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS string_to_xstring
    IMPORTING
      !iv_string        TYPE string
    RETURNING
      VALUE(rv_xstring) TYPE xstring .
  METHODS xstring_to_bintab
    IMPORTING
      !iv_xstring      TYPE xstring
    EXPORTING
      !ev_length       TYPE i
    RETURNING
      VALUE(rt_bintab) TYPE solix_tab .
  METHODS get_config_fld_num
    IMPORTING
      !iv_collector    TYPE data OPTIONAL
      !iv_category     TYPE data OPTIONAL
      !iv_field        TYPE data OPTIONAL
      !iv_bp           TYPE data OPTIONAL
      !iv_src_type     TYPE data OPTIONAL
      !iv_src_id       TYPE data OPTIONAL
      !it_cfg          TYPE znwhd_cfg_t_fdn_rec OPTIONAL
      !iv_check_field  TYPE data OPTIONAL
    EXPORTING
      !et_cfg          TYPE znwhd_cfg_t_fdn_rec
    RETURNING
      VALUE(rs_config) TYPE znwhd_cfg_s_fdn_rec .
  METHODS get_time_interval
    IMPORTING
      !iv_type      TYPE znwhd_time_interval_type
      !iv_base      TYPE znwhd_time_interval_base DEFAULT 1
    RETURNING
      VALUE(rv_min) TYPE int4 .

  METHODS timestamp_to_utc_time
    IMPORTING
      !iv_timestamp       TYPE timestampl
    RETURNING
      VALUE(rv_timestamp) TYPE string .
ENDINTERFACE.
