CLASS zcl_nwhd_rws_resource DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS get_endpoint
      RETURNING
        VALUE(rv_endpoint) TYPE string .
    METHODS get_request_payload_string
      RETURNING
        VALUE(rv_payload) TYPE string .
    METHODS get_resource_path
      RETURNING
        VALUE(rv_path) TYPE string .
    METHODS get_param
      IMPORTING
        !iv_key         TYPE data
        !iv_default     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_value) TYPE string .
    METHODS send_response_error
      IMPORTING
        !iv_error_text TYPE string
        !iv_error_code TYPE i DEFAULT 500 .
    METHODS send_response_html
      IMPORTING
        !iv_html       TYPE string
        !iv_error_text TYPE string OPTIONAL
        !iv_error_code TYPE i OPTIONAL .
    METHODS send_response_json
      IMPORTING
        !iv_payload    TYPE string
        !iv_error_text TYPE string OPTIONAL
        !iv_error_code TYPE i OPTIONAL .

    METHODS send_response_text
      IMPORTING
        !iv_text         TYPE string
        !iv_content_type TYPE string
        !iv_http_text    TYPE string DEFAULT 'OK'
        !iv_http_code    TYPE i DEFAULT 200 .

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_NWHD_RWS_RESOURCE IMPLEMENTATION.


  METHOD get_endpoint.
    rv_endpoint = mo_request->get_uri( ).
    DATA(lv_append) = mo_request->get_uri_path( ).

    IF rv_endpoint CS '?'.
      SPLIT rv_endpoint AT '?' INTO rv_endpoint DATA(lv_query).
    ENDIF.

    REPLACE lv_append IN rv_endpoint WITH ''.
  ENDMETHOD.


  METHOD get_param.
    IF iv_key IS NOT INITIAL.
      DATA(lv_name) = CONV string( iv_key ).
      rv_value = mo_request->get_uri_attribute( lv_name ).
      IF rv_value IS INITIAL.
        rv_value = mo_request->get_uri_query_parameter( lv_name ).
        IF rv_value IS INITIAL.
          rv_value = iv_default.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_request_payload_string.
    rv_payload = mo_request->get_entity( )->get_string_data( ).
  ENDMETHOD.


  METHOD get_resource_path.
    rv_path = mo_request->get_uri_path( ).
  ENDMETHOD.


  METHOD send_response_error.

*   get error code
    DATA(lv_code) = iv_error_code.
    IF lv_code LE 0.
      lv_code = cl_rest_status_code=>gc_client_error_bad_request.
    ENDIF.

*   set error and code
    mo_response->set_status(
      EXPORTING
        iv_status        = lv_code
        iv_reason_phrase = iv_error_text
    ).
  ENDMETHOD.


  METHOD send_response_html.
*   fill data
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_text_html ).
    lo_entity->set_string_data( iv_html ).

*   response with errors?
    IF iv_error_text IS INITIAL.
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    ELSE.
*   get error code
      DATA(lv_code) = iv_error_code.
      IF lv_code LE 0.
        lv_code = cl_rest_status_code=>gc_client_error_bad_request.
      ENDIF.

*   set error and code
      mo_response->set_status(
        EXPORTING
          iv_status        = lv_code
          iv_reason_phrase = iv_error_text
      ).
    ENDIF.
  ENDMETHOD.


  METHOD send_response_json.
*   fill data
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( iv_payload ).

*   response with errors?
    IF iv_error_text IS INITIAL.
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    ELSE.
*   get error code
      DATA(lv_code) = iv_error_code.
      IF lv_code LE 0.
        lv_code = cl_rest_status_code=>gc_client_error_bad_request.
      ENDIF.

*   set error and code
      mo_response->set_status(
        EXPORTING
          iv_status        = lv_code
          iv_reason_phrase = iv_error_text
      ).
    ENDIF.
  ENDMETHOD.


  METHOD send_response_text.
*   fill data
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_content_type ).
    lo_entity->set_string_data( iv_text ).

*   set http status
    mo_response->set_status(
      EXPORTING
        iv_status        = iv_http_code
        iv_reason_phrase = iv_http_text
    ).
  ENDMETHOD.
ENDCLASS.
