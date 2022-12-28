class ZCL_NWHD_UTL_REST_RESOURCE definition
  public
  inheriting from CL_REST_RESOURCE
  create public .

public section.
PROTECTED SECTION.

  METHODS send_response
    IMPORTING
      !iv_payload      TYPE string OPTIONAL
      !iv_content_type TYPE string DEFAULT 'text/html'
      !iv_http_status  TYPE i DEFAULT 200
      !iv_http_reason  TYPE string DEFAULT 'OK' .
private section.
ENDCLASS.



CLASS ZCL_NWHD_UTL_REST_RESOURCE IMPLEMENTATION.


  METHOD send_response.

*   fill data
    IF iv_payload IS NOT INITIAL.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_content_type ).
      lo_entity->set_string_data( iv_payload ).
    ENDIF.

*   set http status
    mo_response->set_status(
      EXPORTING
        iv_status        = iv_http_status
        iv_reason_phrase = iv_http_reason
    ).


  ENDMETHOD.
ENDCLASS.
