class ZCL_NWHD_REC_WS_RES_PING definition
  public
  inheriting from ZCL_NWHD_UTL_REST_RESOURCE
  create public .

public section.

  methods IF_REST_RESOURCE~GET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_REC_WS_RES_PING IMPLEMENTATION.


  METHOD if_rest_resource~get.

*   prepare
    DATA(lv_status) = |NWHD receiver webservice reached at { sy-sysid }{ sy-mandt }| .

*   fill data
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( 'text/html' ).
    lo_entity->set_string_data( lv_status ).

*   set http status
    mo_response->set_status(
      EXPORTING
        iv_status        = '200'
        iv_reason_phrase = lv_status
    ).
  ENDMETHOD.
ENDCLASS.
