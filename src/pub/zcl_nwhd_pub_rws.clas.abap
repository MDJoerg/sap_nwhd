class ZCL_NWHD_PUB_RWS definition
  public
  inheriting from ZCL_NWHD_PUB_QRWS
  create public .

public section.
protected section.

  methods FORWARD_PAYLOAD
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_PUB_RWS IMPLEMENTATION.


  METHOD forward_payload.

* ------- local data
    DATA lv_http_code TYPE i.
    DATA lv_http_text TYPE string.
    DATA lv_response  TYPE string.


* ------- check
    IF mv_payload IS INITIAL.
      get_logger( )->error( |invalid payload| ).
    ELSE.
* ------- send
      DATA(lr_util)    = zcl_nwhd_factory=>create_util( ).
      IF lr_util->webservice_post(
        EXPORTING
          iv_rfcdest      = ms_params-rfc_dest
          iv_content_type = mv_content_type
          iv_payload      = mv_payload
        IMPORTING
          ev_response     = lv_response
          ev_http_code    = lv_http_code
          ev_http_status  = lv_http_text
      ) EQ abap_true.
        get_logger( )->info( |WebService call result: code { lv_http_code } - { lv_http_text } - { lv_response }| ).
      ELSE.
        get_logger( )->error( |WebService call failed: code { lv_http_code } - { lv_http_text } - { lv_response }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
