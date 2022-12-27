FUNCTION z_nwhd_pub_out_rws.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_RFCDEST) TYPE  RFCDEST
*"     VALUE(IV_PAYLOAD) TYPE  STRING
*"     VALUE(IV_CONTENT_TYPE) TYPE  STRING
*"  EXCEPTIONS
*"      ERRORS_OCCURED
*"----------------------------------------------------------------------

* -------- init tooling
  DATA(lr_ut_qrfc) = zcl_nwhd_factory=>create_qrfc_util( ).
  DATA(lr_util)    = zcl_nwhd_factory=>create_util( ).

  DATA lv_http_code TYPE i.
  DATA lv_http_text TYPE string.
  DATA lv_response  TYPE string.


* -------- call webservice
  DATA(lv_success) = lr_util->webservice_post(
    EXPORTING
      iv_rfcdest      = iv_rfcdest
      iv_content_type = iv_content_type
      iv_payload      = iv_payload
    IMPORTING
      ev_response     = lv_response
      ev_http_code    = lv_http_code
      ev_http_status  = lv_http_text
  ).


* ------- check response
  IF lv_success EQ abap_true
    AND lv_http_code >= 200 AND lv_http_code < 300.
    " success
  ELSE.
    lr_ut_qrfc->restart_package( ).
  ENDIF.

ENDFUNCTION.
