class ZCL_NWHD_REC_WS_RES_RECEIVE definition
  public
  inheriting from ZCL_NWHD_UTL_REST_RESOURCE
  create public .

public section.

  methods IF_REST_RESOURCE~POST
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_REC_WS_RES_RECEIVE IMPLEMENTATION.


  METHOD if_rest_resource~post.

* -------- get content
    DATA(lv_payload) = io_entity->get_string_data( ).
    IF lv_payload IS INITIAL.
      send_response(
*    iv_payload      =
*    iv_content_type = 'text/html'
        iv_http_status  = 400
        iv_http_reason  = 'MISSING_PAYLOAD'
    )  .
      RETURN.
    ENDIF.


* -------- transform to result
    DATA(lr_util) = zcl_nwhd_factory=>create_util( ).
    DATA ls_data TYPE znwhd_s_data_job.

    IF lr_util->from_json(
      EXPORTING
        iv_json    = lv_payload
      CHANGING
        cs_data    = ls_data
    ) EQ abap_false
      OR ls_data IS INITIAL
      OR ls_data-source_type IS INITIAL
      OR ls_data-source_id   IS INITIAL
      OR ls_data-source_ref  IS INITIAL.
      send_response(
*    iv_payload      =
*    iv_content_type = 'text/html'
        iv_http_status  = 400
        iv_http_reason  = 'INVALID_PAYLOAD'
    )  .
      RETURN.
    ENDIF.

* -------- check source system
    DATA(lr_ldb) = zcl_nwhd_factory=>create_ldb_bl( ).
    IF lr_ldb->is_source_valid(
         iv_type            = ls_data-source_type
         iv_id              = ls_data-source_id
         iv_auto_create     = abap_true
         iv_default_allowed = abap_false
       ) EQ abap_false.
      send_response(
*    iv_payload      =
*    iv_content_type = 'text/html'
        iv_http_status  = 400
        iv_http_reason  = 'INVALID_SOURCE'
    )  .
      RETURN.
    ENDIF.


* --------- check source message
    IF lr_ldb->is_message_valid(
           iv_type       = ls_data-source_type
           iv_id         = ls_data-source_id
           iv_source_ref = ls_data-source_ref                 " Source Reference
       ) EQ abap_false.
      send_response(
*    iv_payload      =
*    iv_content_type = 'text/html'
        iv_http_status  = 400
        iv_http_reason  = 'INVALID_SOURCE_MSG'
    )  .
      RETURN.
    ENDIF.


* --------- prepare queue
    DATA(ls_src) = lr_ldb->get_last_source( ).
    DATA(lv_queue) = COND trfcqnam( WHEN ls_src-qname_inbound IS NOT INITIAL
                                    THEN ls_src-qname_inbound
                                    ELSE |ZNWHD_{ ls_src-source_id }| ).


* -------- create qrfc package
    DATA(ls_params) = VALUE znwhd_s_param_job( ).
    DATA(lr_qrfc)   = zcl_nwhd_factory=>create_qrfc_util( ).


* ------ init outbound queue
    IF lr_qrfc->start_inbound_queue( lv_queue ) EQ abap_false.
      send_response(
*    iv_payload      =
*    iv_content_type = 'text/html'
        iv_http_status  = 500
        iv_http_reason  = 'CREATE_QUEUE_FAILED'
    )  .
      RETURN.
    ENDIF.

* ------ create queue entry
    CALL FUNCTION 'Z_NWHD_REC_INB_LDB'
      IN BACKGROUND TASK
      AS SEPARATE UNIT
      DESTINATION 'NONE'
      EXPORTING
        is_result = ls_data
        is_params = ls_params.


* ------- finish queue package
    lr_qrfc->close_package( ).



* --------- send success message
    send_response(
*    iv_payload      =
*    iv_content_type = 'text/html'
      iv_http_status  = 200
      iv_http_reason  = 'OK'
  )  .

  ENDMETHOD.
ENDCLASS.
