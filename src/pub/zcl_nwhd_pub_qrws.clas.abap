class ZCL_NWHD_PUB_QRWS definition
  public
  inheriting from ZCL_NWHD_PUB
  create public .

public section.

  methods ZIF_NWHD_PUB~PUBLISH
    redefinition .
protected section.

  data MS_PARAMS type ZNWHD_S_PARAM_JOB .
  data MS_RESULT type ZNWHD_S_DATA_JOB .
  data MV_PAYLOAD type STRING .
  data MV_CONTENT_TYPE type STRING .

  methods BUILD_PAYLOAD
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods FORWARD_PAYLOAD
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods RESET .
private section.
ENDCLASS.



CLASS ZCL_NWHD_PUB_QRWS IMPLEMENTATION.


  METHOD zif_nwhd_pub~publish.

* -------- local data
    DATA lv_msg TYPE bapi_msg.


* -------- init and check
    reset( ).
    IF is_params-rfc_dest IS INITIAL
      OR is_params-qname IS INITIAL.
      get_logger( )->error( |qRFC destination or queue missing.| ).
      RETURN.
    ELSE.
      ms_params = is_params.
      ms_result = is_result.
    ENDIF.


* -------- get payload
    IF build_payload( ) EQ abap_false.
      get_logger( )->error( |wrong json payload| ).
      RETURN.
    ENDIF.

* -------- forward
    IF forward_payload( ) EQ abap_true.
      rv_success = abap_true.
    ELSE.
      get_logger( )->error( |forwarding json payload failed| ).
    ENDIF.


  ENDMETHOD.


  METHOD build_payload.
    DATA(lr_util)   = zcl_nwhd_factory=>create_util( ).
    mv_payload = lr_util->to_json( is_data = ms_result ).
    IF mv_payload IS NOT INITIAL.
      mv_content_type = |application/json|.
      rv_success = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD forward_payload.

* -------- call remote function
    DATA(lr_qrfc)   = zcl_nwhd_factory=>create_qrfc_util( ).


* ------ init outbound queue
    IF lr_qrfc->start_outbound_queue( ms_params-qname ) EQ abap_false.
      get_logger( )->error( |Error setting outbound queue { ms_params-qname }| ).
      RETURN.
    ENDIF.


* ------ create queue entry
    CALL FUNCTION 'Z_NWHD_PUB_OUT_RWS'
      IN BACKGROUND TASK
      AS SEPARATE UNIT
      DESTINATION 'NONE'
      EXPORTING
        iv_rfcdest      = ms_params-rfc_dest
        iv_payload      = mv_payload
        iv_content_type = mv_content_type.


* ------- finish queue package
    lr_qrfc->close_package( ).
    get_logger( )->info( |Sending data webservice to { ms_params-rfc_dest } via queue { ms_params-qname } prepared| ).
    rv_success = abap_true.

  ENDMETHOD.


  method RESET.
      clear: ms_params,
             ms_result,
             mv_payload,
             mv_content_type.
  endmethod.
ENDCLASS.
