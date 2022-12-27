class ZCL_NWHD_PUB_QRWS definition
  public
  inheriting from ZCL_NWHD_PUB
  create public .

public section.

  methods ZIF_NWHD_PUB~PUBLISH
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_PUB_QRWS IMPLEMENTATION.


  METHOD zif_nwhd_pub~publish.

* -------- local data
    DATA lv_msg TYPE bapi_msg.


* -------- check
    IF is_params-rfc_dest IS INITIAL
      OR is_params-qname IS INITIAL.
      get_logger( )->error( |qRFC destination or queue missing.| ).
      RETURN.
    ENDIF.

* ------ get payload
    DATA(lr_util)   = zcl_nwhd_factory=>create_util( ).
    DATA(lv_payload) = lr_util->to_json( is_data = is_result ).
    IF lv_payload IS INITIAL.
      get_logger( )->error( |wrong json payload| ).
      RETURN.
    ENDIF.

    DATA(lv_contenttype) = |application/json|.


* -------- call remote function
    DATA(ls_params) = is_params.
    DATA(lr_qrfc)   = zcl_nwhd_factory=>create_qrfc_util( ).


* ------ init outbound queue
    IF lr_qrfc->start_outbound_queue( ls_params-qname ) EQ abap_false.
      get_logger( )->error( |Error setting outbound queue { ls_params-qname }| ).
      RETURN.
    ENDIF.


* ------ create queue entry
    CALL FUNCTION 'Z_NWHD_PUB_OUT_RWS'
      IN BACKGROUND TASK
      AS SEPARATE UNIT
      DESTINATION 'NONE'
      EXPORTING
        iv_rfcdest      = is_params-rfc_dest
        iv_payload      = lv_payload
        iv_content_type = lv_contenttype.


* ------- finish queue package
    lr_qrfc->close_package( ).
    get_logger( )->info( |Sending data webservice to { ls_params-rfc_dest } via queue { ls_params-qname } prepared| ).
    rv_success = abap_true.


  ENDMETHOD.
ENDCLASS.
