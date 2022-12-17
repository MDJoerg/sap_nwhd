class ZCL_NWHD_PUB_QRFC definition
  public
  inheriting from ZCL_NWHD_PUB
  create public .

public section.

  methods ZIF_NWHD_PUB~PUBLISH
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_PUB_QRFC IMPLEMENTATION.


  METHOD zif_nwhd_pub~publish.

* -------- local data
    DATA lv_msg TYPE bapi_msg.


* -------- check
    IF is_params-rfc_dest IS INITIAL
      OR is_params-qname IS INITIAL.
      get_logger( )->error( |qRFC destination or queue missing.| ).
      RETURN.
    ENDIF.

* -------- call remote function
    DATA(ls_params) = is_params.
    DATA(lr_qrfc)   = zcl_nwhd_factory=>create_qrfc_util( ).


* ------ init outbound queue
    IF lr_qrfc->start_outbound_queue( ls_params-qname ) EQ abap_false.
      get_logger( )->error( |Error setting outbound queue { ls_params-qname }| ).
      RETURN.
    ENDIF.

* ------ create queue entry
    CALL FUNCTION 'Z_NWHD_REC_INB_LDB'
      IN BACKGROUND TASK
      AS SEPARATE UNIT
      DESTINATION ls_params-rfc_dest
      EXPORTING
        is_result = is_result
        is_params = ls_params.


* ------- finish queue package
    lr_qrfc->close_package( ).
    get_logger( )->info( |Sending data to { ls_params-rfc_dest } via queue { ls_params-qname } finished| ).
    rv_success = abap_true.


  ENDMETHOD.
ENDCLASS.
