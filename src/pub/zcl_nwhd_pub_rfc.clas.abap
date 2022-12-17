class ZCL_NWHD_PUB_RFC definition
  public
  inheriting from ZCL_NWHD_PUB
  create public .

public section.

  methods ZIF_NWHD_PUB~PUBLISH
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_PUB_RFC IMPLEMENTATION.


  METHOD zif_nwhd_pub~publish.

* -------- local data
    DATA lv_msg TYPE bapi_msg.


* -------- check
    IF is_params-rfc_dest IS INITIAL.
      get_logger( )->error( |RFC destination missing. Use NONE for local.| ).
      RETURN.
    ENDIF.

* -------- call remote function
    DATA(ls_params) = is_params.

    CALL FUNCTION 'Z_NWHD_REC_INB_LDB'
      DESTINATION ls_params-rfc_dest
      EXPORTING
        is_result             = is_result
        is_params             = is_params
      EXCEPTIONS
        wrong_data            = 1
        wrong_installation    = 2
        system_failure        = 3 MESSAGE lv_msg
        communication_failure = 4 MESSAGE lv_msg
        OTHERS                = 5.
    IF sy-subrc <> 0.
      get_logger( )->error( |Error sending data to { ls_params-rfc_dest } - subrc { sy-subrc } - { lv_msg })| ).
    ELSE.
      get_logger( )->info( |Sending data to { ls_params-rfc_dest } finished| ).
      rv_success = abap_true.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
