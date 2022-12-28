class ZCL_NWHD_REC_WS_ROUTER definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.

  methods HANDLE_CSRF_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_REC_WS_ROUTER IMPLEMENTATION.


  method HANDLE_CSRF_TOKEN.
*CALL METHOD SUPER->HANDLE_CSRF_TOKEN
*  EXPORTING
*    IO_CSRF_HANDLER =
*    IO_REQUEST      =
*    IO_RESPONSE     =
*    .
  endmethod.


  METHOD if_rest_application~get_root_handler.

* -------- init
    DATA(lr_router) = NEW cl_rest_router( ).
    ro_root_handler = lr_router.

* -------- add path
    lr_router->attach( iv_template = '/' iv_handler_class = 'ZCL_NWHD_REC_WS_RES_PING' ).
    lr_router->attach( iv_template = '/ping' iv_handler_class = 'ZCL_NWHD_REC_WS_RES_PING' ).
    lr_router->attach( iv_template = '/receive' iv_handler_class = 'ZCL_NWHD_REC_WS_RES_RECEIVE' ).


  ENDMETHOD.
ENDCLASS.
