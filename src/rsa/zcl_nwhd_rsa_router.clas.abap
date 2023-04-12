class ZCL_NWHD_RSA_ROUTER definition
  public
  inheriting from ZCL_NWHD_RWS_HANDLER
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_RSA_ROUTER IMPLEMENTATION.


  METHOD if_rest_application~get_root_handler.

* -------- constants for resource handler
    DATA lc_src_av  TYPE seoclsname VALUE 'ZCL_NWHD_RSA_RES_SRC_AV'.
    DATA lc_fdn_av  TYPE seoclsname VALUE 'ZCL_NWHD_RSA_RES_FDN_AV'.
    DATA lc_fdn_ts  TYPE seoclsname VALUE 'ZCL_NWHD_RSA_RES_FDN_TS'.

* -------- init
    DATA(lr_router) = NEW cl_rest_router( ).
    ro_root_handler = lr_router.


* ======== API V1
* -------- sources
    lr_router->attach( iv_template = '/v1/sources' iv_handler_class = lc_src_av ).

* -------- numeric values
    lr_router->attach( iv_template = '/v1/numeric_available/{source}' iv_handler_class = lc_fdn_av ).
    lr_router->attach( iv_template = '/v1/numeric_timeseries/{source}/{collector}/{category}/{field}' iv_handler_class = lc_fdn_ts ).


  ENDMETHOD.
ENDCLASS.
