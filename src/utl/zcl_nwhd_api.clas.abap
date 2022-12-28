class ZCL_NWHD_API definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_API .
protected section.

  data MR_LOGGER type ref to ZIF_NWHD_LOGGER .
private section.
ENDCLASS.



CLASS ZCL_NWHD_API IMPLEMENTATION.


  METHOD zif_nwhd_api~get_logger.
    IF mr_logger IS INITIAL.
      mr_logger = zcl_nwhd_factory=>create_logger( ).
    ENDIF.
    rr_logger = mr_logger.
  ENDMETHOD.


  method ZIF_NWHD_API~SET_LOGGER.
    mr_logger = ir_logger.
  endmethod.
ENDCLASS.
