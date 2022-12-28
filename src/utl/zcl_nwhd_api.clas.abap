class ZCL_NWHD_API definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_API .
protected section.

  aliases GET_UTIL
    for ZIF_NWHD_API~GET_UTIL .

  data MR_LOGGER type ref to ZIF_NWHD_LOGGER .
  data MR_UTIL type ref to ZIF_NWHD_UTIL .
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


  METHOD zif_nwhd_api~get_util.
    IF mr_util IS INITIAL.
      mr_util = zcl_nwhd_factory=>create_util( ).
    ENDIF.
    rr_util = mr_util.
  ENDMETHOD.
ENDCLASS.
