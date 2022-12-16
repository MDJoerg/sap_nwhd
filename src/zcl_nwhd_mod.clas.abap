class ZCL_NWHD_MOD definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_MOD .
protected section.

  data MR_LOGGER type ref to ZIF_NWHD_LOGGER .
private section.
ENDCLASS.



CLASS ZCL_NWHD_MOD IMPLEMENTATION.


  method ZIF_NWHD_MOD~GET_LOGGER.
    if mr_logger is INITIAL.
      mr_logger = zcl_nwhd_factory=>create_logger( ).
    endif.
    rr_logger = mr_logger.
  endmethod.


  method ZIF_NWHD_MOD~GET_NAME.
    rv_name = sy-repid.
  endmethod.


  method ZIF_NWHD_MOD~SET_LOGGER.
    mr_logger = ir_logger.
  endmethod.
ENDCLASS.
