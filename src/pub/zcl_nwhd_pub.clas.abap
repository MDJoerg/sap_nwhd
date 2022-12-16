class ZCL_NWHD_PUB definition
  public
  inheriting from ZCL_NWHD_MOD
  create public .

public section.

  interfaces ZIF_NWHD_PUB .

  aliases GET_LOGGER
    for ZIF_NWHD_MOD~GET_LOGGER .
  aliases GET_NAME
    for ZIF_NWHD_MOD~GET_NAME .
  aliases SET_LOGGER
    for ZIF_NWHD_MOD~SET_LOGGER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_PUB IMPLEMENTATION.


  method ZIF_NWHD_PUB~PUBLISH.
    get_logger( )->error( |publish not implemented in module| ).
  endmethod.
ENDCLASS.
