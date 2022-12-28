interface ZIF_NWHD_MOD
  public .


  methods GET_NAME
    returning
      value(RV_NAME) type STRING .
  methods GET_LOGGER
    returning
      value(RR_LOGGER) type ref to ZIF_NWHD_LOGGER .
  methods SET_LOGGER
    importing
      !IR_LOGGER type ref to ZIF_NWHD_LOGGER .
endinterface.
