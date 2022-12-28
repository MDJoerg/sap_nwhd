interface ZIF_NWHD_API
  public .


  methods GET_LOGGER
    returning
      value(RR_LOGGER) type ref to ZIF_NWHD_LOGGER .
  methods SET_LOGGER
    importing
      !IR_LOGGER type ref to ZIF_NWHD_LOGGER .
  methods GET_UTIL
    returning
      value(RR_UTIL) type ref to ZIF_NWHD_UTIL .
endinterface.
