interface ZIF_NWHD_PUB
  public .


  interfaces ZIF_NWHD_MOD .

  aliases GET_LOGGER
    for ZIF_NWHD_MOD~GET_LOGGER .
  aliases GET_NAME
    for ZIF_NWHD_MOD~GET_NAME .
  aliases SET_LOGGER
    for ZIF_NWHD_MOD~SET_LOGGER .

  methods PUBLISH
    importing
      !IS_PARAMS type ZNWHD_S_PARAM_JOB
      !IS_RESULT type ZNWHD_S_DATA_JOB
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.
