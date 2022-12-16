interface ZIF_NWHD_COL
  public .


  interfaces ZIF_NWHD_MOD .

  aliases GET_LOGGER
    for ZIF_NWHD_MOD~GET_LOGGER .
  aliases GET_NAME
    for ZIF_NWHD_MOD~GET_NAME .
  aliases SET_LOGGER
    for ZIF_NWHD_MOD~SET_LOGGER .

  methods COLLECT
    importing
      !IT_PARAM type ZNWHD_T_PARAM_VALUE optional
    exporting
      !ES_DATA type ZNWHD_S_DATA_COL
      !EV_STEP type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.
