interface ZIF_NWHD_JOB
  public .


  interfaces ZIF_NWHD_MOD .

  aliases GET_LOGGER
    for ZIF_NWHD_MOD~GET_LOGGER .
  aliases GET_NAME
    for ZIF_NWHD_MOD~GET_NAME .
  aliases SET_LOGGER
    for ZIF_NWHD_MOD~SET_LOGGER .

  methods PROCESS
    importing
      !IS_PARAMS type ZNWHD_S_PARAM_JOB
      !IV_PUBLISH type ABAP_BOOL default ABAP_TRUE
    exporting
      !ES_RESULT type ZNWHD_S_DATA_JOB
      !EV_STEP type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods PUBLISH
    importing
      !IS_PARAMS type ZNWHD_S_PARAM_JOB
      !IS_RESULT type ZNWHD_S_DATA_JOB
    exporting
      !ET_PUBLISHED type STRING_TABLE
      !ET_PUBLISH_ERROR type STRING_TABLE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.
