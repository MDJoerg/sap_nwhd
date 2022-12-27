interface ZIF_NWHD_UTIL
  public .


  methods GET_ALL_MOD_COL
    returning
      value(RT_COL) type STRING_TABLE .
  methods GET_ALL_MOD_JOB
    returning
      value(RT_JOB) type STRING_TABLE .
  methods GET_ALL_MOD_PUB
    returning
      value(RT_PUB) type STRING_TABLE .
  methods WEBSERVICE_POST
    importing
      !IV_RFCDEST type RFCDEST
      !IV_CONTENT_TYPE type STRING optional
      !IV_PAYLOAD type STRING
    exporting
      !EV_RESPONSE type STRING
      !EV_HTTP_CODE type I
      !EV_HTTP_STATUS type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods TO_JSON
    importing
      !IS_DATA type DATA
    returning
      value(RV_JSON) type STRING .
  methods FROM_JSON
    importing
      !IV_JSON type STRING
    changing
      !CS_DATA type DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.
