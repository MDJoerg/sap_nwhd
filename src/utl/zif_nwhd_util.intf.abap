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
  methods GET_MD5_STRING_HASH
    importing
      !IV_DATA type STRING
    returning
      value(RV_HASH) type HASH160X .
  methods GET_MD5_TAGS_HASH
    importing
      !IT_TAG type ZNWHD_T_DATA_TAG
    returning
      value(RV_HASH) type HASH160X .
  methods GUI_DOWNLOAD_STRING
    importing
      !IV_DATA type STRING
      !IV_TITLE type STRING optional
      !IV_FILENAME type STRING optional
      !IV_EXT type STRING default 'json'
    exporting
      !EV_FILENAME type STRING
      !EV_PATH type STRING
      !EV_FULLPATH type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods STRING_TO_XSTRING
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
  methods XSTRING_TO_BINTAB
    importing
      !IV_XSTRING type XSTRING
    exporting
      !EV_LENGTH type I
    returning
      value(RT_BINTAB) type SOLIX_TAB .
endinterface.
