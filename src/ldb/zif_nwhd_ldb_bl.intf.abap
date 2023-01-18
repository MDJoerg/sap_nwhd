interface ZIF_NWHD_LDB_BL
  public .


  interfaces ZIF_NWHD_API .

  aliases GET_LOGGER
    for ZIF_NWHD_API~GET_LOGGER .
  aliases SET_LOGGER
    for ZIF_NWHD_API~SET_LOGGER .

  methods IS_SOURCE_VALID
    importing
      !IV_TYPE type DATA
      !IV_ID type DATA
      !IV_AUTO_CREATE type ABAP_BOOL default ABAP_TRUE
      !IV_DEFAULT_ALLOWED type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods IS_MESSAGE_VALID
    importing
      !IV_TYPE type DATA
      !IV_ID type DATA
      !IV_SOURCE_REF type ZNWHD_SOURCE_REF
    exporting
      !ES_MSG type ZNWHD_LDB_S_DB_MSG
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods GET_LAST_SOURCE
    returning
      value(RS_SRC) type ZNWHD_LDB_S_DB_SRC .
  methods SAVE
    importing
      !IS_RESULT type ZNWHD_S_DATA_JOB
      !IV_AUTO_CREATE type ABAP_BOOL default ABAP_TRUE
      !IV_DEFAULT_ALLOWED type ABAP_BOOL default ABAP_TRUE
      !IV_COMMIT type ABAP_BOOL default ABAP_TRUE
      !IV_WAIT type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods SAVE_PREPARE
    importing
      !IS_RESULT type ZNWHD_S_DATA_JOB
      !IV_AUTO_CREATE type ABAP_BOOL default ABAP_TRUE
      !IV_DEFAULT_ALLOWED type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods SAVE_TO_LDB
    importing
      !IV_COMMIT type ABAP_BOOL default ABAP_TRUE
      !IV_WAIT type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods READ_MSG_CONTEXT
    importing
      !IV_MSG_GUID type ZNWHD_GUID
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods GET_CONTEXT_AS_RESULT
    returning
      value(RS_RESULT) type ZNWHD_S_DATA_JOB .
  methods SOURCE_EXPORT
    importing
      !IV_SRC_GUID type ZNWHD_GUID_SRC
      !IV_STARTED_AT type ZNWHD_TIMESTAMPL_STARTED_AT
      !IV_FINISHED_AT type ZNWHD_TIMESTAMPL_FINISHED_AT
    returning
      value(RS_RESULT) type ZNWHD_LDB_S_CTX_DB .
endinterface.
