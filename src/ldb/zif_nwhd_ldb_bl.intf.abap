interface ZIF_NWHD_LDB_BL
  public .


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
endinterface.
