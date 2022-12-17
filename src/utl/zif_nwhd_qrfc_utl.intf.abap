interface ZIF_NWHD_QRFC_UTL
  public .


  methods START_INBOUND_QUEUE
    importing
      !IV_QUEUE type DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods START_OUTBOUND_QUEUE
    importing
      !IV_QUEUE type DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods CLOSE_PACKAGE
    importing
      !IV_WAIT type ABAP_BOOL default ABAP_FALSE .
  methods RESTART_PACKAGE
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
endinterface.
