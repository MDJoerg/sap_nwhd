interface ZIF_NWHD_LOGGER
  public .


  methods ADD
    importing
      !IV_ID type SYMSGID default 'ZNWHD'
      !IV_NUMBER type SYMSGNO default '000'
      !IV_TYPE type SYMSGTY default 'S'
      !IV_MSG type DATA optional
      !IV_P1 type DATA optional
      !IV_P2 type DATA optional
      !IV_P3 type DATA optional
      !IV_P4 type DATA optional
    returning
      value(RS_MSG) type BAPIRET2 .
  methods TRACE
    importing
      !IV_MSG type DATA .
  methods INFO
    importing
      !IV_MSG type DATA .
  methods WARNING
    importing
      !IV_MSG type DATA .
  methods ERROR
    importing
      !IV_MSG type DATA .
  methods GET_MSG_TAB
    returning
      value(RT_MSG) type BAPIRET2_TAB .
  methods ADD_MSG_TAB
    importing
      !IT_MSG type BAPIRET2_TAB .
endinterface.
