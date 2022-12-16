class ZCL_NWHD_LOGGER definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_LOGGER .
protected section.

  data MT_MSG type BAPIRET2_TAB .
private section.
ENDCLASS.



CLASS ZCL_NWHD_LOGGER IMPLEMENTATION.


  METHOD zif_nwhd_logger~add.

    rs_msg-id           = iv_id.
    rs_msg-type         = iv_type.
    rs_msg-number       = iv_number.
    rs_msg-message_v1   = iv_p1.
    rs_msg-message_v2   = iv_p2.
    rs_msg-message_v3   = iv_p3.
    rs_msg-message_v4   = iv_p4.
    rs_msg-message      = iv_msg.

    IF rs_msg-message IS INITIAL.
      MESSAGE ID iv_id
            TYPE iv_type
          NUMBER iv_number
            WITH iv_p1
                 iv_p2
                 iv_p3
                 iv_p4
            INTO rs_msg-message.
    ENDIF.

    append rs_msg to mt_msg.

  ENDMETHOD.


  method ZIF_NWHD_LOGGER~ADD_MSG_TAB.
    append LINES OF iT_msg to mt_msg.
  endmethod.


  method ZIF_NWHD_LOGGER~ERROR.
    zif_nwhd_logger~add(
        iv_id     = 'ZNWHD'
        iv_number = '004'
        iv_type   = 'E'
        iv_msg    = iv_msg
    ).
  endmethod.


  method ZIF_NWHD_LOGGER~GET_MSG_TAB.
    rt_msg = mt_msg.
  endmethod.


  method ZIF_NWHD_LOGGER~INFO.
    zif_nwhd_logger~add(
        iv_id     = 'ZNWHD'
        iv_number = '002'
        iv_type   = 'I'
        iv_msg    = iv_msg
    ).
 endmethod.


  METHOD zif_nwhd_logger~trace.
    zif_nwhd_logger~add(
        iv_id     = 'ZNWHD'
        iv_number = '001'
        iv_type   = 'S'
        iv_msg    = iv_msg
    ).
  ENDMETHOD.


  method ZIF_NWHD_LOGGER~WARNING.
    zif_nwhd_logger~add(
        iv_id     = 'ZNWHD'
        iv_number = '003'
        iv_type   = 'W'
        iv_msg    = iv_msg
    ).
  endmethod.
ENDCLASS.
