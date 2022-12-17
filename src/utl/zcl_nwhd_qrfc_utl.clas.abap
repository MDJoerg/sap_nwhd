class ZCL_NWHD_QRFC_UTL definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_QRFC_UTL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_QRFC_UTL IMPLEMENTATION.


  METHOD zif_nwhd_qrfc_utl~close_package.
    IF iv_wait EQ abap_true.
      COMMIT WORK AND WAIT.
    ELSE.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD zif_nwhd_qrfc_utl~restart_package.
* ------ local data
    DATA: lt_return TYPE TABLE OF  bapiret2.

* ------ set success
    rv_success = abap_true.

* ------ call api
    CALL FUNCTION 'TRFC_SEND_BACK'
      EXPORTING
        astate                  = 'ARETRY'
*       IF_FNAME                =
      TABLES
        arstate                 = lt_return
*       ARDATA01                =
*       ARDATA02                =
*       ARDATA03                =
*       ARDATA04                =
*       ARDATA05                =
      EXCEPTIONS
        no_trfc_or_qrfc_mode    = 1
        unknown_state           = 2
        missing_interface_fname = 3
        OTHERS                  = 4.
    IF sy-subrc <> 0.
      rv_success = abap_false.
    ELSE.
      LOOP AT lt_return TRANSPORTING NO FIELDS
        WHERE type CA 'EAX'.
        rv_success = abap_false.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_nwhd_qrfc_utl~start_inbound_queue.

    SET UPDATE TASK LOCAL.

    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
      EXPORTING
*       QOUT_NAME          = ' '
        qin_name           = CONV trfcqnam( iv_queue )
*       QIN_COUNT          =
*       CALL_EVENT         = ' '
*       NO_EXECUTE         = ' '
      EXCEPTIONS
        invalid_queue_name = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.
      rv_success = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD zif_nwhd_qrfc_utl~start_outbound_queue.

    SET UPDATE TASK LOCAL.

    CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
      EXPORTING
        qname              = CONV trfcqnam( iv_queue )
*       NOSEND             = ' '
*       TRFC_IF_SYSFAIL    = ' '
*       CALL_EVENT         = ' '
      EXCEPTIONS
        invalid_queue_name = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.
      rv_success = abap_true.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
