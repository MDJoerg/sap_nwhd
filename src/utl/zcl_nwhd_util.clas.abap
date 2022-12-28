class ZCL_NWHD_UTIL definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_UTIL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_UTIL IMPLEMENTATION.


  METHOD zif_nwhd_util~get_all_mod_col.
    SELECT clsname
      FROM zv_nwhd_mod_col
      INTO TABLE rt_col.
  ENDMETHOD.


  method ZIF_NWHD_UTIL~GET_ALL_MOD_JOB.
    SELECT clsname
      FROM zv_nwhd_mod_job
      INTO TABLE rt_job.
  endmethod.


  METHOD zif_nwhd_util~get_all_mod_pub.
    SELECT clsname
      FROM zv_nwhd_mod_pub
      INTO TABLE rt_pub.
  ENDMETHOD.


  METHOD zif_nwhd_util~from_json.
    cl_fdt_json=>json_to_data(
      EXPORTING
        iv_json = iv_json
*      IMPORTING
*        ev_meta =
      CHANGING
        ca_data = cs_data
    ).

    IF cs_data IS NOT INITIAL.
      rv_success = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_nwhd_util~to_json.
    rv_json = cl_fdt_json=>data_to_json(
      EXPORTING
        ia_data = is_data
*        iv_id   =                  " FDT: Universal Unique Identifier
    ).
  ENDMETHOD.


  METHOD zif_nwhd_util~webservice_post.

* ----------- prepare
    rv_success        = abap_false.
    ev_http_status    = 'Unknown SAP Error'.
    ev_http_code      = 0.

    DATA lr_client TYPE REF TO if_http_client.

* ----------- get client
    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = iv_rfcdest                  " Logical destination (specified in function call)
      IMPORTING
        client                   = lr_client                  " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found       = 1                " Connection Parameter (Destination) Not Available
        destination_not_found    = 2                " Destination not found
        destination_no_authority = 3                " No Authorization to Use HTTP Destination
        plugin_not_active        = 4                " HTTP/HTTPS communication not available
        internal_error           = 5                " Internal error (e.g. name too long)
        OTHERS                   = 6
    ).
    IF sy-subrc <> 0.
      ev_http_status = |no client|.
      RETURN.
    ENDIF.


* --------- configure request
    lr_client->request->set_method( 'POST' ).
    lr_client->request->set_cdata( iv_payload ).
    IF iv_content_type IS NOT INITIAL.
      lr_client->request->set_content_type( iv_content_type ).
    ELSE.
      lr_client->request->set_content_type( 'text/text' ).
    ENDIF.

* --------- send
    lr_client->send(
*    EXPORTING
*      timeout                    = co_timeout_default " Timeout of Answer Waiting Time
      EXCEPTIONS
        http_communication_failure = 1                  " Communication Error
        http_invalid_state         = 2                  " Invalid state
        http_processing_failed     = 3                  " Error when processing method
        http_invalid_timeout       = 4                  " Invalid Time Entry
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
      ev_http_status = |sending failed|.
      RETURN.
    ENDIF.

* ---------- receive
    lr_client->receive(
      EXCEPTIONS
        http_communication_failure = 1                " Communication Error
        http_invalid_state         = 2                " Invalid state
        http_processing_failed     = 3                " Error when processing method
        OTHERS                     = 4
    ).
    IF sy-subrc <> 0.
      ev_http_status = |receiving data failed|.
      RETURN.
    ENDIF.


* ----------- get status and response
    lr_client->response->get_status(
      IMPORTING
        code   = ev_http_code                 " HTTP Status Code
        reason = ev_http_status                 " HTTP status description
    ).


* ----------- fill response
    ev_response = lr_client->response->get_cdata( ).
    lr_client->close(
      EXCEPTIONS
        http_invalid_state = 1                " Invalid state
        OTHERS             = 2
    ).
    rv_success = abap_true.

  ENDMETHOD.


  METHOD zif_nwhd_util~get_md5_string_hash.

    DATA lv_xstring TYPE xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_data
*       MIMETYPE       = ' '
*       ENCODING       =
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.



    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
*       ALG            = 'SHA1'
        data           = lv_xstring
*       LENGTH         = 0
      IMPORTING
*       HASH           =
*       HASHLEN        =
        hashx          = rv_hash
*       HASHXLEN       =
*       HASHSTRING     =
*       HASHXSTRING    =
*       HASHB64STRING  =
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.

  ENDMETHOD.


  METHOD zif_nwhd_util~get_md5_tags_hash.
    DATA(lv_sep)    = '||'.
    DATA(lv_string) = |{ lv_sep }|.

    DATA(lt_tag) = it_tag.
    SORT lt_tag.

    LOOP AT lt_tag ASSIGNING FIELD-SYMBOL(<ls_tag>).
      lv_string = |{ lv_string }'{ <ls_tag>-tag }'='{ <ls_tag>-value }'{ lv_sep }|.
    ENDLOOP.

    rv_hash = zif_nwhd_util~get_md5_string_hash( lv_string ).
  ENDMETHOD.
ENDCLASS.
