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


  METHOD zif_nwhd_util~gui_download_string.

* ---------- ask user
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = iv_title                 " Window Title
        default_extension         = iv_ext                 " Default Extension
        default_file_name         = iv_filename                 " Default File Name
*      with_encoding             =
*      file_filter               =                  " File Type Filter Table
*      initial_directory         =                  " Initial Directory
*      prompt_on_overwrite       = 'X'
      CHANGING
        filename                  = ev_filename                 " File Name to Save
        path                      = ev_path                 " Path to File
        fullpath                  = ev_fullpath                 " Path + File Name
*      user_action               =                  " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*      file_encoding             =
      EXCEPTIONS
        cntl_error                = 1                " Control error
        error_no_gui              = 2                " No GUI available
        not_supported_by_gui      = 3                " GUI does not support this
        invalid_default_file_name = 4                " Invalid default file name
        OTHERS                    = 5
    ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.


* --------- check
    IF ev_filename IS INITIAL.
      rv_success = abap_true.
      RETURN.
    ENDIF.

* --------- transform to bin tab
    DATA(lv_xstring) = zif_nwhd_util~string_to_xstring( iv_data ).
    IF lv_xstring IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_len) = 0.
    DATA(lt_bin) = zif_nwhd_util~xstring_to_bintab(
    EXPORTING
      iv_xstring = lv_xstring
    IMPORTING
     ev_length  = lv_len
  ).
    IF lt_bin[] IS INITIAL OR lv_len = 0.
      RETURN.
    ENDIF.



* --------- save to gui
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_len                     " File length for binary files
        filename                  = ev_fullpath                     " Name of file
        filetype                  = 'BIN'                " File type (ASCII, binary ...)
*        append                    = space                " Character Field of Length 1
*        write_field_separator     = space                " Separate Columns by Tabs in Case of ASCII Download
*        header                    = '00'                 " Byte Chain Written to Beginning of File in Binary Mode
*        trunc_trailing_blanks     = space                " Do not Write Blank at the End of Char Fields
*        write_lf                  = 'X'                  " Insert CR/LF at End of Line in Case of Char Download
*        col_select                = space                " Copy Only Selected Columns of the Table
*        col_select_mask           = space                " Vector Containing an 'X' for the Column To Be Copied
*        dat_mode                  = space                " Numeric and date fields are in DAT format in WS_DOWNLOAD
*        confirm_overwrite         = space                " Overwrite File Only After Confirmation
*        no_auth_check             = space                " Switch off Check for Access Rights
*        codepage                  =                      " Character Representation for Output
*        ignore_cerr               = abap_true            " Ignore character set conversion errors?
*        replacement               = '#'                  " Replacement Character for Non-Convertible Characters
*        write_bom                 = space                " If set, writes a Unicode byte order mark
*        trunc_trailing_blanks_eol = 'X'                  " Remove Trailing Blanks in Last Column
*        wk1_n_format              = space
*        wk1_n_size                = space
*        wk1_t_format              = space
*        wk1_t_size                = space
*        show_transfer_status      = 'X'                  " Enables suppression of transfer status message
*        fieldnames                =                      " Table Field Names
*        write_lf_after_last_line  = 'X'                  " Writes a CR/LF after final data record
*        virus_scan_profile        = '/SCET/GUI_DOWNLOAD' " Virus Scan Profile
*      IMPORTING
*        filelength                =                      " Number of bytes transferred
      CHANGING
        data_tab                  =  lt_bin                    " Transfer table
      EXCEPTIONS
*        file_write_error          = 1                    " Cannot write to file
*        no_batch                  = 2                    " Cannot execute front-end function in background
*        gui_refuse_filetransfer   = 3                    " Incorrect Front End
*        invalid_type              = 4                    " Invalid value for parameter FILETYPE
*        no_authority              = 5                    " No Download Authorization
*        unknown_error             = 6                    " Unknown error
*        header_not_allowed        = 7                    " Invalid header
*        separator_not_allowed     = 8                    " Invalid separator
*        filesize_not_allowed      = 9                    " Invalid file size
*        header_too_long           = 10                   " Header information currently restricted to 1023 bytes
*        dp_error_create           = 11                   " Cannot create DataProvider
*        dp_error_send             = 12                   " Error Sending Data with DataProvider
*        dp_error_write            = 13                   " Error Writing Data with DataProvider
*        unknown_dp_error          = 14                   " Error when calling data provider
*        access_denied             = 15                   " Access to file denied.
*        dp_out_of_memory          = 16                   " Not enough memory in data provider
*        disk_full                 = 17                   " Storage medium is full.
*        dp_timeout                = 18                   " Data provider timeout
*        file_not_found            = 19                   " Could not find file
*        dataprovider_exception    = 20                   " General Exception Error in DataProvider
*        control_flush_error       = 21                   " Error in Control Framework
*        not_supported_by_gui      = 22                   " GUI does not support this
*        error_no_gui              = 23                   " GUI not available
        OTHERS                    = 24
    ).
    IF sy-subrc EQ 0.
      rv_success = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_nwhd_util~string_to_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_string
*       MIMETYPE       = ' '
*       ENCODING       =
      IMPORTING
        buffer = rv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
  ENDMETHOD.


  METHOD zif_nwhd_util~xstring_to_bintab.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstring
*       APPEND_TO_TABLE       = ' '
      IMPORTING
        output_length = ev_length
      TABLES
        binary_tab    = rt_bintab.


  ENDMETHOD.
ENDCLASS.
