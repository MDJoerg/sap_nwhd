CLASS zcl_nwhd_rsa_res_fdn_av DEFINITION
  PUBLIC
  INHERITING FROM zcl_nwhd_rws_resource
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_rest_resource~get
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_NWHD_RSA_RES_FDN_AV IMPLEMENTATION.


  METHOD if_rest_resource~get.

* ---- local data
    DATA lv_ts_from TYPE timestamp.
    DATA lv_ts_to   TYPE timestamp.

* --------- get params
    TRY.
        DATA(lv_src) = get_param( zif_nwhd_rsa_c=>c_param_source ).

        DATA(lv_date_from) = get_param(
                                       iv_key     = zif_nwhd_rsa_c=>c_param_date_from
                                       iv_default = CONV string( sy-datum - zif_nwhd_rsa_c=>c_default_date_interval )
                                     ).

        DATA(lv_date_to) = get_param(
                                       iv_key     = zif_nwhd_rsa_c=>c_param_date_to
                                       iv_default = CONV string( sy-datum )
                                     ).

      CATCH cx_root INTO DATA(lx_exc).
        DATA(lv_error) = lx_exc->get_text( ).
        send_response_error( lv_error ).
        RETURN.
    ENDTRY.


* --------- init tools
    DATA(lr_util) = zcl_nwhd_factory=>create_util( ).
    DATA(lr_ldb) = zcl_nwhd_factory=>create_ldb_bl( ).


* --------- select ldb for active sources
    DATA(lt_fdn_cnt) = lr_ldb->get_fdn_available(
                         iv_src       = lv_src
                         iv_date_from = CONV sydatum( lv_date_from )
                         iv_date_to   = CONV sydatum( lv_date_to )
                       ).
    IF lt_fdn_cnt[] IS INITIAL.
      send_response_json( |[]| ).
      RETURN.
    ENDIF.

* --------- transform to json
    DATA(lv_json) = lr_util->to_json( is_data = lt_fdn_cnt ).
    IF lv_json IS INITIAL.
      send_response_error(
        EXPORTING
          iv_error_text = |invalid response|
*          iv_error_code = 500
      ).
    ELSE.
      send_response_json( lv_json ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
