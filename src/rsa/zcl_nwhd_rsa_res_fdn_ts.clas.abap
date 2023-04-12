class ZCL_NWHD_RSA_RES_FDN_TS definition
  public
  inheriting from ZCL_NWHD_RWS_RESOURCE
  create public .

public section.

  methods IF_REST_RESOURCE~GET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_RSA_RES_FDN_TS IMPLEMENTATION.


  METHOD if_rest_resource~get.
* ---------- local data
    DATA: BEGIN OF ls_out,
            value     TYPE znwhd_value_number,
            timestamp TYPE string,
          END OF ls_out.
    DATA: lt_out LIKE TABLE OF ls_out.

* --------- get params
    TRY.
        DATA(lv_src) = get_param( zif_nwhd_rsa_c=>c_param_source ).
        DATA(lv_col) = get_param( zif_nwhd_rsa_c=>c_param_collector ).
        DATA(lv_cat) = get_param( zif_nwhd_rsa_c=>c_param_category ).
        DATA(lv_fld) = get_param( zif_nwhd_rsa_c=>c_param_field ).

        DATA(lv_date_from) = get_param(
                                       iv_key     = zif_nwhd_rsa_c=>c_param_date_from
                                       iv_default = CONV string( sy-datum - zif_nwhd_rsa_c=>c_default_date_interval )
                                     ).

        DATA(lv_date_to) = get_param(
                                       iv_key     = zif_nwhd_rsa_c=>c_param_date_to
                                       iv_default = CONV string( sy-datum )
                                     ).

        DATA(lv_max) = get_param(
                                       iv_key     = zif_nwhd_rsa_c=>c_param_max_rows
                                       iv_default = CONV string( zif_nwhd_rsa_c=>c_default_max_rows )
                                     ).
        DATA(lv_max_rows) = CONV integer( lv_max ).

      CATCH cx_root INTO DATA(lx_exc).
        DATA(lv_error) = lx_exc->get_text( ).
        send_response_error( lv_error ).
        RETURN.
    ENDTRY.


* --------- init tools
    DATA(lr_util) = zcl_nwhd_factory=>create_util( ).
    DATA(lr_ldb) = zcl_nwhd_factory=>create_ldb_bl( ).


* --------- select ldb for active sources
    DATA(lt_fdn) = lr_ldb->get_fdn_values(
                     iv_src       = lv_src
                     iv_collector = lv_col
                     iv_category  = lv_cat
                     iv_field     = lv_fld
                     iv_date_from = CONV sydatum( lv_date_from )
                     iv_date_to   = CONV sydatum( lv_date_to )
                     iv_max_rows  = lv_max_rows
                   ).
    IF lt_fdn[] IS INITIAL.
      send_response_json( |[]| ).
      RETURN.
    ENDIF.


* --------- transform to external format
    LOOP AT lt_fdn ASSIGNING FIELD-SYMBOL(<ls_data>).
      ls_out-value = <ls_data>-value.
      ls_out-timestamp = lr_util->timestamp_to_utc_time( <ls_data>-measured_at ).
      APPEND ls_out TO lt_out.

      IF <ls_data>-confirmed_count > 0.
        ls_out-value = <ls_data>-value.
        ls_out-timestamp = lr_util->timestamp_to_utc_time( <ls_data>-confirmed_at ).
        APPEND ls_out TO lt_out.
      ENDIF.
    ENDLOOP.


* --------- transform to json
    DATA(lv_json) = lr_util->to_json( is_data = lt_out ).
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
