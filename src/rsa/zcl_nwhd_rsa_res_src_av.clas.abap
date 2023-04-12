class ZCL_NWHD_RSA_RES_SRC_AV definition
  public
  inheriting from ZCL_NWHD_RWS_RESOURCE
  create public .

public section.

  methods IF_REST_RESOURCE~GET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_RSA_RES_SRC_AV IMPLEMENTATION.


  METHOD if_rest_resource~get.

* --------- local data
    DATA: BEGIN OF ls_out,
            src_guid           TYPE string,
            source_id          TYPE string,
            source_type        TYPE string,
            source_description TYPE string,
          END OF ls_out.
    DATA lt_out LIKE TABLE OF ls_out.


* --------- init tools
    DATA(lr_util) = zcl_nwhd_factory=>create_util( ).
    DATA(lr_ldb) = zcl_nwhd_factory=>create_ldb_bl( ).


* --------- select ldb for active sources
    DATA(lt_src) = lr_ldb->get_active_sources( ).
    IF lt_src[] IS INITIAL.
      send_response_json( |[]| ).
      RETURN.
    ENDIF.

* --------- transform results to outbound format
    LOOP AT lt_src ASSIGNING FIELD-SYMBOL(<ls_data>).
      MOVE-CORRESPONDING <ls_data> TO ls_out.
      APPEND ls_out TO lt_out.
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
