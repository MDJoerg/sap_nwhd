class ZCL_NWHD_LDB_PUB definition
  public
  inheriting from ZCL_NWHD_PUB
  create public .

public section.

  methods ZIF_NWHD_PUB~PUBLISH
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_LDB_PUB IMPLEMENTATION.


  METHOD zif_nwhd_pub~publish.

* ------- init protocol
    get_logger( )->info( |starting publishing for source type { is_result-source_type } id { is_result-source_id } ref { is_result-source_ref }| ).
    IF is_result IS INITIAL.
      get_logger( )->error( |invalid data| ).
      RETURN.
    ENDIF.


* -------- prepare
    DATA(lr_ldb) = zcl_nwhd_factory=>create_ldb_bl( ).
    lr_ldb->set_logger( get_logger( ) ).

    IF lr_ldb->save(
      EXPORTING
        is_result          = is_result                  " NWHD: Job Data
        iv_auto_create     = abap_true
        iv_default_allowed = abap_true
        iv_commit          = abap_true
        iv_wait            = abap_true
    ) EQ abap_false.
      get_logger( )->error( |ldb save failed| ).
      RETURN.
    ELSE.
      get_logger( )->info( |ldb save finished| ).
      rv_success = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
