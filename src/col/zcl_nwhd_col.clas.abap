class ZCL_NWHD_COL definition
  public
  inheriting from ZCL_NWHD_MOD
  create public .

public section.

  interfaces ZIF_NWHD_COL .
protected section.

  data MS_DATA type ZNWHD_S_DATA_COL .

  methods APPEND_NUMBER_VALUE
    importing
      !IV_CATEGORY type STRING optional
      !IV_KEY type STRING
      !IV_VALUE type DATA .
  methods APPEND_TEXT_VALUE
    importing
      !IV_CATEGORY type STRING optional
      !IV_KEY type STRING
      !IV_VALUE type DATA .
  methods CLOSE_DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods COLLECT_DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods INIT_DATA
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL IMPLEMENTATION.


  METHOD append_number_value.
    DATA(lv_value) = CONV znwhd_value_number( iv_value ).
    APPEND INITIAL LINE TO ms_data-fields ASSIGNING FIELD-SYMBOL(<ls_new>).
    <ls_new>-category     = iv_category.
    <ls_new>-key          = iv_key.
    <ls_new>-value_number = lv_value.
  ENDMETHOD.


  METHOD append_text_value.
    DATA(lv_value) = CONV znwhd_value_text( iv_value ).
    APPEND INITIAL LINE TO ms_data-fields ASSIGNING FIELD-SYMBOL(<ls_new>).
    <ls_new>-category     = iv_category.
    <ls_new>-key          = iv_key.
    <ls_new>-value_number = lv_value.
  ENDMETHOD.


  METHOD close_data.
    IF ms_data IS INITIAL.
      RETURN.
    ELSE.
      GET TIME STAMP FIELD ms_data-finished_at.
      rv_success = abap_true.
    ENDIF.
  ENDMETHOD.


  method COLLECT_DATA.
    rv_success = abap_false.
  endmethod.


  METHOD init_data.
    CLEAR ms_data.
    ms_data-collector = zif_nwhd_col~get_name( ).
    GET TIME STAMP FIELD ms_data-started_at.
    rv_success = abap_true.
  ENDMETHOD.


  METHOD zif_nwhd_col~collect.
* ------ init
    IF init_data( ) EQ abap_false.
      RETURN.
    ENDIF.

* ------ collect
    IF collect_data( ) EQ abap_false.
      RETURN.
    ENDIF.

* ------ close
    IF close_data( ) EQ abap_false.
      RETURN.
    ENDIF.

* ------ success
    rv_success = abap_true.
    es_data    = ms_data.

  ENDMETHOD.
ENDCLASS.
