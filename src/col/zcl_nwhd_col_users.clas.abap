class ZCL_NWHD_COL_USERS definition
  public
  inheriting from ZCL_NWHD_COL
  create public .

public section.

  constants C_NAME type STRING value 'Users' ##NO_TEXT.

  methods ZIF_NWHD_COL~GET_NAME
    redefinition .
protected section.

  methods COLLECT_DATA
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_NWHD_COL_USERS IMPLEMENTATION.


  METHOD collect_data.

* ---------- check params
    IF ms_col_params-flag_no_system_wide EQ abap_true.
      rv_success = abap_true.
      RETURN.
    ENDIF.


* ---------- local data
    DATA: lt_data   TYPE TABLE OF uinfos.
    DATA: lt_users  TYPE string_table.

* ---------- call api
    CALL FUNCTION 'TH_SYSTEMWIDE_USER_LIST'
      TABLES
        list           = lt_data
      EXCEPTIONS
        argument_error = 1
        send_error     = 2
        OTHERS         = 3.


* --------- calc
* get users and modes
    DATA(lv_cnt_mod_intern) = 0.
    DATA(lv_cnt_mod_extern) = 0.
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
      READ TABLE lt_users TRANSPORTING NO FIELDS
        WITH KEY table_line = <lfs_data>-bname.
      IF sy-subrc NE 0.
        APPEND <lfs_data>-bname TO lt_users.
      ENDIF.

      IF <lfs_data>-intmodi NE '*'.
        ADD <lfs_data>-intmodi TO lv_cnt_mod_intern.
      ENDIF.

      IF <lfs_data>-extmodi NE '*'.
        ADD <lfs_data>-extmodi TO lv_cnt_mod_extern.
      ENDIF.
    ENDLOOP.

* calc
    DESCRIBE TABLE lt_data LINES DATA(lv_cnt_sessions).
    DESCRIBE TABLE lt_users LINES DATA(lv_cnt_users).
    DATA(lv_cnt_mod_all) = lv_cnt_mod_intern + lv_cnt_mod_extern.
    DATA(lv_cnt_mod_per_user) = CONV float( lv_cnt_mod_all / lv_cnt_users ).

* --------- publish sums
    append_number_value(
        iv_category = 'Current'
        iv_key      = 'Sessions'
        iv_value    = lv_cnt_sessions
    ).

    append_number_value(
        iv_category = 'Current'
        iv_key      = 'LoggedOn'
        iv_value    = lv_cnt_users
    ).

    append_number_value(
        iv_category = 'Current'
        iv_key      = 'Modes'
        iv_value    = lv_cnt_mod_all
    ).

    append_number_value(
        iv_category = 'Current'
        iv_key      = 'ModesPerUser'
        iv_value    = lv_cnt_mod_per_user
    ).


    append_number_value(
        iv_category = 'Modes'
        iv_key      = 'Internal'
        iv_value    = lv_cnt_mod_intern
    ).

    append_number_value(
        iv_category = 'Modes'
        iv_key      = 'External'
        iv_value    = lv_cnt_mod_extern
    ).


* ---------- return
    rv_success = abap_true.

  ENDMETHOD.


  METHOD ZIF_NWHD_COL~GET_NAME.
    rv_name = zcl_nwhd_col_users=>c_name.
  ENDMETHOD.
ENDCLASS.
