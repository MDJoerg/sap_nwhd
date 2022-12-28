class ZCL_NWHD_JOB definition
  public
  inheriting from ZCL_NWHD_MOD
  create public .

public section.

  interfaces ZIF_NWHD_JOB .

  aliases GET_LOGGER
    for ZIF_NWHD_MOD~GET_LOGGER .
  aliases SET_LOGGER
    for ZIF_NWHD_MOD~SET_LOGGER .
protected section.

  data MS_DATA type ZNWHD_S_DATA_JOB .
  data MS_PARAMS type ZNWHD_S_PARAM_JOB .

  methods CLOSE_JOB
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods GET_TAGS
    returning
      value(RT_TAGS) type ZNWHD_T_DATA_TAG .
  methods INIT_JOB
    importing
      !IS_PARAMS type ZNWHD_S_PARAM_JOB
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
  methods PROCESS_JOB
    exporting
      !EV_STEP type STRING
    returning
      value(RV_SUCCESS) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_NWHD_JOB IMPLEMENTATION.


  METHOD CLOSE_JOB.
    GET TIME STAMP FIELD ms_data-finished_at.
    ms_data-tags = get_tags( ).
    rv_success = abap_true.
  ENDMETHOD.


  METHOD get_tags.

* ----- local data & macros
    DATA ls_tag LIKE LINE OF rt_tags.
    DEFINE mc_add_tag.
      ls_tag-tag = &1.
      ls_tag-value = &2.
      APPEND ls_tag TO rt_tags.
    end-OF-DEFINITION.

* ------ add
    mc_add_tag 'OpSys'       sy-opsys.
    mc_add_tag 'Host'        sy-host.
    mc_add_tag 'DBName'      sy-dbnam.
    mc_add_tag 'DBSys'       sy-dbsys.
    mc_add_tag 'SAPRelease'  sy-saprl.
    mc_add_tag 'Timezone'    sy-zonlo.


* ------ final preps
    SORT rt_tags.

  ENDMETHOD.


  METHOD init_job.
    CLEAR: ms_data.
    ms_params = is_params.

    ms_data-source_ref  = zcl_nwhd_factory=>create_guid( ).
    ms_data-source_type = zif_nwhd_c=>c_source_type.
    ms_data-source_id   = COND #( WHEN is_params-source_id IS NOT INITIAL
                                  THEN is_params-source_id
                                  ELSE |{ sy-sysid }{ sy-mandt }| ).
    GET TIME STAMP FIELD ms_data-started_at.
    rv_success = abap_true.
  ENDMETHOD.


  METHOD process_job.

* ------- local data
    DATA: lt_param  TYPE znwhd_t_param_value.
    DATA: ls_col_data	TYPE znwhd_s_data_col.

* ------- check collector modules
    IF ms_params-collectors[] IS INITIAL.
      ev_step = 'init'.
      RETURN.
    ENDIF.


* ------- loop collectors
    DATA ls_col_param TYPE znwhd_s_param_job_col.
    MOVE-CORRESPONDING ms_params TO ls_col_param.

    LOOP AT ms_params-collectors ASSIGNING FIELD-SYMBOL(<lv_col_mod>).

*     get collector module
      DATA(lr_col) = zcl_nwhd_factory=>create_collector( <lv_col_mod> ).
      IF lr_col IS INITIAL.
        ev_step = |collector { <lv_col_mod> }|.
        RETURN.
      ELSE.
        DATA(lv_col_name) = lr_col->get_name( ).
      ENDIF.

*     prepare params
      CLEAR lt_param.
      LOOP AT ms_params-parameters ASSIGNING FIELD-SYMBOL(<ls_param>)
         WHERE collector = space OR collector = lv_col_name.
        APPEND INITIAL LINE TO lt_param ASSIGNING FIELD-SYMBOL(<ls_new_param>).
        MOVE-CORRESPONDING <ls_param> TO <ls_new_param>.
      ENDLOOP.

*     execute collector module
      IF lr_col->collect(
        EXPORTING
          it_param      = lt_param                  " table of param / value
          is_col_params = ls_col_param
      IMPORTING
        es_data    = ls_col_data                 " NWHD: Collector Data
        ev_step    = ev_step
      ) EQ abap_false.
        ev_step = |process collector { <lv_col_mod> } in step { ev_step }|.
        RETURN.
      ELSE.
        APPEND ls_col_data TO ms_data-collected.
      ENDIF.
    ENDLOOP.

    rv_success = abap_true.
  ENDMETHOD.


  METHOD zif_nwhd_job~process.

* ------ init
    IF init_job( is_params ) EQ abap_false.
      ev_step = 'init'.
      RETURN.
    ENDIF.

* ------ process
    IF process_job( ) EQ abap_false.
      ev_step = 'process'.
      RETURN.
    ENDIF.


* ------ close
    IF close_job( ) EQ abap_false.
      ev_step = 'close'.
      RETURN.
    ENDIF.

* ------ fill result
    es_result = ms_data.


* ------ publish?
    IF iv_publish EQ abap_true.
      IF zif_nwhd_job~publish(
          is_params  = is_params                 " NWHD: Job Parameter
          is_result  = es_result                 " NWHD: Job Data
      ) EQ abap_false.
        ev_step = 'publish'.
        RETURN.
      ENDIF.
    ENDIF.

* ------ final success
    rv_success = abap_true.


  ENDMETHOD.


  METHOD zif_nwhd_job~publish.
* ------- check publishers given
    IF is_params-publishers[] IS INITIAL.
      rv_success = abap_true.
      RETURN.
    ENDIF.

* ------- prepare
    DATA(lv_error) = abap_false.
    DATA(lv_lin)   = lines( is_params-publishers ).
    get_logger( )->trace( |{ lv_lin } publisher found| ).


* ------- loop all
    LOOP AT is_params-publishers ASSIGNING FIELD-SYMBOL(<lv_pub>).

      DATA(lr_pub) = zcl_nwhd_factory=>create_publisher( <lv_pub> ).
      lr_pub->set_logger( get_logger( ) ).


      IF lr_pub IS INITIAL.
        APPEND <lv_pub> TO et_publish_error.
        lv_error = abap_true.
      ELSE.
        IF lr_pub->publish(
             is_params = is_params
             is_result = is_result
           ) EQ abap_false.
          APPEND <lv_pub> TO et_publish_error.
          lv_error = abap_true.
          get_logger( )->error( |Publisher { <lv_pub> } failed| ).
        ELSE.
          APPEND <lv_pub> TO et_published.
          get_logger( )->info( |Publisher { <lv_pub> } processed| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

* -------- check
    rv_success = COND #( WHEN lv_error = abap_true
                         THEN abap_false
                         ELSE abap_true ).

* -------- protocol
    IF rv_success EQ abap_true.
      get_logger( )->info( |Job publish finished| ).
    ELSE.
      get_logger( )->warning( |Job publish finished with errors| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
