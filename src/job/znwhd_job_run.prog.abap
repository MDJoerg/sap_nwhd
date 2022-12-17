*&---------------------------------------------------------------------*
*& Report ZNWHD_COL_RUN
*&---------------------------------------------------------------------*
*& Starts a collecting job and triggers the publisher modules
*&---------------------------------------------------------------------*
REPORT znwhd_job_run NO STANDARD PAGE HEADING.
TABLES: znwhd_s_sel_job.
PARAMETERS: p_job       LIKE znwhd_s_sel_job-job DEFAULT 'ZCL_NWHD_JOB' OBLIGATORY.
SELECTION-SCREEN: ULINE.
SELECT-OPTIONS: so_col  FOR znwhd_s_sel_job-collector NO INTERVALS OBLIGATORY.
SELECT-OPTIONS: so_pub  FOR znwhd_s_sel_job-publisher NO INTERVALS.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_srcid     LIKE znwhd_s_sel_job-source_id.
PARAMETERS: p_rfc       LIKE znwhd_s_sel_job-rfc_dest.
PARAMETERS: p_qnam      LIKE znwhd_s_sel_job-qname.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_disp AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_prot AS CHECKBOX DEFAULT 'X'.


INITIALIZATION.
* ----- get all collectors
  IF so_col[] IS INITIAL.
    DATA(lt_col) = zcl_nwhd_factory=>create_util( )->get_all_mod_col( ).
    LOOP AT lt_col ASSIGNING FIELD-SYMBOL(<lv_col>).
      so_col-sign   = 'I'.
      so_col-option = 'EQ'.
      so_col-low    = <lv_col>.
      APPEND so_col.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.

* ------- local data
  DATA ls_data TYPE znwhd_s_data_job.
  DATA lv_step TYPE string.


* ------- prepare params
  DATA(ls_params) = VALUE znwhd_s_param_job(
    rfc_dest  = p_rfc
    qname     = p_qnam
    source_id	= p_srcid
  ).

  LOOP AT so_col.
    APPEND so_col-low TO ls_params-collectors.
  ENDLOOP.

  LOOP AT so_pub.
    APPEND so_pub-low TO ls_params-publishers.
  ENDLOOP.


* ------- init collector
  DATA(lr_job) = zcl_nwhd_factory=>create_job( p_job ).
  IF lr_job IS INITIAL.
    WRITE: / 'invalid job:', p_job.
  ELSE.
* ------- collect
    IF lr_job->process(
    EXPORTING
      is_params  = ls_params
    IMPORTING
     es_result  = ls_data
     ev_step    = lv_step
     ) EQ abap_false.
      WRITE: / 'Error while calling job', p_job, 'in step', lv_step.
    ELSE.
* ------- output
      IF p_disp EQ abap_true.
        PERFORM display_result.
      ENDIF.

* ------- trace
      IF p_prot EQ abap_true.
        DATA(lt_msg) = lr_job->get_logger( )->get_msg_tab( ).
        SKIP.
        ULINE.
        LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
          WRITE: / <ls_msg>-type, <ls_msg>-message.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form display_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_result .
  DATA(lv_lin) = lines( ls_data-collected ).
  DATA(lv_tag) = lines( ls_data-tags ).

  WRITE: / 'Source     :', ls_data-source_type, ls_data-source_id.
  WRITE: / 'Source Ref :', ls_data-source_ref.
  WRITE: / 'Started at :', ls_data-started_at.
  WRITE: / 'Finished at:', ls_data-finished_at.
  WRITE: / 'Collected  :', lv_lin.
  WRITE: / 'Tags       :', lv_tag.
  SKIP.

  DATA(lv_col)    = ||.
  DATA(lv_cat)    = ||.

  IF lv_tag > 0.
    WRITE: / 'Tags:'.
    LOOP AT ls_data-tags ASSIGNING FIELD-SYMBOL(<ls_tag>).
      WRITE: AT /5 <ls_tag>-tag.
      WRITE AT 60 <ls_tag>-value.
    ENDLOOP.
  ENDIF.

  LOOP AT ls_data-collected ASSIGNING FIELD-SYMBOL(<ls_col>).

    DATA(lt_fields) = <ls_col>-fields.
    SORT lt_fields BY category key.

    IF lv_col NE <ls_col>-collector.
      SKIP.
      WRITE: / 'Collector:',
               <ls_col>-collector.
      lv_col = <ls_col>-collector.
    ENDIF.


    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).

      IF lv_cat NE <ls_field>-category.
        SKIP.
        WRITE: AT /5 'Category:',
                 <ls_field>-category.
        lv_cat = <ls_field>-category.
      ENDIF.

      WRITE: AT /10 <ls_field>-key.
      IF <ls_field>-value_text IS NOT INITIAL.
        WRITE: AT 60 <ls_field>-value_text.
      ELSE.
        WRITE: AT 60 <ls_field>-value_number.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form publish_result
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM publish_result .

  LOOP AT so_pub ASSIGNING FIELD-SYMBOL(<ls_pub>).
    SKIP.
    WRITE: / 'Publisher:', <ls_pub>-low.

    SKIP.
  ENDLOOP.
ENDFORM.
