*&---------------------------------------------------------------------*
*& Report ZNWHD_COL_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT znwhd_col_test NO STANDARD PAGE HEADING.
TABLES: znwhd_s_sel_job.
PARAMETERS: p_col LIKE znwhd_s_sel_job-collector DEFAULT 'ZCL_NWHD_COL_DUMPS' OBLIGATORY.


START-OF-SELECTION.

* ------- local data
  DATA ls_data TYPE znwhd_s_data_col.
  DATA lv_step TYPE string.


* ------- init collector
  DATA(lr_col) = zcl_nwhd_factory=>create_collector( p_col ).
  IF lr_col IS INITIAL.
    WRITE: / 'invalid collector:', p_col.
  ELSE.
* ------- collect
    IF lr_col->collect(
      IMPORTING
        es_data    = ls_data
        ev_step    = lv_step
    ) EQ abap_false.
      WRITE: / 'Error while calling collector', p_col, 'in step', lv_step.
    ELSE.
* ------- output
      DATA(lv_lin) = lines( ls_data-fields ).
      WRITE: / 'Collector  :', ls_data-collector.
      WRITE: / 'Started at :', ls_data-started_at.
      WRITE: / 'Finished at:', ls_data-finished_at.
      WRITE: / 'Data count :', lv_lin.
      SKIP.

      DATA(lv_cat)    = ||.
      DATA(lt_fields) = ls_data-fields.
      SORT lt_fields BY category key.

      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).

        IF lv_cat NE <ls_field>-category.
          SKIP.
          WRITE: / 'Category:',
                   <ls_field>-category.
          lv_cat = <ls_field>-category.
        ENDIF.

        WRITE: / <ls_field>-key.
        IF <ls_field>-value_text IS NOT INITIAL.
          WRITE: AT 60 <ls_field>-value_text.
        ELSE.
          WRITE: AT 60 <ls_field>-value_number.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
