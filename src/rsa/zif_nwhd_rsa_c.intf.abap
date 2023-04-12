INTERFACE zif_nwhd_rsa_c
  PUBLIC .


  CONSTANTS c_param_source    TYPE string VALUE 'source' ##NO_TEXT.
  CONSTANTS c_param_collector TYPE string VALUE 'collector' ##NO_TEXT.
  CONSTANTS c_param_category  TYPE string VALUE 'category' ##NO_TEXT.
  CONSTANTS c_param_field     TYPE string VALUE 'field' ##NO_TEXT.

  CONSTANTS c_param_date_from TYPE string VALUE 'from_date' ##NO_TEXT.
  CONSTANTS c_param_date_to   TYPE string VALUE 'to_date' ##NO_TEXT.
  CONSTANTS c_param_max_rows  TYPE string VALUE 'max_rows' ##NO_TEXT.


  CONSTANTS c_default_date_interval     TYPE i VALUE 90.
  CONSTANTS c_default_max_rows          TYPE i VALUE 10000.


ENDINTERFACE.
