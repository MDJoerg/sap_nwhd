INTERFACE zif_nwhd_c
  PUBLIC .


  CONSTANTS c_source_type TYPE znwhd_source_type VALUE 'SAPNWABAP' ##NO_TEXT.
  CONSTANTS version TYPE string VALUE '20221220' ##NO_TEXT.
  CONSTANTS release TYPE string VALUE '0.2.1' ##NO_TEXT.
  CONSTANTS github_repo TYPE string VALUE 'https://github.com/MDJoerg/sap_nwhd' ##NO_TEXT.

  CONSTANTS: BEGIN OF c_detail_level,
               unspecified TYPE znwhd_col_detail_level VALUE '0',
               last_hour   TYPE znwhd_col_detail_level VALUE '1',
               last_24h    TYPE znwhd_col_detail_level VALUE '2',
               last_week   TYPE znwhd_col_detail_level VALUE '3',
               last_month  TYPE znwhd_col_detail_level VALUE '4',
               last_year   TYPE znwhd_col_detail_level VALUE '5',
             END OF c_detail_level.

  CONSTANTS: begin of c_category,
              last_hour    type ZNWHD_FIELD_CATEGORY value 'LastHour',
              last_24h     type ZNWHD_FIELD_CATEGORY value 'Last24h',
              last_week    type ZNWHD_FIELD_CATEGORY value 'LastWeek',
              last_month   type ZNWHD_FIELD_CATEGORY value 'LastMonth',
              last_year    type ZNWHD_FIELD_CATEGORY value 'LastYear',
              all          type ZNWHD_FIELD_CATEGORY value 'All',
             end of c_category.

ENDINTERFACE.
