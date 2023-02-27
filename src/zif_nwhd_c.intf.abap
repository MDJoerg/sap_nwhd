INTERFACE zif_nwhd_c
  PUBLIC .


  CONSTANTS c_source_type TYPE znwhd_source_type VALUE 'SAPNWABAP' ##NO_TEXT.
  CONSTANTS version TYPE string VALUE '20230227' ##NO_TEXT.
  CONSTANTS release TYPE string VALUE '0.3.12' ##NO_TEXT.
  CONSTANTS github_repo TYPE string VALUE 'https://github.com/MDJoerg/sap_nwhd' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_detail_level,
      unspecified       TYPE znwhd_col_detail_level VALUE '0',
      very_important    TYPE znwhd_col_detail_level VALUE '1',
      important         TYPE znwhd_col_detail_level VALUE '2',
      medium_importance TYPE znwhd_col_detail_level VALUE '3',
      low_importance    TYPE znwhd_col_detail_level VALUE '4',
      lowest_importance TYPE znwhd_col_detail_level VALUE '5',
    END OF c_detail_level .

  CONSTANTS:
    BEGIN OF c_timeint_level,
      unspecified TYPE znwhd_col_detail_level VALUE '0',
      last_hour   TYPE znwhd_col_detail_level VALUE '1',
      last_24h    TYPE znwhd_col_detail_level VALUE '2',
      last_week   TYPE znwhd_col_detail_level VALUE '3',
      last_month  TYPE znwhd_col_detail_level VALUE '4',
      last_year   TYPE znwhd_col_detail_level VALUE '5',
    END OF c_timeint_level .

  CONSTANTS:
    BEGIN OF c_category,
      last_hour  TYPE znwhd_field_category VALUE 'LastHour',
      last_24h   TYPE znwhd_field_category VALUE 'Last24h',
      last_week  TYPE znwhd_field_category VALUE 'LastWeek',
      last_month TYPE znwhd_field_category VALUE 'LastMonth',
      last_year  TYPE znwhd_field_category VALUE 'LastYear',
      all        TYPE znwhd_field_category VALUE 'All',
    END OF c_category .
  CONSTANTS:
    BEGIN OF c_time_interval_type,
      none  TYPE znwhd_time_interval_type VALUE ' ',
      min   TYPE znwhd_time_interval_type VALUE '1',
      min5  TYPE znwhd_time_interval_type VALUE '5',
      min15 TYPE znwhd_time_interval_type VALUE 'Q',
      min30 TYPE znwhd_time_interval_type VALUE 'S',
      hour  TYPE znwhd_time_interval_type VALUE 'H',
      day   TYPE znwhd_time_interval_type VALUE 'D',
      week  TYPE znwhd_time_interval_type VALUE 'W',
      month TYPE znwhd_time_interval_type VALUE 'M',
      year  TYPE znwhd_time_interval_type VALUE 'Y',
    END OF c_time_interval_type .
ENDINTERFACE.
