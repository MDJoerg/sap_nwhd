interface ZIF_NWHD_C
  public .


  constants C_SOURCE_TYPE type ZNWHD_SOURCE_TYPE value 'SAPNWABAP' ##NO_TEXT.
  constants VERSION type STRING value '20230321' ##NO_TEXT.
  constants RELEASE type STRING value '0.4.0' ##NO_TEXT.
  constants GITHUB_REPO type STRING value 'https://github.com/MDJoerg/sap_nwhd' ##NO_TEXT.
  constants:
    BEGIN OF c_detail_level,
      unspecified       TYPE znwhd_col_detail_level VALUE '0',
      very_important    TYPE znwhd_col_detail_level VALUE '1',
      important         TYPE znwhd_col_detail_level VALUE '2',
      medium_importance TYPE znwhd_col_detail_level VALUE '3',
      low_importance    TYPE znwhd_col_detail_level VALUE '4',
      lowest_importance TYPE znwhd_col_detail_level VALUE '5',
    END OF c_detail_level .
  constants:
    BEGIN OF c_timeint_level,
      unspecified TYPE znwhd_col_detail_level VALUE '0',
      last_hour   TYPE znwhd_col_detail_level VALUE '1',
      last_24h    TYPE znwhd_col_detail_level VALUE '2',
      last_week   TYPE znwhd_col_detail_level VALUE '3',
      last_month  TYPE znwhd_col_detail_level VALUE '4',
      last_year   TYPE znwhd_col_detail_level VALUE '5',
    END OF c_timeint_level .
  constants:
    BEGIN OF c_category,
      last_hour  TYPE znwhd_field_category VALUE 'LastHour',
      last_24h   TYPE znwhd_field_category VALUE 'Last24h',
      last_week  TYPE znwhd_field_category VALUE 'LastWeek',
      last_month TYPE znwhd_field_category VALUE 'LastMonth',
      last_year  TYPE znwhd_field_category VALUE 'LastYear',
      all        TYPE znwhd_field_category VALUE 'All',
    END OF c_category .
  constants:
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
endinterface.
