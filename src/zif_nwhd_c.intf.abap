interface ZIF_NWHD_C
  public .


  constants C_SOURCE_TYPE type ZNWHD_SOURCE_TYPE value 'SAPNWABAP' ##NO_TEXT.
  constants VERSION type STRING value '20230118' ##NO_TEXT.
  constants RELEASE type STRING value '0.3.7' ##NO_TEXT.
  constants GITHUB_REPO type STRING value 'https://github.com/MDJoerg/sap_nwhd' ##NO_TEXT.
  constants:
    BEGIN OF c_detail_level,
               unspecified TYPE znwhd_col_detail_level VALUE '0',
               last_hour   TYPE znwhd_col_detail_level VALUE '1',
               last_24h    TYPE znwhd_col_detail_level VALUE '2',
               last_week   TYPE znwhd_col_detail_level VALUE '3',
               last_month  TYPE znwhd_col_detail_level VALUE '4',
               last_year   TYPE znwhd_col_detail_level VALUE '5',
             END OF c_detail_level .
  constants:
    begin of c_category,
              last_hour    type ZNWHD_FIELD_CATEGORY value 'LastHour',
              last_24h     type ZNWHD_FIELD_CATEGORY value 'Last24h',
              last_week    type ZNWHD_FIELD_CATEGORY value 'LastWeek',
              last_month   type ZNWHD_FIELD_CATEGORY value 'LastMonth',
              last_year    type ZNWHD_FIELD_CATEGORY value 'LastYear',
              all          type ZNWHD_FIELD_CATEGORY value 'All',
             end of c_category .
endinterface.
