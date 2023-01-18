*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTC_NWHDCFG_FDN.................................*
DATA:  BEGIN OF STATUS_ZTC_NWHDCFG_FDN               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTC_NWHDCFG_FDN               .
CONTROLS: TCTRL_ZTC_NWHDCFG_FDN
            TYPE TABLEVIEW USING SCREEN '2010'.
*...processing: ZTD_NWHDLDB_SRC.................................*
DATA:  BEGIN OF STATUS_ZTD_NWHDLDB_SRC               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTD_NWHDLDB_SRC               .
CONTROLS: TCTRL_ZTD_NWHDLDB_SRC
            TYPE TABLEVIEW USING SCREEN '2000'.
*.........table declarations:.................................*
TABLES: *ZTC_NWHDCFG_FDN               .
TABLES: *ZTD_NWHDLDB_SRC               .
TABLES: ZTC_NWHDCFG_FDN                .
TABLES: ZTD_NWHDLDB_SRC                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
