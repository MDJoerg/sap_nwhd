*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTD_NWHDLDB_SRC.................................*
DATA:  BEGIN OF STATUS_ZTD_NWHDLDB_SRC               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTD_NWHDLDB_SRC               .
CONTROLS: TCTRL_ZTD_NWHDLDB_SRC
            TYPE TABLEVIEW USING SCREEN '2000'.
*.........table declarations:.................................*
TABLES: *ZTD_NWHDLDB_SRC               .
TABLES: ZTD_NWHDLDB_SRC                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
