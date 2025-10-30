*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIAL_T_LOG_CNF..................................*
DATA:  BEGIN OF STATUS_ZIAL_T_LOG_CNF                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIAL_T_LOG_CNF                .
*.........table declarations:.................................*
TABLES: *ZIAL_T_LOG_CNF                .
TABLES: ZIAL_T_LOG_CNF                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
