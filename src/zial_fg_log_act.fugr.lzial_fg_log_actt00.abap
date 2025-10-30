*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIAL_V_LOG_ACT..................................*
TABLES: ZIAL_V_LOG_ACT, *ZIAL_V_LOG_ACT. "view work areas
CONTROLS: TCTRL_ZIAL_V_LOG_ACT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZIAL_V_LOG_ACT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZIAL_V_LOG_ACT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZIAL_V_LOG_ACT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZIAL_V_LOG_ACT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZIAL_V_LOG_ACT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZIAL_V_LOG_ACT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZIAL_V_LOG_ACT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZIAL_V_LOG_ACT_TOTAL.

*.........table declarations:.................................*
TABLES: BALOBJ                         .
TABLES: BALOBJT                        .
TABLES: BALSUB                         .
TABLES: BALSUBT                        .
TABLES: ZIAL_T_LOG_ACT                 .
