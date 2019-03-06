*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.03.2019 at 12:30:28
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZANGEBOT_VBAK...................................*
DATA:  BEGIN OF STATUS_ZANGEBOT_VBAK                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZANGEBOT_VBAK                 .
CONTROLS: TCTRL_ZANGEBOT_VBAK
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZANGEBOT_VBAP...................................*
DATA:  BEGIN OF STATUS_ZANGEBOT_VBAP                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZANGEBOT_VBAP                 .
CONTROLS: TCTRL_ZANGEBOT_VBAP
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZANGEBOT_VBAK                 .
TABLES: *ZANGEBOT_VBAP                 .
TABLES: ZANGEBOT_VBAK                  .
TABLES: ZANGEBOT_VBAP                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
