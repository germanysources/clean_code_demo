*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 02.06.2019 at 11:49:50
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZANGEBOT_MATERIA................................*
DATA:  BEGIN OF STATUS_ZANGEBOT_MATERIA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZANGEBOT_MATERIA              .
CONTROLS: TCTRL_ZANGEBOT_MATERIA
            TYPE TABLEVIEW USING SCREEN '0003'.
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
TABLES: *ZANGEBOT_MATERIA              .
TABLES: *ZANGEBOT_VBAK                 .
TABLES: *ZANGEBOT_VBAP                 .
TABLES: ZANGEBOT_MATERIA               .
TABLES: ZANGEBOT_VBAK                  .
TABLES: ZANGEBOT_VBAP                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
