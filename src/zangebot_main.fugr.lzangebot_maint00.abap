*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 27.08.2019 at 17:52:17
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZANGEBOT_KUNDEN.................................*
DATA:  BEGIN OF STATUS_ZANGEBOT_KUNDEN               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZANGEBOT_KUNDEN               .
CONTROLS: TCTRL_ZANGEBOT_KUNDEN
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZANGEBOT_MATERIA................................*
DATA:  BEGIN OF STATUS_ZANGEBOT_MATERIA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZANGEBOT_MATERIA              .
CONTROLS: TCTRL_ZANGEBOT_MATERIA
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZANGEBOT_MATTEXT................................*
DATA:  BEGIN OF STATUS_ZANGEBOT_MATTEXT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZANGEBOT_MATTEXT              .
CONTROLS: TCTRL_ZANGEBOT_MATTEXT
            TYPE TABLEVIEW USING SCREEN '0005'.
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
TABLES: *ZANGEBOT_KUNDEN               .
TABLES: *ZANGEBOT_MATERIA              .
TABLES: *ZANGEBOT_MATTEXT              .
TABLES: *ZANGEBOT_VBAK                 .
TABLES: *ZANGEBOT_VBAP                 .
TABLES: ZANGEBOT_KUNDEN                .
TABLES: ZANGEBOT_MATERIA               .
TABLES: ZANGEBOT_MATTEXT               .
TABLES: ZANGEBOT_VBAK                  .
TABLES: ZANGEBOT_VBAP                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
