*&---------------------------------------------------------------------*
*& Report  ZANGEBOTE_ABGESAGT
*& Auswertung abgesagte Angebote. Clean Code Demo.
*& Liest die Daten ueber den Extraktor in der Klasse zangebote_abgesagt
*& ein und stellt diese im ALV-Grid dar.
*&---------------------------------------------------------------------*
REPORT zangebote_abgesagt.

" Fuer Selektions-Optionen notwendig
DATA: bestelldatum        TYPE bstdk,
      kunde               TYPE kunnr,
      artikel             TYPE matnr,
      summe_kunde_artikel TYPE zangebote_abgesagt=>out_summe_kunde_artikel,
      extraktor           TYPE REF TO zangebote_abgesagt.
SELECT-OPTIONS: bstdk FOR bestelldatum,
  kunnr FOR kunde,
  matnr FOR artikel.
PARAMETERS: layout TYPE slis_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR layout.
  DATA: variant TYPE disvariant.
  variant-report = sy-repid.
  variant-variant = layout.
  CALL FUNCTION 'LVC_VARIANT_SAVE_LOAD'
    EXPORTING
      i_save_load = 'F'
      i_tabname   = '1'
    CHANGING
      cs_variant  = variant
    EXCEPTIONS
      OTHERS      = 1.
  IF sy-subrc = 0.
    layout = variant-variant.
  ENDIF.

START-OF-SELECTION.
  DATA: variant        TYPE disvariant,
        structure_name TYPE tabname.

  CREATE OBJECT extraktor
    EXPORTING
      bestelldaten = bstdk[]
      kunden       = kunnr[]
      artikel      = matnr[].

  TRY.
      extraktor->get_angebote( IMPORTING summe_kunde_artikel =
        summe_kunde_artikel ).

      PERFORM get_structure_name CHANGING structure_name.
      variant-variant = layout.
      variant-report = sy-repid.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_save                   = 'A'
          is_variant               = variant
          i_structure_name         = structure_name
          i_callback_program       = variant-report
          i_callback_pf_status_set = 'SET_PF_STATUS'
          i_callback_user_command  = 'USER_COMMAND'
        TABLES
          t_outtab                 = summe_kunde_artikel.
    CATCH zcx_angebot_abgesagt INTO DATA(not_found).
      MESSAGE not_found TYPE 'S'.
  ENDTRY.

  " Strukturname der internen Tabelle <summe_kunde_artikel> lesen
  " aus den Runtime-Services
FORM get_structure_name CHANGING name TYPE tabname.
  DATA: table_descr TYPE REF TO cl_abap_tabledescr.

  table_descr ?= cl_abap_tabledescr=>describe_by_data( summe_kunde_artikel ).
  name = table_descr->get_table_line_type( )->get_relative_name( ).

ENDFORM.

##CALLED
FORM set_pf_status USING exclude TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING exclude.

ENDFORM.

##CALLED
FORM user_command USING command TYPE syst-ucomm
      ##NEEDED selfield TYPE slis_selfield.

  CASE command.
    WHEN 'LOG'.
      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXCEPTIONS
          no_data_available = 4.
      IF sy-subrc <> 0.
        MESSAGE s002(zangebote_abgesagt).
      ENDIF.
    WHEN 'BACK'.
      " Destructor rufen
      extraktor->teardown( ).
    WHEN 'EXIT'.
      extraktor->teardown( ).
  ENDCASE.

ENDFORM.
