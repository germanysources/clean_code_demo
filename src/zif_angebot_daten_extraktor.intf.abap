INTERFACE zif_angebot_daten_extraktor
  PUBLIC .

  METHODS get_angebotskopfdaten
    EXPORTING
      !kopfdaten TYPE zangebots_kopfdaten
    RAISING
      zcx_angebot_abgesagt .
  METHODS get_angebotspositionen
    IMPORTING
      !angebots_nummern TYPE zangebots_kopfdaten
    EXPORTING
      !positionsdaten   TYPE zangebots_positionsdaten .

ENDINTERFACE.
