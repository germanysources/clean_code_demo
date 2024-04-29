CLASS test_angebote_abgesagt DEFINITION DEFERRED.

CLASS stub_kopf_positionsdaten DEFINITION
  INHERITING FROM zangebote_abgesagt_stub_demo
  FRIENDS test_angebote_abgesagt.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

    " Ausgabeparameter enthaelt konstanten Wert
    METHODS get_angebotskopfdaten REDEFINITION.

    " Ausgabeparameter enthaelt konstanten Wert
    METHODS get_angebotspositionen REDEFINITION.

ENDCLASS.

CLASS stub_kopf_positionsdaten IMPLEMENTATION.

  METHOD constructor.
    DATA: bdt  TYPE zangebote_abgesagt_stub_demo=>_bestelldaten,
          kdt  TYPE zangebote_abgesagt_stub_demo=>_kunden,
          art  TYPE zangebote_abgesagt_stub_demo=>_artikel.

    " Dem constructor der Superklasse koennen leere Parameter uebergeben werden,
    " da die Selektionsoptionen nur fuer die Datenbankabfragen benoetigt
    " werden. Die Datenbankabfragen mocken wir (d.h. wir ersetzen diese)
    super->constructor( bestelldaten = bdt kunden = kdt artikel = art ).

  ENDMETHOD.

  METHOD get_angebotskopfdaten.

    " Jetzt ersetzen wir das Select-Statement,
    " um unabhaengig von den Datenbankinhalten zu sein
    kopfdaten = VALUE #(
    ( vbeln = '1' kunnr = '1' bstdk = '20190110' )
    ( vbeln = '2' kunnr = '1' bstdk = '20190111' )
    ( vbeln = '3' kunnr = '2' bstdk = '20190110' ) ).

  ENDMETHOD.

  METHOD get_angebotspositionen.

    " Jetzt ersetzen wir das Select-Statement,
    " um unabhaengig von den Datenbankinhalten zu sein
    positionsdaten = VALUE #(
    " nicht abgesagt
    ( vbeln = '1' matnr = 'A1' netpr = 1400 kpein = 10 kmein = 'ST' )
    " abgesagt
    ( vbeln = '1' matnr = 'A1' netpr = 1600 kpein = 10 kmein = 'ST' abgru = absage_grund_zu_teuer )
    " abgesagt
    ( vbeln = '2' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' abgru = absage_grund_zu_teuer )
    ( vbeln = '3' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' ) ).

  ENDMETHOD.

ENDCLASS.

* Testklasse Risikostufe harmlos, da in dieser Klasse keine
* persistenten Daten geaendert werden
CLASS test_angebote_abgesagt DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA under_test TYPE REF TO zangebote_abgesagt_stub_demo.

    METHODS setup.

    METHODS kumulation FOR TESTING
      RAISING
        zcx_angebot_abgesagt.

ENDCLASS.

" Die Friends-Beziehung erlaubt uns den Zugriff
" auf alle Methoden und Attribute (auch private)
CLASS zangebote_abgesagt_stub_demo DEFINITION LOCAL FRIENDS test_angebote_abgesagt.

CLASS test_angebote_abgesagt IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT under_test
      EXPORTING
        bestelldaten = VALUE zangebote_abgesagt_stub_demo=>_bestelldaten( )
        kunden       = VALUE zangebote_abgesagt_stub_demo=>_kunden( )
        artikel      = VALUE zangebote_abgesagt_stub_demo=>_artikel( ).

  ENDMETHOD.

  METHOD kumulation.
    DATA: act_sum_kunde_artikel TYPE zangebote_abgesagt_stub_demo=>summe_kunde_artikel,
          exp_sum_kunde_artikel TYPE zangebote_abgesagt_stub_demo=>summe_kunde_artikel,
          cut_with_stub TYPE REF TO stub_kopf_positionsdaten.

    " Die erwarteten Resultate
    exp_sum_kunde_artikel = VALUE #(
    ( kunnr = '1' monat = '201901' artikel = 'A1' anzahl_gesamt = 2
      kmein = 'ST' anzahl_abgesagt = 1 netto_preis_von = 1600
      netto_preis_bis = 1600 kpein_von = 10 kpein_bis = 10 )
    ( kunnr = '1' monat = '201901' artikel = 'A2' anzahl_gesamt = 1
      kmein = 'ST' anzahl_abgesagt = 1 netto_preis_von = 1000
      netto_preis_bis = 1000 kpein_von = 10 kpein_bis = 10 )
    ( kunnr = '2' monat = '201901' artikel = 'A2' anzahl_gesamt = 1
      kmein = 'ST' anzahl_abgesagt = 0 )  ).

    CREATE OBJECT cut_with_stub.
    cut_with_stub->angebote_kumulieren(
     IMPORTING sum_kunde_artikel = act_sum_kunde_artikel ).

    cl_abap_unit_assert=>assert_equals( exp = exp_sum_kunde_artikel
      act = act_sum_kunde_artikel msg = 'Kumulation fehlerhaft' ).

  ENDMETHOD.

ENDCLASS.
