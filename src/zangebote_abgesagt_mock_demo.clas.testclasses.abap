
" Initialisierung <summe> fuellen
DEFINE initialisierung.
  CLEAR: summe, position.
  summe-netto_preis_von = 100. summe-netto_preis_bis = 200.
  summe-kpein_von = 1. summe-kpein_bis = 1.
END-OF-DEFINITION.

DEFINE get_mockup_loader.
  " @todo
  " Nach dem Import mit abapgit die Parameter in der create Methode anpassen.
  " i_path hier ersetzen wir /your/path durch unseren Pfad
  " i_encoding 4110, falls wir die Textdateien aus der Vorlage verwenden.
  " Exportieren wir die Microsoft Excel Dateien als Unicode-Text braucht keine Codierung angegeben werden.
  DATA(mockup) = zcl_mockup_loader=>create(
  i_path = '/your/path' i_type = 'FILE'
  i_encoding = '4110' i_begin_comment = '*' ).
END-OF-DEFINITION.

* Testklasse Risikostufe Harmlos, da in dieser Klasse keine
* persistenten Daten geaendert werden
CLASS test_angebote_abgesagt DEFINITION FOR TESTING
  INHERITING FROM zangebote_abgesagt_mock_demo
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

    " Datenbankaufruf mocken
    " Um die Methode kurz zu halten, wurde der value Operator benutzt
    " Dieser Operator steht ab Release 740 zur Verfuegung
    METHODS get_angebotskopfdaten REDEFINITION.

    " Datenbankaufruf mocken
    " Um die Methode kurz zu halten, wurde der value Operator benutzt
    " Dieser Operator steht ab Release 740 zur Verfuegung
    METHODS get_angebotspositionen REDEFINITION.

  PRIVATE SECTION.
    " Die privaten Komponenten werden hiermit getestet
    DATA under_test TYPE REF TO zangebote_abgesagt_mock_demo.

    " Objekt under_test erzeugen, um Datenbankabfragen zu testen
    METHODS setup.

    " offenes Angebot (nicht abgesagt) zur Summe hinzufuegen
    METHODS tadd_ang_to_summe FOR TESTING.

    " abgesagtes Angebot zur Summe hinzufuegen
    METHODS add_asg_ang_to_summe FOR TESTING.

    " Kumulation. Um die Methode kurz zu halten, wurde der value Operator benutzt
    " Dieser Operator steht ab Release 740 zur Verfuegung
    METHODS kumulation FOR TESTING
      RAISING
        zcx_angebot_abgesagt.

    " Kopfdaten der Angebote lesen
    " Benutzt die Original-Klasse zangebote_abgesagt_mock_demo
    " Nutzt den Mockup-Loader Version 2.0.2 https://github.com/sbcgua/mockup_loader
    METHODS kopfdaten FOR TESTING
      RAISING cx_static_check.

    " Positionsdaten der Angebote lesen
    " Benutzt die Original-Klasse zangebote_abgesagt_mock_demo
    " Nutzt den Mockup-Loader Version 2.0.2 https://github.com/sbcgua/mockup_loader
    METHODS positionsdaten FOR TESTING
      RAISING cx_static_check.

    METHODS tadd_log_message FOR TESTING.

ENDCLASS.

" Die Friends-Beziehung erlaubt uns den Zugriff
" auf alle Methoden und Attribute (auch private)
CLASS zangebote_abgesagt_mock_demo DEFINITION LOCAL FRIENDS test_angebote_abgesagt.

CLASS test_angebote_abgesagt IMPLEMENTATION.

  METHOD constructor.
    DATA: bdt  TYPE zangebote_abgesagt_mock_demo=>_bestelldaten,
          kdt  TYPE zangebote_abgesagt_mock_demo=>_kunden,
          art  TYPE zangebote_abgesagt_mock_demo=>_artikel.

    " Dem constructor der Superklasse koennen leere Parameter uebergeben werden,
    " da die Selektionsoptionen nur fuer die Datenbankabfragen benoetigt
    " werden. Die Datenbankabfragen mocken wir (d.h. wir ersetzen diese)
    super->constructor( bestelldaten = bdt kunden = kdt artikel = art ).

  ENDMETHOD.

  METHOD get_angebotskopfdaten.

    " Jetzt ersetzen wir das Select-Statement
    " um unabhaengig von den Datenbankinhalten zu sein
    kopfdaten = VALUE #(
    ( vbeln = '1' kunnr = '1' bstdk = '20190110' )
    ( vbeln = '2' kunnr = '1' bstdk = '20190111' )
    ( vbeln = '3' kunnr = '2' bstdk = '20190110' ) ).

  ENDMETHOD.

  METHOD get_angebotspositionen.

    " Jetzt ersetzen wir das Select-Statement
    " um unabhaengig von den Datenbankinhalten zu sein
    positionsdaten = VALUE #(
    " nicht abgesagt
    ( vbeln = '1' matnr = 'A1' netpr = 1400 kpein = 10 kmein = 'ST' )
    " abgesagt
    ( vbeln = '1' matnr = 'A1' netpr = 1600 kpein = 10 kmein = 'ST' abgru = zangebote_abgesagt_mock_demo=>absage_grund_zu_teuer )
    " abgesagt
    ( vbeln = '2' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' abgru = zangebote_abgesagt_mock_demo=>absage_grund_zu_teuer )
    ( vbeln = '3' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' ) ).

  ENDMETHOD.

  METHOD setup.
    DATA: bdt  TYPE zangebote_abgesagt_mock_demo=>_bestelldaten,
          _bdt LIKE LINE OF bdt,
          kdt  TYPE zangebote_abgesagt_mock_demo=>_kunden,
          _kdt LIKE LINE OF kdt,
          art  TYPE zangebote_abgesagt_mock_demo=>_artikel.

    " Die Selektionsoptionen muessen wir entsprechend
    " unseren vorhandenen Angeboten anpassen.
    " Diese sind fuer die Datenbankabfragen relevant
    " @todo nach dem Import mit abapgit die Selektionsoptionen entsprechend anpassen.
    _bdt-sign = 'I'. _bdt-option = 'BT'. _bdt-low = '20190101'. _bdt-high = '20190131'.
    APPEND _bdt TO bdt.
    _kdt-sign = 'I'. _kdt-option = 'EQ'. _kdt-low = ''.
    APPEND _kdt TO kdt.

    CREATE OBJECT under_test
      EXPORTING
        bestelldaten = bdt
        kunden       = kdt
        artikel      = art.

  ENDMETHOD.

  METHOD tadd_ang_to_summe.
    DATA: summe     TYPE zangebot_summe_kunde_artikel,
          position  TYPE zangebots_position,
          exp_summe TYPE zangebot_summe_kunde_artikel.

    " Testfaelle
    " Angebot nicht abgesagt
    initialisierung.
    position-netpr = 300.
    position-kpein = 1.
    exp_summe = summe.
    exp_summe-anzahl_gesamt = 1.
    under_test->add_ang_to_summe( EXPORTING position = position
      CHANGING summe = summe ).
    cl_abap_unit_assert=>assert_equals( exp = exp_summe act = summe
      msg = 'Summe fuer nicht abgesagtes Angebot nicht korrekt' ).

  ENDMETHOD.

  METHOD add_asg_ang_to_summe.
    DATA: summe     TYPE zangebot_summe_kunde_artikel,
          position  TYPE zangebots_position,
          exp_summe TYPE zangebot_summe_kunde_artikel.

    " Testfaelle
    " Angebot abgesagt
    "   Preis diese Position < Preis von
    initialisierung.
    position-netpr = 800.
    position-kpein = 10.
    position-abgru = zangebote_abgesagt_mock_demo=>absage_grund_zu_teuer.
    exp_summe = summe.
    exp_summe-netto_preis_von = 800.
    exp_summe-kpein_von = 10.
    exp_summe-anzahl_gesamt = 1.
    exp_summe-anzahl_abgesagt = 1.
    under_test->add_ang_to_summe( EXPORTING position = position
     CHANGING summe = summe ).
    cl_abap_unit_assert=>assert_equals( exp = exp_summe act = summe
      msg = 'Summe fuer abgesagtes Angebot nicht korrekt' ).

    "   Preis diese Position >= Preis von und Preis diese Position <= Preis bis
    initialisierung.
    position-netpr = 120.
    position-kpein = 1.
    position-abgru = zangebote_abgesagt_mock_demo=>absage_grund_zu_teuer.
    exp_summe = summe.
    exp_summe-anzahl_gesamt = 1.
    exp_summe-anzahl_abgesagt = 1.
    under_test->add_ang_to_summe( EXPORTING position = position
     CHANGING summe = summe ).
    cl_abap_unit_assert=>assert_equals( exp = exp_summe act = summe
      msg = 'Summe fuer abgesagtes Angebot nicht korrekt' ).

    "   Preis diese Position > Preis bis
    initialisierung.
    position-netpr = 300.
    position-kpein = 1.
    position-abgru = zangebote_abgesagt_mock_demo=>absage_grund_zu_teuer.
    exp_summe = summe.
    exp_summe-netto_preis_bis = 300.
    exp_summe-anzahl_gesamt = 1.
    exp_summe-anzahl_abgesagt = 1.
    under_test->add_ang_to_summe( EXPORTING position = position
     CHANGING summe = summe ).
    cl_abap_unit_assert=>assert_equals( exp = exp_summe act = summe
      msg = 'Summe fuer abgesagtes Angebot nicht korrekt' ).

  ENDMETHOD.

  METHOD kumulation.
    DATA: kopfdaten             TYPE zangebots_kopfdaten,
          positionen            TYPE zangebots_positionsdaten,
          act_sum_kunde_artikel TYPE zangebote_abgesagt_mock_demo=>summe_kunde_artikel,
          exp_sum_kunde_artikel TYPE zangebote_abgesagt_mock_demo=>summe_kunde_artikel.

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

    " Jetzt ausfuehren Methode angebote_kumulieren (gibt uns die tatsaechlichen Daten)
    angebote_kumulieren(
     IMPORTING sum_kunde_artikel = act_sum_kunde_artikel ).

    " Jetzt Vergleich tatsaechliche und erwartete Daten
    cl_abap_unit_assert=>assert_equals( exp = exp_sum_kunde_artikel
      act = act_sum_kunde_artikel msg = 'Kumulation fehlerhaft' ).

  ENDMETHOD.

  METHOD kopfdaten.
    DATA: exp_auschluss  TYPE zangebots_kopf,
          exp_einschluss TYPE zangebots_kopfdaten,
          act_kopfdaten  TYPE zangebots_kopfdaten.
    FIELD-SYMBOLS: <exp> TYPE zangebots_kopf.

    get_mockup_loader.

    " Die erwarteten Werte werden aus den Dateien auftrag_ausschluss und angebote_einschluss gelesen.
    " Hier gibt es einmal die Auftraege, die ausgeschlossen werden muessen,
    " und einmal die Angebote, die auf Einschluss geprueft werden muessen.
    mockup->load_data( EXPORTING i_obj = 'auftrag_auschluss'
      IMPORTING e_container = exp_auschluss ).
    mockup->load_data( EXPORTING i_obj = 'angebote_einschluss'
      IMPORTING e_container = exp_einschluss ).

    under_test->get_angebotskopfdaten( IMPORTING kopfdaten = act_kopfdaten ).

    " Den Auftrag auf Auschluss pruefen
    cl_abap_unit_assert=>assert_table_not_contains(
      line = exp_auschluss table = act_kopfdaten
      msg = 'Auftrag ist enthalten' ).

    " Die Angebote auf Einschluss pruefen
    LOOP AT exp_einschluss ASSIGNING <exp>.
      cl_abap_unit_assert=>assert_table_contains(
        line = <exp> table = act_kopfdaten
        msg = 'Angebot ist nicht enthalten' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD positionsdaten.
    DATA: act_positionen TYPE zangebots_positionsdaten,
          exp_positionen LIKE act_positionen,
          angebote       TYPE zangebots_kopfdaten.

    get_mockup_loader.

    " Die Angebote, deren Positionen wir lesen wollen
    mockup->load_data( EXPORTING i_obj = 'angebote' i_strict = abap_false
     IMPORTING e_container = angebote ).
    mockup->load_data( EXPORTING i_obj = 'angebote_positionen'
      IMPORTING e_container = exp_positionen ).

    under_test->get_angebotspositionen( EXPORTING angebots_nummern = angebote
      IMPORTING positionsdaten = act_positionen ).

    " Sortierung, damit Vergleich nicht fehlschlaegt
    SORT: act_positionen BY vbeln posnr,
      exp_positionen BY vbeln posnr.

    " Vergleich erwartete und tatsaechliche Positionen
    cl_abap_unit_assert=>assert_equals( exp = exp_positionen
      act = act_positionen msg = 'Positionen stimmen nicht' ).

  ENDMETHOD.

  METHOD tadd_log_message.
    DATA:
      ##NEEDED
      mtext    TYPE string,
      position TYPE zangebots_position.

    " Hier testen wir nur darauf, dass keine Ausnahmen auftreten
    MESSAGE s000(zangebote_abgesagt) INTO mtext.
    position-vbeln = '1'.
    position-posnr = '10'.
    under_test->add_log_message( position ).

  ENDMETHOD.

ENDCLASS.
