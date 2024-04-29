CLASS test_angebote_abgesagt DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA under_test TYPE REF TO zangebote_abgesagt_stub_demo1.

    METHODS setup
      RAISING
        zcx_angebot_abgesagt.

    METHODS prepare_stub
      RETURNING
        VALUE(result) TYPE REF TO zif_angebot_daten_extraktor
      RAISING
        zcx_angebot_abgesagt.

    METHODS kumulation FOR TESTING
      RAISING
        zcx_angebot_abgesagt.

ENDCLASS.

" Die Friends-Beziehung erlaubt uns den Zugriff
" auf alle Methoden und Attribute (auch private)
CLASS zangebote_abgesagt_stub_demo1 DEFINITION LOCAL FRIENDS test_angebote_abgesagt.

CLASS test_angebote_abgesagt IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT under_test
      EXPORTING
        bestelldaten = VALUE zangebote_abgesagt_stub_demo1=>_bestelldaten( )
        kunden       = VALUE zangebote_abgesagt_stub_demo1=>_kunden( )
        artikel      = VALUE zangebote_abgesagt_stub_demo1=>_artikel( )
        daten_extraktor = prepare_stub( ).

  ENDMETHOD.

  METHOD prepare_stub.

    result = CAST zif_angebot_daten_extraktor(
      cl_abap_testdouble=>create( 'ZIF_ANGEBOT_DATEN_EXTRAKTOR' ) ).
    DATA(kopfdaten) = VALUE zangebots_kopfdaten(
      ( vbeln = '1' kunnr = '1' bstdk = '20190110' )
      ( vbeln = '2' kunnr = '1' bstdk = '20190111' )
      ( vbeln = '3' kunnr = '2' bstdk = '20190110' ) ).
    cl_abap_testdouble=>configure_call(
      result )->set_parameter( name = 'KOPFDATEN' value = kopfdaten ).
    result->get_angebotskopfdaten( ).

    DATA(positionsdaten) = VALUE zangebots_positionsdaten(
      ( vbeln = '1' matnr = 'A1' netpr = 1400 kpein = 10 kmein = 'ST' )
      ( vbeln = '1' matnr = 'A1' netpr = 1600 kpein = 10 kmein = 'ST' abgru = '02' )
      ( vbeln = '2' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' abgru = '02' )
      ( vbeln = '3' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' ) ).
    cl_abap_testdouble=>configure_call(
      result )->set_parameter( name = 'POSITIONSDATEN' value = positionsdaten ).
    result->get_angebotspositionen(
      EXPORTING
        angebots_nummern = kopfdaten ).

  ENDMETHOD.

  METHOD kumulation.
    DATA: act_sum_kunde_artikel TYPE zangebote_abgesagt_stub_demo1=>summe_kunde_artikel,
          exp_sum_kunde_artikel TYPE zangebote_abgesagt_stub_demo1=>summe_kunde_artikel.

    exp_sum_kunde_artikel = VALUE #(
    ( kunnr = '1' monat = '201901' artikel = 'A1' anzahl_gesamt = 2
      kmein = 'ST' anzahl_abgesagt = 1 netto_preis_von = 1600
      netto_preis_bis = 1600 kpein_von = 10 kpein_bis = 10 )
    ( kunnr = '1' monat = '201901' artikel = 'A2' anzahl_gesamt = 1
      kmein = 'ST' anzahl_abgesagt = 1 netto_preis_von = 1000
      netto_preis_bis = 1000 kpein_von = 10 kpein_bis = 10 )
    ( kunnr = '2' monat = '201901' artikel = 'A2' anzahl_gesamt = 1
      kmein = 'ST' anzahl_abgesagt = 0 )  ).

    under_test->angebote_kumulieren(
     IMPORTING sum_kunde_artikel = act_sum_kunde_artikel ).

    cl_abap_unit_assert=>assert_equals( exp = exp_sum_kunde_artikel
      act = act_sum_kunde_artikel msg = 'Kumulation fehlerhaft' ).

  ENDMETHOD.

ENDCLASS.
