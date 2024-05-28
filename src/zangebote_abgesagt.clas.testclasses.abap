CLASS test_with_fake DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: under_test TYPE REF TO zangebote_abgesagt.

    CLASS-METHODS class_setup
      RAISING
        cx_static_check.

    METHODS setup.

    METHODS kopfdaten FOR TESTING
      RAISING
        cx_static_check.

    CLASS-METHODS class_teardown
      RAISING
        cx_static_check.

ENDCLASS.

CLASS zangebote_abgesagt DEFINITION LOCAL FRIENDS test_with_fake.

CLASS test_with_fake IMPLEMENTATION.

  METHOD class_setup.
    DATA:
      stub_kopfdaten TYPE STANDARD TABLE OF zangebot_vbak_fa,
      stub_kaufdaten TYPE STANDARD TABLE OF zangebot_vbkd_fa.

    stub_kopfdaten = VALUE #(
      ( vbeln = '1' kunnr = '1' bstdk = '20190105' vbtyp = 'B' )
      ( vbeln = '2' kunnr = '2' bstdk = '20190110' vbtyp = 'B' )
      ( vbeln = '3' kunnr = '1' bstdk = '20190201' vbtyp = 'B' )
      ( vbeln = '4' kunnr = '1' bstdk = '20190107' vbtyp = 'C' ) ).
    stub_kaufdaten = VALUE #(
      ( vbeln = '1' bstdk = '20190105' )
      ( vbeln = '2' bstdk = '20190110' )
      ( vbeln = '3' bstdk = '20190201' )
      ( vbeln = '4' bstdk = '20190107' ) ).

    DELETE FROM zangebot_vbak_fa.
    INSERT zangebot_vbak_fa FROM TABLE stub_kopfdaten.
    DELETE FROM zangebot_vbkd_fa.
    INSERT zangebot_vbkd_fa FROM TABLE stub_kaufdaten.
    COMMIT WORK AND WAIT.

    cl_osql_replace=>activate_replacement(
      replacement_table =
      VALUE #(
      ( source = 'VBAK' target = 'ZANGEBOT_VBAK_FA' )
      ( source = 'VBKD' target = 'ZANGEBOT_VBKD_FA' ) ) ).

  ENDMETHOD.

  METHOD setup.

    DATA(bestelldaten) = VALUE zangebote_abgesagt=>_bestelldaten(
      ( sign = 'I' option = 'BT' low = '20190101' high = '20190131' ) ).

    CREATE OBJECT under_test
      EXPORTING
        bestelldaten = bestelldaten
        kunden       = VALUE zangebote_abgesagt=>_kunden( )
        artikel      = VALUE zangebote_abgesagt=>_artikel( ).

  ENDMETHOD.

  METHOD kopfdaten.

    DATA(exp_ausschluss) = VALUE zangebots_kopfdaten(
      ( vbeln = '3' bstdk = '20190201' kunnr = '1' )
      ( vbeln = '4' bstdk = '20190107' kunnr = '1' ) ).
    DATA(exp_einschluss) = VALUE zangebots_kopfdaten(
      ( vbeln = '1' bstdk = '20190105' kunnr = '1' )
      ( vbeln = '2' bstdk = '20190110' kunnr = '2' ) ).

    under_test->get_angebotskopfdaten(
      IMPORTING
        kopfdaten = DATA(act_kopfdaten) ).

    LOOP AT exp_ausschluss ASSIGNING FIELD-SYMBOL(<exp_ausschluss>).
      cl_abap_unit_assert=>assert_table_not_contains(
        line = <exp_ausschluss> table = act_kopfdaten
        msg = 'Ausgeschlossener Beleg ist enthalten' ).
    ENDLOOP.

    LOOP AT exp_einschluss ASSIGNING FIELD-SYMBOL(<exp_einschluss>).
      cl_abap_unit_assert=>assert_table_contains(
        line = <exp_einschluss> table = act_kopfdaten
        msg = 'Angebot solle enthalten sein, ist aber nicht' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD class_teardown.

    cl_osql_replace=>activate_replacement( ).

  ENDMETHOD.

ENDCLASS.

CLASS test_angebote_abgesagt DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: under_test TYPE REF TO zangebote_abgesagt.

    CLASS-METHODS class_setup.

    METHODS setup.

    CLASS-METHODS db_stub_vorbereiten.

    METHODS initialisierung
      CHANGING
        summe    TYPE zangebot_summe_kunde_artikel
        position TYPE zangebots_position.

    " offenes Angebot (nicht abgesagt) zur Summe hinzufuegen
    METHODS add_ang_to_summe FOR TESTING.

    " abgesagtes Angebot zur Summe hinzufuegen
    METHODS add_asg_ang_to_summe FOR TESTING.

    METHODS kumulation FOR TESTING.

    METHODS kopfdaten FOR TESTING
      RAISING
        cx_static_check.

    METHODS positionsdaten FOR TESTING
      RAISING
        cx_static_check.

    METHODS add_log_message FOR TESTING.

ENDCLASS.

" Die Friends-Beziehung erlaubt uns den Zugriff
" auf alle Methoden und Attribute (auch geschuetzte und private)
CLASS zangebote_abgesagt DEFINITION LOCAL FRIENDS test_angebote_abgesagt.

CLASS test_angebote_abgesagt IMPLEMENTATION.

  METHOD class_setup.

    db_stub_vorbereiten( ).

  ENDMETHOD.

  METHOD setup.

    DATA(bestelldaten) = VALUE zangebote_abgesagt=>_bestelldaten(
      ( sign = 'I' option = 'BT' low = '20190101' high = '20190131' ) ).

    CREATE OBJECT under_test
      EXPORTING
        bestelldaten = bestelldaten
        kunden       = VALUE zangebote_abgesagt=>_kunden( )
        artikel      = VALUE zangebote_abgesagt=>_artikel( ).

  ENDMETHOD.

  METHOD db_stub_vorbereiten.

    DATA(test_environment) = cl_osql_test_environment=>create(
      VALUE #( ( 'MARA' ) ( 'VBAK' ) ( 'VBKD' ) ( 'VBAP' ) ) ).
    test_environment->clear_doubles( ).

    DATA(materialien) = VALUE zangebot_materialien(
      ( matnr = '1' meins = 'ST' )
      ( matnr = '2' meins = 'ST' ) ).
    test_environment->insert_test_data( materialien ).

    DATA(kopfdaten) = VALUE zangebot_kopfdaten(
      ( vbeln = '1' kunnr = '1' bstdk = '20190105' vbtyp = 'B' )
      ( vbeln = '2' kunnr = '2' bstdk = '20190110' vbtyp = 'B' )
      ( vbeln = '3' kunnr = '1' bstdk = '20190201' vbtyp = 'B' )
      ( vbeln = '4' kunnr = '1' bstdk = '20190107' vbtyp = 'C' ) ).
    test_environment->insert_test_data( kopfdaten ).

    DATA(kaufdaten) = VALUE zangebot_kaufdaten(
      ( vbeln = '1' bstdk = '20190105' )
      ( vbeln = '2' bstdk = '20190110' )
      ( vbeln = '3' bstdk = '20190201' )
      ( vbeln = '4' bstdk = '20190107' ) ).
    test_environment->insert_test_data( kaufdaten ).

    DATA(positionsdaten) = VALUE zangebot_positionsdaten(
      ( vbeln = '1' posnr = 10 matnr = '1' netpr = 100 kpein = 1 kmein = 'ST' waerk = 'EUR' )
      ( vbeln = '1' posnr = 20 matnr = '2' netpr = 80 kpein = 1 kmein = 'ST' waerk = 'EUR' )
      ( vbeln = '2' posnr = 10 matnr = '1' netpr = 110 kpein = 1 kmein = 'ST' waerk = 'EUR'
        abgru = '02' )
      ( vbeln = '3' posnr = 10 matnr = '1' netpr = 100 kpein = 1 kmein = 'ST' waerk = 'EUR' )
      ( vbeln = '4' posnr = 10 matnr = '1' netpr = 100 kpein = 1 kmein = 'ST' waerk = 'EUR' ) ).
    test_environment->insert_test_data( positionsdaten ).

  ENDMETHOD.

  METHOD initialisierung.

    CLEAR: position.
    summe = VALUE #( netto_preis_von = 100 netto_preis_bis = 200
      kpein_von = 1 kpein_bis = 1 ).

  ENDMETHOD.

  METHOD add_ang_to_summe.
    DATA: summe     TYPE zangebot_summe_kunde_artikel,
          position  TYPE zangebots_position,
          exp_summe TYPE zangebot_summe_kunde_artikel.

    initialisierung(
      CHANGING
        summe = summe
        position = position ).
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

    " Preis < Preis von
    initialisierung(
      CHANGING
        summe = summe
        position = position ).
    position-netpr = 800.
    position-kpein = 10.
    position-abgru = zangebote_abgesagt=>absage_grund_zu_teuer.
    exp_summe = summe.
    exp_summe-netto_preis_von = 800.
    exp_summe-kpein_von = 10.
    exp_summe-anzahl_gesamt = 1.
    exp_summe-anzahl_abgesagt = 1.
    under_test->add_ang_to_summe( EXPORTING position = position
      CHANGING summe = summe ).
    cl_abap_unit_assert=>assert_equals( exp = exp_summe act = summe
      msg = 'Summe fuer abgesagtes Angebot nicht korrekt' ).

    " Preis diese Position innerhalb Preis von und Preis bis
    initialisierung(
      CHANGING
        summe = summe
        position = position ).
    position-netpr = 120.
    position-kpein = 1.
    position-abgru = zangebote_abgesagt=>absage_grund_zu_teuer.
    exp_summe = summe.
    exp_summe-anzahl_gesamt = 1.
    exp_summe-anzahl_abgesagt = 1.
    under_test->add_ang_to_summe( EXPORTING position = position
      CHANGING summe = summe ).
    cl_abap_unit_assert=>assert_equals( exp = exp_summe act = summe
      msg = 'Summe fuer abgesagtes Angebot nicht korrekt' ).

    " Preis diese Position > Preis bis
    initialisierung(
      CHANGING
        summe = summe
        position = position ).
    position-netpr = 300.
    position-kpein = 1.
    position-abgru = zangebote_abgesagt=>absage_grund_zu_teuer.
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
          act_sum_kunde_artikel TYPE zangebote_abgesagt=>summe_kunde_artikel,
          exp_sum_kunde_artikel TYPE zangebote_abgesagt=>summe_kunde_artikel.

    kopfdaten = VALUE #(
    ( vbeln = '1' kunnr = '1' bstdk = '20190110' )
    ( vbeln = '2' kunnr = '1' bstdk = '20190111' )
    ( vbeln = '3' kunnr = '2' bstdk = '20190110' ) ).

    positionen = VALUE #(
    " nicht abgesagt
    ( vbeln = '1' matnr = 'A1' netpr = 1400 kpein = 10 kmein = 'ST' )
    " abgesagt
    ( vbeln = '1' matnr = 'A1' netpr = 1600 kpein = 10 kmein = 'ST' abgru = zangebote_abgesagt=>absage_grund_zu_teuer )
    " abgesagt
    ( vbeln = '2' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' abgru = zangebote_abgesagt=>absage_grund_zu_teuer )
    ( vbeln = '3' matnr = 'A2' netpr = 1000 kpein = 10 kmein = 'ST' ) ).

    exp_sum_kunde_artikel = VALUE #(
    ( kunnr = '1' monat = '201901' artikel = 'A1' anzahl_gesamt = 2
      kmein = 'ST' anzahl_abgesagt = 1 netto_preis_von = 1600
      netto_preis_bis = 1600 kpein_von = 10 kpein_bis = 10 )
    ( kunnr = '1' monat = '201901' artikel = 'A2' anzahl_gesamt = 1
      kmein = 'ST' anzahl_abgesagt = 1 netto_preis_von = 1000
      netto_preis_bis = 1000 kpein_von = 10 kpein_bis = 10 )
    ( kunnr = '2' monat = '201901' artikel = 'A2' anzahl_gesamt = 1
      kmein = 'ST' anzahl_abgesagt = 0 )  ).

    under_test->angebote_kumulieren( EXPORTING kopfdaten = kopfdaten
      positionsdaten = positionen
      IMPORTING sum_kunde_artikel = act_sum_kunde_artikel ).

    cl_abap_unit_assert=>assert_equals( exp = exp_sum_kunde_artikel
      act = act_sum_kunde_artikel msg = 'Kumulation fehlerhaft' ).

  ENDMETHOD.

  METHOD kopfdaten.

    DATA(exp_ausschluss) = VALUE zangebots_kopfdaten(
      ( vbeln = '3' bstdk = '20190201' kunnr = '1' )
      ( vbeln = '4' bstdk = '20190107' kunnr = '1' ) ).
    DATA(exp_einschluss) = VALUE zangebots_kopfdaten(
      ( vbeln = '1' bstdk = '20190105' kunnr = '1' )
      ( vbeln = '2' bstdk = '20190110' kunnr = '2' ) ).

    under_test->get_angebotskopfdaten(
      IMPORTING
        kopfdaten = DATA(act_kopfdaten) ).

    LOOP AT exp_ausschluss ASSIGNING FIELD-SYMBOL(<exp_ausschluss>).
      cl_abap_unit_assert=>assert_table_not_contains(
        line = <exp_ausschluss> table = act_kopfdaten
        msg = 'Ausgeschlossener Beleg ist enthalten' ).
    ENDLOOP.

    LOOP AT exp_einschluss ASSIGNING FIELD-SYMBOL(<exp_einschluss>).
      cl_abap_unit_assert=>assert_table_contains(
        line = <exp_einschluss> table = act_kopfdaten
        msg = 'Angebot solle enthalten sein, ist aber nicht' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD positionsdaten.

    DATA(angebote) = VALUE zangebots_kopfdaten(
      ( vbeln = '1' bstdk = '20190105' kunnr = '1' )
      ( vbeln = '2' bstdk = '20190110' kunnr = '2' ) ).

    DATA(exp_positionen) = VALUE zangebots_positionsdaten(
      ( vbeln = '1' posnr = 10 matnr = '1' netpr = 100 kpein = 1 kmein = 'ST' waerk = 'EUR' )
      ( vbeln = '1' posnr = 20 matnr = '2' netpr = 80 kpein = 1 kmein = 'ST' waerk = 'EUR' )
      ( vbeln = '2' posnr = 10 matnr = '1' netpr = 110 kpein = 1 kmein = 'ST' waerk = 'EUR'
        abgru = '02' ) ).

    under_test->get_angebotspositionen(
      EXPORTING
        angebots_nummern = angebote
      IMPORTING
        positionsdaten = DATA(act_positionen) ).

    " Sortierung, damit Vergleich nicht fehlschlaegt
    SORT: act_positionen BY vbeln posnr,
      exp_positionen BY vbeln posnr.

    cl_abap_unit_assert=>assert_equals( exp = exp_positionen
      act = act_positionen msg = 'Positionen stimmen nicht' ).

  ENDMETHOD.

  METHOD add_log_message.
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
