* MIT License
* Copyright (c) 2019 Johannes Gerbershagen
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
class ZANGEBOTE_ABGESAGT_STUB_DEMO1 definition
  public
  create public .

public section.

  types:
    _bestelldaten TYPE RANGE OF bstdk .
  types:
    _kunden TYPE RANGE OF kunnr .
  types:
    _artikel TYPE RANGE OF matnr .
  types:
    summe_kunde_artikel TYPE HASHED TABLE OF zangebot_summe_kunde_artikel
      WITH UNIQUE KEY kunnr artikel monat kmein waerk .
  types:
    out_summe_kunde_artikel TYPE STANDARD TABLE OF zangebot_summe_kunde_artikel .

  methods CONSTRUCTOR
    importing
      !BESTELLDATEN type _BESTELLDATEN
      !KUNDEN type _KUNDEN
      !ARTIKEL type _ARTIKEL
      !DATEN_EXTRAKTOR type ref to ZIF_ANGEBOT_DATEN_EXTRAKTOR .
  methods GET_ANGEBOTE
    exporting
      !SUMME_KUNDE_ARTIKEL type OUT_SUMME_KUNDE_ARTIKEL
    raising
      ZCX_ANGEBOT_ABGESAGT .
  methods TEARDOWN .
  PROTECTED SECTION.
    CONSTANTS absage_grund_zu_teuer TYPE abgru VALUE '02' ##NO_TEXT.

    METHODS angebote_kumulieren
      EXPORTING
        !sum_kunde_artikel TYPE summe_kunde_artikel
      RAISING
        zcx_angebot_abgesagt .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF _name_kunde,
        kunnr TYPE kunnr,
        name1 TYPE name1_gp,
      END OF _name_kunde .
    TYPES:
      BEGIN OF _bez_artikel,
        matnr TYPE matnr,
        maktx TYPE maktx,
      END OF _bez_artikel .

    CONSTANTS vbtyp_angebot TYPE vbtyp VALUE 'B' ##NO_TEXT.
    DATA bestelldaten TYPE _bestelldaten .
    DATA kunden TYPE _kunden .
    DATA artikel TYPE _artikel .
    DATA log_handle TYPE balloghndl .
    DATA daten_extraktor TYPE REF TO zif_angebot_daten_extraktor.

    METHODS einheiten_umrechnung
      CHANGING
        !position TYPE zangebots_position .
    METHODS add_ang_to_summe
      IMPORTING
        !position TYPE zangebots_position
      CHANGING
        !summe    TYPE zangebot_summe_kunde_artikel .
    METHODS add_log_message
      IMPORTING
        !position TYPE zangebots_position .
    METHODS get_texte
      CHANGING
        !summe TYPE summe_kunde_artikel .
    METHODS verhaeltnis
      CHANGING
        !summe TYPE summe_kunde_artikel .
ENDCLASS.



CLASS ZANGEBOTE_ABGESAGT_STUB_DEMO1 IMPLEMENTATION.


  METHOD add_ang_to_summe.
    DATA: netto_pos(8) TYPE p DECIMALS 4,
          netto_ver(8) TYPE p DECIMALS 4.

    " Fuer Vergleichbarkeit der Preise immer auf Preis pro Einheit (Stueck, kg ...) umrechnen
    " Es kann Angebote geben mit einem Preis pro 100 Stueck und einmal Preis pro Stueck
    netto_pos = position-netpr / position-kpein.

    IF position-abgru = absage_grund_zu_teuer.
      ADD 1 TO summe-anzahl_abgesagt.

      " minimaler Preis
      IF summe-kpein_von > 0.
        netto_ver = summe-netto_preis_von / summe-kpein_von.
      ELSE.
        netto_ver = 0.
      ENDIF.
      IF netto_ver > netto_pos OR netto_ver = 0.
        summe-netto_preis_von = position-netpr.
        summe-kpein_von = position-kpein.
      ENDIF.

      " maximaler Preis.
      IF summe-kpein_bis > 0.
        netto_ver = summe-netto_preis_bis / summe-kpein_bis.
      ELSE.
        netto_ver = 0.
      ENDIF.
      IF netto_pos > netto_ver.
        summe-netto_preis_bis = position-netpr.
        summe-kpein_bis = position-kpein.
      ENDIF.

    ENDIF.
    ADD 1 TO summe-anzahl_gesamt.

  ENDMETHOD.


  METHOD add_log_message.
    DATA: log_msg TYPE bal_s_msg.

    " Einmal die System-Meldung in <sy> protokollieren
    " sowie die Meldung mit der Position.
    ##ENH_OK
    MOVE-CORRESPONDING sy TO log_msg.
    log_msg-detlevel = '2'.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = log_handle
        i_s_msg      = log_msg.
    " keine Ausnahmen abgefangen
    " log_not_found log_handle wurde im constructor erzeugt
    " verweist dieses nicht mehr auf ein gueltiges Protokoll
    " haben wir einen Programmierfehler.
    " msg_inconsistent kann nicht auftreten mit den Feldern,
    " die wir im Parameter <i_s_msg> uebergeben haben

    IF 1 = 0.
      " Statement fuer Verwendungsnachweis Meldung
      MESSAGE e001(zangebote_abgesagt) WITH position-vbeln position-posnr.
    ENDIF.
    CLEAR log_msg.
    log_msg-msgid = 'ZANGEBOTE_ABGESAGT'.
    log_msg-msgty = 'E'.
    log_msg-msgno = 001.
    log_msg-msgv1 = position-vbeln.
    log_msg-msgv2 = position-posnr.
    log_msg-detlevel = '1'.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = log_handle
        i_s_msg      = log_msg.

  ENDMETHOD.


  METHOD angebote_kumulieren.
    DATA: sum            TYPE zangebot_summe_kunde_artikel,
          kopfdaten      TYPE zangebots_kopfdaten,
          positionsdaten TYPE zangebots_positionsdaten.
    FIELD-SYMBOLS: <kopf>     TYPE zangebots_kopf,
                   <position> TYPE zangebots_position.

    daten_extraktor->get_angebotskopfdaten(
      IMPORTING
        kopfdaten = kopfdaten ).
    daten_extraktor->get_angebotspositionen(
      EXPORTING
        angebots_nummern = kopfdaten
      IMPORTING
        positionsdaten = positionsdaten ).

    " Initialisierung
    CLEAR sum_kunde_artikel.

    LOOP AT kopfdaten ASSIGNING <kopf>.

      CLEAR: sum.
      sum-kunnr = <kopf>-kunnr.
      " Monat Bestelldatum
      sum-monat = <kopf>-bstdk+0(6).

      LOOP AT positionsdaten ASSIGNING <position> WHERE vbeln = <kopf>-vbeln.

        sum-artikel = <position>-matnr.
        sum-anzahl_gesamt = 1.
        sum-kmein = <position>-kmein.
        sum-waerk = <position>-waerk.
        CLEAR: sum-anzahl_abgesagt, sum-netto_preis_von, sum-netto_preis_bis.

        READ TABLE sum_kunde_artikel INTO sum FROM sum.
        IF sy-subrc = 0.
          DELETE TABLE sum_kunde_artikel FROM sum.
          add_ang_to_summe( EXPORTING position = <position>
            CHANGING summe = sum ).
        ELSE.
          IF <position>-abgru = absage_grund_zu_teuer.
            sum-netto_preis_von = <position>-netpr.
            sum-netto_preis_bis = <position>-netpr.
            sum-kpein_von = <position>-kpein.
            sum-kpein_bis = <position>-kpein.
            sum-anzahl_abgesagt = 1.
          ENDIF.
        ENDIF.
        INSERT sum INTO TABLE sum_kunde_artikel.
        ASSERT sy-subrc = 0.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    DATA log_header TYPE bal_s_log.
    FIELD-SYMBOLS: <e_empty> LIKE LINE OF artikel.

    me->bestelldaten = bestelldaten.
    me->kunden = kunden.
    me->artikel = artikel.
    me->daten_extraktor = daten_extraktor.
    "Angebot ohne Artikelnr. nicht beruecksichtigen
    IF space IN me->artikel.
      APPEND INITIAL LINE TO me->artikel ASSIGNING <e_empty>.
      <e_empty>-sign = 'E'.
      <e_empty>-option = 'EQ'.
    ENDIF.

    " log Protokoll mit Funktionengruppe SBAL erstellen
    log_header-object = 'ZSD-SLS'.
    log_header-subobject = 'ZANG_ABGESAGT'.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = log_header
      IMPORTING
        e_log_handle = log_handle.

  ENDMETHOD.


  METHOD einheiten_umrechnung.
    DATA: ziel_einheit TYPE meins,
          ##NEEDED
          mtext        TYPE string.

    SELECT SINGLE meins INTO ziel_einheit
      FROM mara
      WHERE matnr = position-matnr.
    IF sy-subrc <> 0.
      " Meldung in String-Objekt schreiben, damit die Meldung protokolliert werden kann
      MESSAGE e003(zangebote_abgesagt) WITH position-vbeln position-posnr position-matnr INTO mtext.
      add_log_message( position ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = position-kpein
        unit_in              = position-kmein
        unit_out             = ziel_einheit
      IMPORTING
        output               = position-kpein
      EXCEPTIONS
        conversion_not_found = 2
        division_by_zero     = 4
        units_missing        = 6
        unit_in_not_found    = 8
        unit_out_not_found   = 10.
    " Die restlichen Ausnahmen
    " wurden nicht abgefangen, da diese ihre Ursache
    " in Programmierfehlern haben.
    IF sy-subrc = 0.
      position-kmein = ziel_einheit.
    ELSE.
      " Die Ausnahmen werden protokolliert. Vorraussetzung ist, dass die Ausnahme
      " mit der Anweisung MESSAGE TYPE '' ID '' NUMBER '' RAISING ausgeloest wurde
      " Die Ausgabeparameter "output" per Wertuebergabe uebergeben wurde,
      " findet im Fehlerfall keine Aenderung von position-kpein statt.
      add_log_message( position ).
    ENDIF.

  ENDMETHOD.


  METHOD get_angebote.
    DATA: hash_sum_kunde_artikel TYPE summe_kunde_artikel.

    angebote_kumulieren(
      IMPORTING sum_kunde_artikel = hash_sum_kunde_artikel ).

    verhaeltnis( CHANGING summe = hash_sum_kunde_artikel ).
    get_texte( CHANGING summe = hash_sum_kunde_artikel ).

    CLEAR: summe_kunde_artikel.
    APPEND LINES OF hash_sum_kunde_artikel TO summe_kunde_artikel.

  ENDMETHOD.


  METHOD get_texte.
    DATA: name_kunden TYPE HASHED TABLE OF _name_kunde
          WITH UNIQUE KEY kunnr,
          bez_artikel TYPE HASHED TABLE OF _bez_artikel
          WITH UNIQUE KEY matnr.
    FIELD-SYMBOLS: <sum> TYPE zangebot_summe_kunde_artikel,
                   <nku> TYPE _name_kunde,
                   <bar> TYPE _bez_artikel.

    " zur besseren Performance werden die Bezeichnungen und Namen
    " alle in einem select gelesen
    SELECT kunnr, name1 FROM kna1
      INTO CORRESPONDING FIELDS OF TABLE @name_kunden
      FOR ALL ENTRIES IN @summe
      WHERE kunnr = @summe-kunnr.
    SELECT matnr, maktx FROM makt
      INTO CORRESPONDING FIELDS OF TABLE @bez_artikel
      FOR ALL ENTRIES IN @summe
      WHERE matnr = @summe-artikel AND spras = @sy-langu.

    LOOP AT summe ASSIGNING <sum>.
      READ TABLE name_kunden ASSIGNING <nku> WITH TABLE KEY kunnr = <sum>-kunnr.
      IF sy-subrc = 0.
        <sum>-name_kunde = <nku>-name1.
      ENDIF.
      READ TABLE bez_artikel ASSIGNING <bar> WITH TABLE KEY matnr = <sum>-artikel.
      IF sy-subrc = 0.
        <sum>-bez_artikel = <bar>-maktx.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD teardown.

    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle = log_handle.

  ENDMETHOD.


  METHOD verhaeltnis.
    FIELD-SYMBOLS: <sum> TYPE zangebot_summe_kunde_artikel.

    " Verhaeltnis Anzahl abgesagte Positionen zur Anzahl aller Positionen
    LOOP AT summe ASSIGNING <sum>.
      <sum>-ver_abs = <sum>-anzahl_abgesagt / <sum>-anzahl_gesamt * 100.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
