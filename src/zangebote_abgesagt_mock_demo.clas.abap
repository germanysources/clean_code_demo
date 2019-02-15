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

class ZANGEBOTE_ABGESAGT_MOCK_DEMO definition
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
      !ARTIKEL type _ARTIKEL .
  methods GET_ANGEBOTE
    exporting
      !SUMME_KUNDE_ARTIKEL type OUT_SUMME_KUNDE_ARTIKEL
    raising
      ZCX_ANGEBOT_ABGESAGT .
  methods TEARDOWN .
protected section.

  methods GET_ANGEBOTSKOPFDATEN
    exporting
      !KOPFDATEN type ZANGEBOTS_KOPFDATEN
    raising
      ZCX_ANGEBOT_ABGESAGT .
  methods GET_ANGEBOTSPOSITIONEN
    importing
      !ANGEBOTS_NUMMERN type ZANGEBOTS_KOPFDATEN
    exporting
      !POSITIONSDATEN type ZANGEBOTS_POSITIONSDATEN .
  methods ANGEBOTE_KUMULIEREN
    exporting
      !SUM_KUNDE_ARTIKEL type SUMME_KUNDE_ARTIKEL
    RAISING
      zcx_angebot_abgesagt.
private section.

  types:
    BEGIN OF _name_kunde,
      kunnr TYPE kunnr,
      name1 TYPE name1_gp,
    END OF _name_kunde .
  types:
    BEGIN OF _bez_artikel,
    matnr TYPE matnr,
    maktx TYPE maktx,
  END OF _bez_artikel .

  constants ABSAGE_GRUND_ZU_TEUER type ABGRU value '02' ##NO_TEXT.
  constants VBTYP_ANGEBOT type VBTYP value 'B' ##NO_TEXT.
  data BESTELLDATEN type _BESTELLDATEN .
  data KUNDEN type _KUNDEN .
  data ARTIKEL type _ARTIKEL .
  data LOG_HANDLE type BALLOGHNDL .

  methods ADD_ANG_TO_SUMME
    importing
      !POSITION type ZANGEBOTS_POSITION
    changing
      !SUMME type ZANGEBOT_SUMME_KUNDE_ARTIKEL .
  methods ADD_LOG_MESSAGE
    importing
      !POSITION type ZANGEBOTS_POSITION .
  methods GET_TEXTE
    changing
      !SUMME type SUMME_KUNDE_ARTIKEL .
ENDCLASS.



CLASS ZANGEBOTE_ABGESAGT_MOCK_DEMO IMPLEMENTATION.


  METHOD ADD_ANG_TO_SUMME.
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


  method ADD_LOG_MESSAGE.
    DATA: log_msg TYPE bal_s_msg.

    " Einmal die System-Meldung in <sy> protokollieren
    " sowie die Meldung mit der Position.
    ##ENH_OK
    MOVE-CORRESPONDING sy TO log_msg.
    log_msg-detlevel = '2'.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = log_handle
        i_s_msg = log_msg.
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
        i_s_msg = log_msg.

  endmethod.


  METHOD angebote_kumulieren.
    DATA: sum            TYPE zangebot_summe_kunde_artikel,
          kopfdaten      TYPE zangebots_kopfdaten,
          positionsdaten TYPE zangebots_positionsdaten.
    FIELD-SYMBOLS: <kopf>     TYPE zangebots_kopf,
                   <position> TYPE zangebots_position.

    get_angebotskopfdaten( IMPORTING kopfdaten = kopfdaten ).
    get_angebotspositionen( EXPORTING angebots_nummern = kopfdaten
        IMPORTING positionsdaten = positionsdaten ).

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


  method CONSTRUCTOR.
    DATA log_header TYPE bal_s_log.
    FIELD-SYMBOLS: <e_empty> LIKE LINE OF artikel.

    me->bestelldaten = bestelldaten.
    me->kunden = kunden.
    me->artikel = artikel.
    "Angebot ohne Artikelnr. nicht beruecksichtigen
    IF space IN me->artikel.
      APPEND INITIAL LINE TO me->artikel ASSIGNING <e_empty>.
      <e_empty>-sign = 'E'.
      <e_empty>-option = 'EQ'.
    ENDIF.

    " log Protokoll mit Funktionengruppe SBAL erstellen
    log_header-object = 'SD-SLS'.
    log_header-subobject = 'ZANG_ABGESAGT'.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = log_header
      IMPORTING
        e_log_handle = log_handle.

  endmethod.


  method GET_ANGEBOTE.
    DATA: hash_sum_kunde_artikel TYPE summe_kunde_artikel.
    FIELD-SYMBOLS: <sum> TYPE zangebot_summe_kunde_artikel.

    angebote_kumulieren(
      IMPORTING sum_kunde_artikel = hash_sum_kunde_artikel ).

    " Verhaeltnis Abgesagte gesamt Anzahl Angebote
    LOOP AT hash_sum_kunde_artikel ASSIGNING <sum>.
      " Jetzt koennen nur Nicht-Schluesselfelder geaendert werden
      <sum>-ver_abs = <sum>-anzahl_abgesagt / <sum>-anzahl_gesamt * 100.
    ENDLOOP.

    get_texte( CHANGING summe = hash_sum_kunde_artikel ).

    CLEAR: summe_kunde_artikel.
    APPEND LINES OF hash_sum_kunde_artikel TO summe_kunde_artikel.

  endmethod.


  method GET_ANGEBOTSKOPFDATEN.

    SELECT k~vbeln, k~kunnr, kd~bstdk FROM vbak AS k
      INNER JOIN vbkd AS kd ON kd~vbeln = k~vbeln
      INTO CORRESPONDING FIELDS OF TABLE @kopfdaten
      WHERE k~kunnr IN @kunden AND kd~bstdk IN @bestelldaten
      AND vbtyp = @vbtyp_angebot.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_angebot_abgesagt
        EXPORTING
          textid = zcx_angebot_abgesagt=>not_found.
    ENDIF.

  endmethod.


  METHOD GET_ANGEBOTSPOSITIONEN.
    DATA: belege TYPE RANGE OF vbeln.
    FIELD-SYMBOLS: <no>       TYPE zangebots_kopf,
                   <rno>      LIKE LINE OF belege,
                   <position> TYPE zangebots_position.

    LOOP AT angebots_nummern ASSIGNING <no>.
      APPEND INITIAL LINE TO belege ASSIGNING <rno>.
      <rno>-sign = 'I'.
      <rno>-option = 'EQ'.
      <rno>-low = <no>-vbeln.
    ENDLOOP.

    SELECT vbeln, posnr, matnr, netpr, kpein, kmein, abgru, waerk
      FROM vbap INTO CORRESPONDING FIELDS OF TABLE @positionsdaten
      WHERE vbeln IN @belege AND matnr IN @artikel AND netpr > 0.

    " Umrechnung der Mengeneinheit des Preises
    " auf die Einheit im Materialstamm
    LOOP AT positionsdaten ASSIGNING <position>.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = <position>-kpein
          kzmeinh              = abap_true
          meinh                = <position>-kmein
          matnr                = <position>-matnr
        IMPORTING
          output               = <position>-kpein
          meins                = <position>-kmein
        EXCEPTIONS
          conversion_not_found = 2
          meinh_not_found      = 4
          no_meinh             = 6
          overflow             = 8.
      " Die restlichen Ausnahmen wurden nicht abgefangen
      " da diese ihre Ursache in Programmierfehlern haben.
      " input_invalid kann nicht auftreten <position>-kpein vom Type p
      " output_invalid kann nicht auftreten <position>-kpein vom Type p
      IF sy-subrc <> 0.
        " Umrechnung nicht moeglich. Protokollieren und Angebot ignorerein
        add_log_message( <position> ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  method GET_TEXTE.
    DATA: name_kunden TYPE HASHED TABLE OF _name_kunde
          WITH UNIQUE KEY kunnr,
          bez_artikel TYPE HASHED TABLE OF _bez_artikel
          WITH UNIQUE KEY matnr.
    FIELD-SYMBOLS: <sum> TYPE zangebot_summe_kunde_artikel,
                   <nku> TYPE _name_kunde,
                   <bar> TYPE _bez_artikel.

    " zur besseren Performance werden die Bezeichnung und Namen
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

  endmethod.


  method TEARDOWN.

    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle = log_handle.

  endmethod.
ENDCLASS.
