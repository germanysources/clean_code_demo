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

class ZANGEBOTE_ABGESAGT definition
  public
  final
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

  constants ABSAGE_GRUND_ZU_TEUER type zangebot_ABGRU value '02' ##NO_TEXT.
  constants VBTYP_ANGEBOT type VBTYP value 'B' ##NO_TEXT.
  data BESTELLDATEN type _BESTELLDATEN .
  data KUNDEN type _KUNDEN .
  data ARTIKEL type _ARTIKEL .
  data LOG_HANDLE type BALLOGHNDL .

  methods EINHEITEN_UMRECHNUNG
    changing
      !POSITION type ZANGEBOTS_POSITION .
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
    importing
      !KOPFDATEN type ZANGEBOTS_KOPFDATEN
      !POSITIONSDATEN type ZANGEBOTS_POSITIONSDATEN
    exporting
      !SUM_KUNDE_ARTIKEL type SUMME_KUNDE_ARTIKEL .
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
  methods VERHAELTNIS
    changing
      !SUMME type SUMME_KUNDE_ARTIKEL .
ENDCLASS.



CLASS ZANGEBOTE_ABGESAGT IMPLEMENTATION.


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


  method ANGEBOTE_KUMULIEREN.
    DATA: sum TYPE zangebot_summe_kunde_artikel.
    FIELD-SYMBOLS: <kopf> TYPE zangebots_kopf,
                   <position> TYPE zangebots_position.

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

  endmethod.


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
    log_header-object = 'ZSD-SLS'.
    log_header-subobject = 'ZANG_ABGESAGT'.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = log_header
      IMPORTING
        e_log_handle = log_handle.

  endmethod.


  method EINHEITEN_UMRECHNUNG.
    DATA: ziel_einheit TYPE meins,
          ##NEEDED
          mtext TYPE string.

    SELECT SINGLE basis_einheit INTO ziel_einheit
      FROM zangebot_materia
      WHERE matnr = position-matnr.
   IF sy-subrc <> 0.
     " Meldung in String-Objekt schreiben, damit die Meldung protokolliert werden kann
     MESSAGE e003(zangebote_abgesagt) WITH position-vbeln position-posnr position-matnr INTO mtext.
     add_log_message( position ).
     RETURN.
   ENDIF.

   CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
     EXPORTING
       input = position-kpein
       unit_in = position-kmein
       unit_out = ziel_einheit
     IMPORTING
       output = position-kpein
     EXCEPTIONS
       conversion_not_found = 2
       division_by_zero = 4
       units_missing = 6
       unit_in_not_found = 8
       unit_out_not_found = 10.
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

  endmethod.


  method GET_ANGEBOTE.
    DATA: kopfdaten TYPE zangebots_kopfdaten,
          positionsdaten TYPE zangebots_positionsdaten,
          hash_sum_kunde_artikel TYPE summe_kunde_artikel.

    get_angebotskopfdaten( IMPORTING kopfdaten = kopfdaten ).
    get_angebotspositionen( EXPORTING angebots_nummern = kopfdaten
        IMPORTING positionsdaten = positionsdaten ).
    angebote_kumulieren( EXPORTING kopfdaten = kopfdaten
      positionsdaten = positionsdaten
      IMPORTING sum_kunde_artikel = hash_sum_kunde_artikel ).

    verhaeltnis( CHANGING summe = hash_sum_kunde_artikel ).
    get_texte( CHANGING summe = hash_sum_kunde_artikel ).

    CLEAR: summe_kunde_artikel.
    APPEND LINES OF hash_sum_kunde_artikel TO summe_kunde_artikel.

  endmethod.


  method GET_ANGEBOTSKOPFDATEN.

    SELECT vbeln, kunnr, bstdk FROM zangebot_vbak
      INTO CORRESPONDING FIELDS OF TABLE @kopfdaten
      WHERE kunnr IN @kunden AND bstdk IN @bestelldaten
      AND vbtyp = @vbtyp_angebot.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_angebot_abgesagt
        EXPORTING
          textid = zcx_angebot_abgesagt=>not_found.
    ENDIF.

  endmethod.


  METHOD get_angebotspositionen.
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
      FROM zangebot_vbap INTO CORRESPONDING FIELDS OF TABLE @positionsdaten
      WHERE vbeln IN @belege AND matnr IN @artikel AND netpr > 0.

    " Umrechnung der Mengeneinheit des Preises
    " auf die Einheit im Materialstamm
    LOOP AT positionsdaten ASSIGNING <position>.
      einheiten_umrechnung( CHANGING position = <position> ).
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

    " zur besseren Performance werden die Bezeichnungen und Namen
    " alle in einem select gelesen
    SELECT kunnr, name1 FROM zangebot_kunden
      INTO CORRESPONDING FIELDS OF TABLE @name_kunden
      FOR ALL ENTRIES IN @summe
      WHERE kunnr = @summe-kunnr.
    SELECT matnr, maktx FROM zangebot_mattext
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


  method VERHAELTNIS.
    FIELD-SYMBOLS: <sum> TYPE zangebot_summe_kunde_artikel.

    " Verhaeltnis Anzahl abgesagte Positionen zur Anzahl aller Positionen
    LOOP AT summe ASSIGNING <sum>.
      <sum>-ver_abs = <sum>-anzahl_abgesagt / <sum>-anzahl_gesamt * 100.
    ENDLOOP.

  endmethod.
ENDCLASS.
