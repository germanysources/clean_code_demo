# Beispiel für Clean ABAP Code #
In diesem Repository ist ein kleines Bespiel für "Clean ABAP Code". Dieses
Repository soll Entwicklern einige Ideen aufzeigen zum Thema Qualität in der ABAP Entwicklung.

## Features ##
Dem Benutzer wird eine Auswertung mit Angeboten, die mit dem Grund "zu teuer"
abgesagt wurden, im ALV-Grid präsentiert. Diese Auswertung enthält folgende Informationen:
* Kunde und Artikel (Material)
* Monat (bezieht sich auf das Bestelldatum)
* Anzahl der abgesagten Angebote (Grund "zu teuer")
* Anzahl aller Angebote
* Preisspanne von-bis der abgesagten Angebote

Bezieht sich der Preis auf unterschiedliche Mengeneinheiten (z.B. Preis pro
Stück, Preis pro Palette) werden die Mengeneinheiten einheitlich auf die Einheit
des Materialstammes umgerechnet.
Der Schlüssel besteht aus Kunde, Artikelnr., Monat und Währung. Für diese
Kombination gibt es jeweils nur einen Datensatz.
Angebote ohne Preis oder ohne Artikelnummer werden nicht berücksichtigt.

## Pflege der Angebote ##
Dieser Branch ist für SAP Installationen gedacht ohne eingerichtetes oder installiertes SD-Modul. Es gibt 2 Pflegeviews (Transaktion sm30):
Einmal für die Tabelle ```zangebot_vbak``` mit den Kopfdaten der Angebote. Diese Tabelle ersetzt die Tabelle ```vbak``` des SD-Moduls.

![Pflege zangebot_vbak](https://raw.githubusercontent.com/germanysources/clean_code_demo/without_sd_tables/docs/pflege_kopfdaten.png)

Die 2.Tabelle ```zangebot_vbap``` ersetzt die Tabelle ```vbap``` des SD-Moduls mit den Positionsdaten der Angebote:

![Pflege zangebot_vbap](https://raw.githubusercontent.com/germanysources/clean_code_demo/without_sd_tables/docs/pflege_positionsdaten.png)

## Entwicklungsvorgaben ##
* Einfache Nachvollziehbarkeit und Wartung
* Erweiterungsmöglichkeit
* Wiederverwendbarkeit der Anwendungslogik auch wenn sich das User-Interface ändert
* Trennung des User-Interfaces und der Anwendungslogik
* Trennung der Anwendungslogik und der externen Resourcen (Datenbank)
* Verzicht auf die ungarische Notation
* Gut lesbarerer Source-Code (Länge der Methoden wird kurz gehalten)
* Unit-Tests soweit möglich

## Unit-Tests ##
Die Datenbankabfragen werden auch in den Unit-Tests berücksichtigt. Dazu ist das
Tool [mockup_loader](https://github.com/sbcgua/mockup_loader) notwendig.
Dieses Tool importiert Textdateien, die in einem zip-Archiv gepackt sind.
Im Ordner test/resources sind die entsprechenden Vorlagen.
Den Pfad des zip-Archives auf unserer lokalen Installation müssen wir nach dem Import in das
SAP ERP System anpassen jeweils im Makro ```get_mockup_loader``` der Klassen ```zangebote_abgesagt``` und
```zangebote_abgesagt_mock_demo```.

Zudem müssen noch in der ```setup``` Methode (lokale Testklasse) der Klassen
```zangebote_abgesagt``` und ```zangebote_abgesagt_mock_demo```
Selektionsoptionen gepflegt werden (entsprechend den lokalen Datenbankinhalten).

### Inhalt der Textdateien ###
Werden die Daten wie in den Screenshoots gepflegt, sehen die Textdateien wie folgt aus:
#### angebote_einschluss.txt ####
VBELN | BSTDK | KUNNR
----- | ----- | -----
1 | 05.01.2019 | 20

#### auftrag_ausschluss.txt ####
VBELN | BSTDK | KUNNR
----- | ----- | -----
2 | 17.01.2019 | 20

#### angebote.txt ####
VBELN
-----
1

#### angebote_positionen.txt ####
VBELN | POSNR | MATNR | NETPR | KPEIN | KMEIN | ABGRU | WAERK
----- | ----- | ----- | ----- | ----- | ----- | ----- | -----
1 | 10 | 100-A1 | 3000 | 100 | ST | 02 | EUR
1 | 20 | 100-A2 | 4000 | 100 | ST | 02 | EUR

## Installation ##
Importiert wird der Source-Code mit dem Tool [abapgit](https://github.com/larshp/abapgit).
Mindestens SAP Basis 740 wird benötigt.

### Log Protokolle ###
In der Transaktion slg0 muss zum Objekt "SD-SLS" das Unterobjekt "ZANG_ABGESAGT"
angelegt werden.

## Entwicklungsobjekte ##
### Report ```zangebote_abgesagt``` ###
Enthält nur das User-Interface mit dem ALV-Grid im Volldbildschirm sowie die
Anzeige des Log-Protokolles. 

### Klasse ```zangebote_abgesagt```  ###
Enthält nur die Anwendungslogik. Soll die Trennung von externen Resourcen
und Anwendungslogik, klassenbasierte Ausnahmen zur vollständigen Abtrennung
des User-Interfaces sowie die Umsetzung von Unit-Tests demonstrieren.
Ein Log-Protokoll wird mit der Funktionengruppe ```SBAL``` erzeugt.

### Klasse ```zangebote_abgesagt_mock_demo``` ###
Enthält die selbe Funktion wie Klasse ```zangebote_abgesagt```. Hier wurde
nur die Methode ```angebote_kumulieren``` etwas anders implementiert um das mocken
(gezieltes Ersetzen) von Datenbankabfragen in den Unit-Tests zu demonstrieren.
Ein Log-Protokoll wird mit der Funktionengruppe ```SBAL``` erzeugt.
