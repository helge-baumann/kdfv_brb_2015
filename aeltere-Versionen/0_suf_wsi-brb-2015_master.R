############
# Masterfile zur Erzeugung eines SUF der WSI-Betriebsraetebefragung 2015
# Ersteller: Helge Baumann, WSI
# Letztes Update: 25.03.2018
############

# Präambel --------------------------------------------------------------------

# Alten Workspace bereinigen:
rm(list = ls())

# Notwendige Pakete laden

## Pakete ggfs. installieren und laden
library(pacman)
p_load("readstata13", "styler", "haven", "labelled",
       "here") # "here" sollte bei akt. VZ starten!

# Subdateien ausführen ---------------------------------------------------------

Fertige_Exportdaten <- list()

# Funktionen
source("./R_Funk/funktionen_suf.R") # notwendig für valuelabels u. num. Var.
# Daten in ./Input
source("1_suf_wsi-brb-2015_datensaetze-laden.R", encoding = "UTF-8")
# Warnmeldung "Factor codes of type double or float detected" ignorieren. 
# Output Datensätze:   Befragungsdaten (dat), Interviewerdaten (int), 
  # Methodendaten (met), Kontaktverlaufsdaten (kon),
  # Gewichte (gew), Stichprobenmerkmale (sti)
# Valuelabels jeweils Datensatz + .valuelabels (zB "dat.valuelabels")
# Variablenlabel jeweils Datensatz + .varlabels (zB "dat.varlabels")

# Methodendatensatz erzeugen 
source("2_suf_wsi-brb-2015_methodendaten-auslagern.R", encoding = "UTF-8")
  # ggfs. Methodendaten sichern und exportieren

# Daten aufbereiten
source("3_suf_wsi-brb-2015_befragungsdaten-aufbereiten.R", encoding = "UTF-8")
  # Exportdatei: "Datensatz_Intern"

source("4_suf_wsi-brb-2015_befragungsdaten-bereinigen.R", encoding = "UTF-8")
# Deanonymisierungsvariablen löschen; Methodenvariablen löschen
# Daten vergröbern
# HIER weiterarbeiten


# zunächst alle Variablen löschen (zB Zielperson)
# Dann Methodendaten löschen
# Dann Vergröberung aller Angaben
# heikel: Krankenstand, Anzahl BR-Mitgl. (DGB-)Gewerkschaft, Betreuung Gew (O1),
# heikel: Stichprobenmerkmale

# Gewichte zuspielen

# Stichprobenmerkmale zuspielen und bereinigen

# Methodendaten und Befragungsdaten exportieren
# hier letzte Prüfung der heiklen Angaben. 




# SessionInfo erzeugen ---------------------------------------------------------
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
