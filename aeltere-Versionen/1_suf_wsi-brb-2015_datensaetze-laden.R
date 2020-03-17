# Datensätze einlesen

# Übersicht der Datensätze: ----------------------------------------------------

# Stata:
## infas_Befragungsdaten_Betriebsrätepanel_4808_20150518_1
## infas_Gewichte_WSI_4808_20150625_2
## infas_Stichprobenmerkmale_WSI_4808_20150611
## infas_Methodendatensatz_WSI_4808_20150812
## infas_Interviewerdatensatz_WSI_4808_20150717
## infas_Kontaktverlaufsdatensatz_WSI_4808_20150812

# SPSS:
## infas_Methodendatensatz_WSI_4808_20150812
## infas_Gewichte_WSI_4808_20150625_2
## infas_Befragungsdaten_Betriebsrätepanel_4808_20150518_1

valuelabels <- list()
varlabels <- list()

# Befragungsdaten einlesen -----------------------------------------------------
dat <- read.dta13(
  paste(
    "./Input/",
    "infas_Befragungsdaten_Betriebsrätepanel_4808_20150518_1",
    # korrigierte Fassung (ohne Duplikate) verwenden:
    "_Korrektur-R1a",
    ".dta",
    sep = ""
  ),
  # fehlende Faktor-Labels generieren (z.B. für Skalen):
  generate.factors = F,
  # Kodierung (wichtig für Umlaute und Sonderzeichen):
  encoding = "UTF-8",
  # ursprüngliche Kodierung:
  fromEncoding = "UTF-8",

  # Keine Label-Zuweisungen für Type float und double (numerische Var.)
  nonint.factors = F
)


# Interviewerdaten einlesen -----------------------------------------------------
int <- read.dta13(
        paste(
                "./Input/",
                "infas_Interviewerdatensatz_WSI_4808_20150717",
                ".dta",
                sep = ""
        ),
        # fehlende Faktor-Labels generieren (z.B. für Skalen):
        generate.factors = F,
        # Kodierung (wichtig für Umlaute und Sonderzeichen):
        encoding = "UTF-8",
        # ursprüngliche Kodierung:
        fromEncoding = "CP1252",
        # Keine Label-Zuweisungen für Type float und double (numerische Var.)
        nonint.factors = T
)




# Methodendaten einlesen -----------------------------------------------------
met <- read.dta13(
        paste(
                "./Input/",
                "infas_Methodendatensatz_WSI_4808_20150812",
                ".dta",
                sep = ""
        ),
        # fehlende Faktor-Labels generieren (z.B. für Skalen):
        generate.factors = F,
        # Kodierung (wichtig für Umlaute und Sonderzeichen):
        encoding = "UTF-8",
        # ursprüngliche Kodierung:
        fromEncoding = "CP1252",
        # Keine Label-Zuweisungen für Type float und double (numerische Var.)
        nonint.factors = T
)

# Kontaktverlaufsdaten einlesen ------------------------------------------------
kon <- read.dta13(
        paste(
                "./Input/",
                "infas_Kontaktverlaufsdatensatz_WSI_4808_20150812",
                ".dta",
                sep = ""
        ),
        # fehlende Faktor-Labels generieren (z.B. für Skalen):
        generate.factors = F,
        # Kodierung (wichtig für Umlaute und Sonderzeichen):
        encoding = "UTF-8",
        # ursprüngliche Kodierung:
        fromEncoding = "CP1252",
        # Keine Label-Zuweisungen für Type float und double (numerische Var.)
        nonint.factors = T
)

# Gewichtungsdaten einlesen ------------------------------------------------
gew <- read.dta13(
        paste(
                "./Input/",
                "infas_Gewichte_WSI_4808_20150625_2",
                ".dta",
                sep = ""
        ),
        # fehlende Faktor-Labels generieren (z.B. für Skalen):
        generate.factors = F,
        # Kodierung (wichtig für Umlaute und Sonderzeichen):
        encoding = "UTF-8",
        # ursprüngliche Kodierung:
        fromEncoding = "CP1252",
        # Keine Label-Zuweisungen für Type float und double (numerische Var.)
        nonint.factors = T
)


# Stichprobendaten einlesen ----------------------------------------------------
sti <- read.dta13(
  paste(
    "./Input/",
    "infas_Stichprobenmerkmale_WSI_4808_20150611",
    ".dta",
    sep = ""
  ),
  # fehlende Faktor-Labels generieren (z.B. für Skalen):
  generate.factors = F,
  # Kodierung (wichtig für Umlaute und Sonderzeichen):
  encoding = "UTF-8",
  # ursprüngliche Kodierung:
  fromEncoding = "CP1252",
  # Keine Label-Zuweisungen für Type float und double (numerische Var.)
  nonint.factors = T
)

# Vollständige Liste mit Valuelabels und varlabels erzeugen------------------
daten_input <- list(dat, int, met, kon, gew, sti)
names(daten_input) <- c("dat", "int", "met", "kon", "gew", "sti")

# "Falsche" Faktoren in numerische Variablen umwandeln-------------------------
for(b in names(daten_input)) {
  daten_input[[b]] <- set_fac(daten_input[[b]])
}

for(b in names(daten_input)) {
  valuelabels[[b]] <- get.labeltables(daten_input[[b]])
  varlabels[[b]] <- attr(daten_input[[b]], "var.labels")
  names(varlabels[[b]]) <- names(daten_input[[b]])
}

# Workspace bereinigen --------------------------------------------------------
rm(list=c("dat", "gew", "int", "kon", "met", "sti", "b"))
