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
## infas_Gewichte_WSI_4808_20150625_2
## infas_Befragungsdaten_Betriebsrätepanel_4808_20150518_1

# Befragungsdaten einlesen -----------------------------------------------------
rawdat <- read_dta(
  paste0(
    "./Input/",
    "infas_Befragungsdaten_Betriebsrätepanel_4808_20150518_1",
    "_Korrektur-R1a", # korrigierte Fassung (ohne Duplikate) verwenden
    ".dta"
    )
  )

# Gewichtungsdaten einlesen ----------------------------------------------------
gew <- read_dta(
  paste0(
    "./Input/",
    "infas_Gewichte_WSI_4808_20150625_2",
    ".dta"
    )
  )

# Interviewerdaten einlesen -----------------------------------------------------
int <- read_dta(
  paste0(
    "./Input/",
    "infas_Interviewerdatensatz_WSI_4808_20150717",
    ".dta"
  )
)

# Methodenvariablen einlesen (für Interviewer)----------------------------------
meth <- read_dta(
  paste0(
    "./Input/",
    "infas_Methodendatensatz_WSI_4808_20150812",
    ".dta"
  )
)

sti <-  read_dta(
  paste0(
    "./Input/",
    "infas_Stichprobenmerkmale_WSI_4808_20150611",
    ".dta"
  )
)
