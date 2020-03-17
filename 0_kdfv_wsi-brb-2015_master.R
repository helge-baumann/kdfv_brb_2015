############
# Masterfile zur Erzeugung eines SUF der WSI-Betriebsraetebefragung 2015
# Ersteller: Helge Emmler, WSI
# Letztes Update: 13.03.2020
############

starttime <- Sys.time()

# Präambel ---------------------------------------------------------------------

## Pakete ggfs. installieren und laden
if(!"pacman" %in% installed.packages()) install.packages("pacman")
library(pacman)
p_load("stringr", "xtable", "dplyr", "haven", "stringi", "filesstrings", 
       "tools", "pdftools", "openxlsx") 

source("./R_Funk/funktionen_suf.R", encoding="UTF-8")

# Schritte in ./Zwischenschritte ausführen--------------------------------------
n <- 1:7
sapply(dir("./Zwischenschritte", full.names=T)[n], source, encoding="UTF-8")

# SessionInfo erzeugen ---------------------------------------------------------
dir.create("./sessionInfo", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  paste0(
    "./sessionInfo/",
    format(Sys.time(), "%Y-%m-%d_%H%M%S"),
    "_sessionInfo.txt")
)

# Ältere Dateien verschieben----------------------------------------------------
dirs <- 
  c("./Output/Berichte/bin", "./Output/Datensaetze/bin", "./sessionInfo/bin")
sapply(dirs, dir.create, showWarnings=F)

sapply(str_remove(dirs, "/bin"), moving)

# Identische Kopien löschen-----------------------------------------------------
sapply(dirs, delete.identical, automate=T, all=T)

# Workspace bereinigen
rm(list=setdiff(ls(), c("sufdat", "rawdat", lsf.str())))

endtime <- Sys.time()
endtime - starttime

