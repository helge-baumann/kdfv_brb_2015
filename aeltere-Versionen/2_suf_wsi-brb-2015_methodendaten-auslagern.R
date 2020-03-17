# Umfassenden Methodendatensatz erzeugen (ausgelagert)

# Basidaten (höchste Detailstufe: kon)------------------------------------------
Methodendaten <- daten_input[["kon"]]

# Datensätze mergen
Methodendaten <- merge(Methodendaten, daten_input[["int"]], 
                       by.x="k_intnr_n", by.y="Internr_n", all.x=T)
Methodendaten <- merge(Methodendaten, daten_input[["met"]], by="lfd", all.x=T)
#Methodendaten <- merge(Methodendaten, sti, by="lfd", all.x=T)
Methodendaten <- merge(Methodendaten, daten_input[["gew"]], by="lfd", all.x=T)

# Variablen aus Befragungsdatensatz
methodenspalten <- c("lfd", "splithalf", "Order", "S1", "S4", "Interviewdauer")

ms <- unique (grep(paste(methodenspalten, collapse="|"), 
                   names(daten_input[["dat"]]), value=TRUE, ignore.case=T))

md <- daten_input[["dat"]][,ms]
Methodendaten <- merge(Methodendaten, md, by="lfd", all.x=T)
          
# Doppelte Spalten entfernen----------------------------------------------------
dupl <- c(".y", "internr_n")
dupl <- unique (grep(paste(dupl, collapse="|"), 
                     names(Methodendaten), value=TRUE, ignore.case=T))

Methodendaten <- Methodendaten[,!(names(Methodendaten) %in% dupl)]

# Namen mit ".x" ersetzen.
names(Methodendaten)[grep(".x", names(Methodendaten))] <-
        gsub(".x", "", names(Methodendaten)[grep(".x", names(Methodendaten))])

# Variablenlabels und Valuelabels anfügen---------------------------------------
Methodendaten.valuelabels.alle <- append(valuelabels[["dat"]], valuelabels[["int"]])
Methodendaten.valuelabels.alle <- append(Methodendaten.valuelabels.alle, 
                                         valuelabels[["kon"]])
Methodendaten.valuelabels.alle <- append(Methodendaten.valuelabels.alle, 
                                         valuelabels[["met"]])

Methodendaten.valuelabels.alle <- append(Methodendaten.valuelabels.alle, 
                                         valuelabels[["gew"]])

Methodendaten.valuelabels <- Methodendaten.valuelabels.alle[
        names(Methodendaten.valuelabels.alle) %in% names(Methodendaten)]
Methodendaten.valuelabels <- Methodendaten.valuelabels[unique(
        names(Methodendaten.valuelabels))]

Methodendaten.varlabels.alle <- c(varlabels[["dat"]], varlabels[["met"]], varlabels[["kon"]], 
                                  varlabels[["int"]], varlabels[["gew"]])
Methodendaten.varlabels <- Methodendaten.varlabels.alle[
        names(Methodendaten.varlabels.alle) %in% names(Methodendaten)]
Methodendaten.varlabels <- Methodendaten.varlabels[unique(
        names(Methodendaten.varlabels))]

# Attribute setzen und überflüssige Objekte löschen-----------------------------
attr(Methodendaten, "label.table") <- Methodendaten.valuelabels
attr(Methodendaten, "var.labels") <-  Methodendaten.varlabels

Fertige_Exportdaten[["Methodendaten"]] <- Methodendaten

rm(list = c())

