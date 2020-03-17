# Befragungsdaten bereinigen

# Methodenvariablen löschen
methodenspalten2 <- c("splithalf", "Order", "S1", "S4", "Interviewdauer")

ms2 <- unique (grep(paste(methodenspalten2, collapse="|"), 
                   names(dat), value=TRUE, ignore.case=T))

dat <- dat[,!(names(dat) %in% ms2)]

# Deanonymisierungsvariablen löschen
deanonym <- c("C0a", "C0b", "C0c",
              "C10", "I7", "P3_o_5", "Q", "S")

ds <- unique (grep(paste(deanonym, collapse="|"), 
                    names(dat), value=TRUE, ignore.case=T))

dat <- dat[,!(names(dat) %in% ds)]

# Anteile
for(b in dat.varlabels) {
        if (grepl("Anteil", b, ignore.case=T)) {
                dat[paste0(names(dat.varlabels)[dat.varlabels==b], "_gen")] <- NA
        }
} # geht noch nicht

