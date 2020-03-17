# Befragungsdaten bereinigen
# # Methodenvariablen löschen-----------------------------------------------------
methodenspalten2 <- c("splithalf", "Order", "S1", "S4", "Interviewdauer")

ms2 <- unique (grep(paste(methodenspalten2, collapse="|"), 
  names(dat), value=TRUE, ignore.case=T))

dat <- dat[,!(names(dat) %in% ms2)]

# Deanonymisierungsvariablen löschen--------------------------------------------
deanonym <- c("C0a", "C0b", "C0c",
  "C10", "I7", "P3_o_5", "Q", "S")

ds <- unique (grep(paste(deanonym, collapse="|"), 
  names(dat), value=TRUE, ignore.case=T))

dat <- dat[,!(names(dat) %in% ds)]

# Attribute setzen und überflüssige Objekte löschen-----------------------------
dat.varlabels <- dat.varlabels.alle[
  names(dat.varlabels.alle) %in% names(dat)]
dat.varlabels <- dat.varlabels[unique(
  names(dat.varlabels))]

dat.valuelabels <- dat.valuelabels.alle[
  names(dat.valuelabels.alle) %in% names(dat)]
dat.valuelabels <- dat.valuelabels[unique(
  names(dat.valuelabels))]

attr(dat, "label.table") <- dat.valuelabels
attr(dat, "var.labels") <-  dat.varlabels

# Workspace bereinigen
rm(list=c("dat.valuelabels", "dat.valuelabels.alle",
  "dat.varlabels", "dat.varlabels.alle",
  "deanonym", "ds", "methodenspalten2", "ms2"))