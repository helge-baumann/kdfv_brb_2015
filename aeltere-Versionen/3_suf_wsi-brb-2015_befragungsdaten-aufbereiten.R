# Befragungsdaten aufbereiten

# Gewichte zuspielen------------------------------------------------------------
gew <- daten_input[["gew"]][,c("lfd", "gewbr_k", "gewbr_l")]
dat <- merge(daten_input[["dat"]], gew, by="lfd")

# Variablenlabels und Valuelabels anfügen
dat.varlabels.alle <- c(varlabels[["dat"]], varlabels[["gew"]])
dat.valuelabels.alle <- append(valuelabels[["dat"]], valuelabels[["gew"]])

rm(list=c("gew"))

# Anteilswerte----------------------------------------------------------------

# Dokumentation generierter Variablen
Doku_gen <- list(NA)

for(b in names(dat)) {
  
  varlabel <- dat.varlabels.alle[b]
  valuelabel <- dat.valuelabels.alle[[b]]
        
  if (grepl("anteil", varlabel, ignore.case=T)) {

    Doku_gen[[paste0(b, "_gen")]] <- matrix(NA)
    Doku_gen[[paste0(b, "_gen")]][1] <- b
  
    # Hauptvariable erzeugen
    z <- dat[,b]
                
    # Vorhandensein übernehmen
    if (grepl(
      "vorhandensei", 
      varlabels[["dat"]][which(names(dat.varlabels.alle)==b)-2], 
      ignore.case=T)) {
          
      Doku_gen[[paste0(b, "_gen")]][2] <- names(dat)[which(names(dat)==b)-2]        
      x <- dat[,which(names(dat)==b)-2]   
                
      # Nein: Anteil=0
      z[tolower(x) == "nein"] <- 0
      # Missings übernehmen
      z[tolower(x) %in% 
        tolower(names(valuelabel))] <- 
      valuelabel[
        as.character(
          x[tolower(x) %in% 
          tolower(names(valuelabel))
          ])]
                
      # Spalte entfernen
      dat <- dat[,-(which(names(dat)==b)-2)]
    }
                
    # Anzahl übernehmen
    y <- dat[,which(names(dat)==b)-1] 
    y.name <- names(dat)[which(names(dat)==b)-1]
                
    # Anteil berechnen aus Betriebsgröße
    z[
      !(y %in% valuelabels[["dat"]][[y.name]]) &
      !(dat$D1 %in% valuelabels[["dat"]][["D1"]]) &
        is.na(y) == F & is.na(dat$D1)==F] <- 
    (y[
      !(y %in% valuelabels[["dat"]][[y.name]]) &
        !(dat$D1 %in% valuelabels[["dat"]][["D1"]]) &
        is.na(y) == F & is.na(dat$D1)==F]/
    dat$D1[
      !(y %in% valuelabels[["dat"]][[y.name]]) &
        !(dat$D1 %in% valuelabels[["dat"]][["D1"]]) &
        is.na(y) == F & is.na(dat$D1)==F]
    )*100
                
    # Missings übernehmen
    # a) aus absoluten Angaben
                
    labtable.y <- valuelabels[["dat"]][[y.name]]
    varunique.y <- na.omit(unique(y))
    names(varunique.y) <- as.character(varunique.y)
    gen.lab.y  <- sort(c(varunique.y[!varunique.y %in% labtable.y], labtable.y))
    y.fac <- factor(y, levels=gen.lab.y, labels=names(gen.lab.y)) 
                     
    labtable.d1 <- valuelabels[["dat"]][["D1"]]
    varunique.d1  <- na.omit(unique(dat$D1))
    names(varunique.d1) <- as.character(varunique.d1)
    gen.lab.d1  <- sort(c(varunique.d1[!varunique.d1 %in% labtable.d1], labtable.d1))
    d1.fac <- factor(dat$D1, levels=gen.lab.d1, labels=names(gen.lab.d1))    
    
    z[
      tolower(y.fac) %in% 
          tolower(names(valuelabel))] <- 
    valuelabel[
      as.character(
        y.fac[
          tolower(y.fac) %in% 
            tolower(names(valuelabel))])]
                
    z[
      tolower(d1.fac) %in% 
          tolower(names(valuelabel))==T &
          is.na(z)==T & is.na(d1.fac)==F
        ] <- 
      valuelabel[
        as.character(
          d1.fac[tolower(d1.fac) %in% 
              tolower(names(valuelabel))==T &
              is.na(z)==T & is.na(d1.fac)==F
            ])]
    
    # Unplausible Werte erkennen
   z[z > 100 & !(z %in% valuelabel) & is.na(z)==F] <- 
     valuelabel[length(valuelabel)]+1
     
    Doku_gen[[paste0(b, "_gen")]][3] <- names(dat)[which(names(dat)==b)-1] 
    Doku_gen[[paste0(b, "_gen")]][4] <- "D1"
    
    # Spalte n-1 entfernen
    dat <- dat[,-(which(names(dat)==b)-1)]
                
    dat[,b] <- z
    names(dat)[which(names(dat)==b)] <- paste0(b, "_gen")
    
    dat.valuelabels.alle[[paste0(b, "_gen")]] <- valuelabel
    dat.valuelabels.alle[[paste0(b, "_gen")]][length(valuelabel)+1] <- 
      dat.valuelabels.alle[[paste0(b, "_gen")]][length(valuelabel)]+1
    
    names(dat.valuelabels.alle[[paste0(b, "_gen")]])[length(valuelabel)+1] <-
      "unplausibler Wert (Anteil > 1)"
    
    dat.varlabels.alle[paste0(b, "_gen")] <- 
      paste0(dat.varlabels.alle[b], " (generierte Variable)")
    
                
    }
} 

# Internen WSI-Datensatz erzeugen

# Stichprobenmerkmale zuspielen-------------------------------------------------
dat <- merge(dat, daten_input[["sti"]], by="lfd")

# Variablenlabels und Valuelabels anfügen
dat.varlabels.alle <- c(dat.varlabels.alle, varlabels[["sti"]])
dat.valuelabels.alle <- append(dat.valuelabels.alle, valuelabels[["sti"]])

Datensatz_Intern <- dat 

source("./R_Funk/systematisierung_wz.R", encoding="UTF-8")

Datensatz_Intern <- Datensatz_Intern[,!(names(dat) %in% "w08_abschnitt")]


branche.varlabels <- NA
branche.varlabels["branche6"] <- 
  "Branche nach WZ 2008 (6er-Vergröberung)"
branche.varlabels["branche8"] <- 
  "Branche nach WZ 2008 (8er-Vergröberung)"
branche.varlabels["branche10"] <- 
  "Branche nach WZ 2008 (10er-Vergröberung)"

branche.valuelabels <- list(NA)
branche.valuelabels[["branche6"]] <-
  1:6
names(branche.valuelabels[["branche6"]]) <- 
  levels(Datensatz_Intern$branche6)
branche.valuelabels[["branche8"]] <-
  1:8
names(branche.valuelabels[["branche8"]]) <- 
  levels(Datensatz_Intern$branche8)
branche.valuelabels[["branche10"]] <-
  1:10
names(branche.valuelabels[["branche10"]]) <- 
  levels(Datensatz_Intern$branche10)

# Variablenlabels und Valuelabels anfügen
dat.varlabels.alle <- c(dat.varlabels.alle, branche.varlabels)
dat.valuelabels.alle <- append(dat.valuelabels.alle, branche.valuelabels)

rm(list=c("branche.varlabels", "branche.valuelabels"))

Datensatz.varlabels <- dat.varlabels.alle[
  names(dat.varlabels.alle) %in% names(Datensatz_Intern)]
Datensatz.varlabels <- Datensatz.varlabels[unique(
  names(Datensatz.varlabels))]

Datensatz.valuelabels <- dat.valuelabels.alle[
  names(dat.valuelabels.alle) %in% names(Datensatz_Intern)]
Datensatz.valuelabels <- Datensatz.valuelabels[unique(
  names(Datensatz.valuelabels))]

attr(Datensatz_Intern, "label.table") <- Datensatz.valuelabels
attr(Datensatz_Intern, "var.labels") <-  Datensatz.varlabels # namen falsch sortiert



dir.create("./Output/Befragungsdaten intern/")

# für Paket haven
  
for(b in names(Datensatz_Intern)) {
  
  if(class(Datensatz_Intern[[b]]) == "factor") {
    
    Datensatz_Intern[[b]] <- 
      round(as.double(get.origin.codes(
        Datensatz_Intern[[b]], 
        label.table=attr(Datensatz_Intern, 
                         "label.table")[[b]])), digits=0)
    
    for(i in attr(Datensatz_Intern, 
                  "label.table")[[b]]) {
      
      val_label(Datensatz_Intern[[b]], i) <- 
        names(attr(Datensatz_Intern, 
                   "label.table")[[b]][attr(Datensatz_Intern, 
                                            "label.table")[[b]]==i])
      
      na_values(Datensatz_Intern[[b]]) <-
        round(as.double(unname(attr(Datensatz_Intern, "label.table")[[b]][
          names(attr(Datensatz_Intern, 
                     "label.table")[[b]]) %in% c("verweigert", "weiß nicht",
                                                 "unplausibler Wert (Anteil > 1)")])), 
          digits=0)
      
      
      
    }
    
    #Datensatz_Intern[[b]] <- as_factor(Datensatz_Intern[[b]])
    #attr(Datensatz_Intern[[b]], "labels") <- attr(Datensatz_Intern, "label.table")[[b]]
                                
    } else { # numerische Variablen
      
      Datensatz_Intern[[b]] <- as.double(Datensatz_Intern[[b]])
      
    for(i in attr(
      Datensatz_Intern, 
      "label.table")[[b]]
      ) {
      
      val_label(Datensatz_Intern[[b]], i) <- 
        names(
          attr(
            Datensatz_Intern, 
            "label.table")[[b]][
              attr(
                Datensatz_Intern, 
                "label.table")[[b]]==i]
          )
      
      na_values(Datensatz_Intern[[b]]) <- 
        as.double(unname(attr(Datensatz_Intern, 
                    "label.table")[[b]]))
      
      }
  }
  
  var_label(Datensatz_Intern[b]) <-
    attr(Datensatz_Intern, "var.labels")[b]
  
}

write_sav(Datensatz_Intern, 
          paste0(
            "./Output/Befragungsdaten intern/",
            "WSI-BRB-2015_Befragungsdaten-intern",
            format(Sys.time(), "%Y-%m-%d"), ".sav"
          )
)

# Stata-Support fehlt noch.
  # Problem: Stata kann keine Labels für Nachkommastellen vergeben.
  # Dies gilt allerdings nur für die gelabelten Werte selbst
  # siehe https://github.com/tidyverse/haven/commit/5010f44ebf24797a75d4acc0f4ddde873c624465
  # und https://github.com/tidyverse/haven/issues/343


Fertige_Exportdaten[["Befragungsdaten_intern"]] <- Datensatz_Intern

#rm(list=c("Datensatz_Intern", "Datensatz.varlabels", "Datensatz.valuelabels",
  #"dat.varlabels", "dat.valuelabels",
  #"d1.fac", "gen.lab.d1", "gen.lab.y",
  #"labtable.d1", "labtable.y",
  #"valuelabel", "varlabel",
  #"varunique.d1", "varunique.y",
  #"x", "y", "z",
  #"y.fac", "y.name", "b"))




