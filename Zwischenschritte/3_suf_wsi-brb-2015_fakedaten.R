# Fakedaten erzeugen
#rawdat <- safe
safe <- rawdat

# (1) Alles umwürfeln, was nicht Kurzversion ist.
# (2) Nicht unique Werte verschieben.
# (3) Filterstruktur nachbauen.

# (1) Fake-ID----
rawdat$lfd <- sample(1:nrow(rawdat), nrow(rawdat), replace=F)
attributes(rawdat$lfd) <- attributes(safe$lfd)

#  (2) umwandeln----

filter <- unname(sapply(rawdat, function(x) sum(is.na(x[safe$langkurz == 1])) > 0 | sum(is.na(x[safe$langkurz == 2])) > 2118))

set.seed(1234)

for(i in 2:ncol(rawdat)) {
  
  # durcheinanderwürfeln
  rawdat[[i]] <- 
    sample(
      safe[[i]][!is.na(safe[[i]])],
      size=length(safe[[i]]),
      replace=T)
  
  # offene Angaben vernichten
  if(str_detect(attributes(safe[[i]])$label, "offen")) {
    
    rawdat[[i]] <- NA
    
  }
  
  
  
  # unique?
  x <- rawdat %>% select(i) %>% group_by_at(1) %>% mutate(n=n()) %>% ungroup() 
  nas <- sum(is.na(safe[[i]]))
  kl <- min(x$n)-(min(x$n)*(nas+1)/nrow(safe))
  n <- 10+(1/((nrow(rawdat)-nas)/nrow(rawdat)))^2
  n[n > max(x$n[!is.na(x[,1])])] <- max(x$n[!is.na(x[,1])])
  
  rawdat[[i]][x$n < n] <- 
    sample(rawdat[[i]][x$n >= n],
           size=length(rawdat[[i]][x$n < n]),
           replace=T)
  
  # check if still unique...?
  n <- 5
  x1 <- rawdat %>% select(i) %>% group_by_at(1) %>% mutate(n=n()) %>% ungroup() 
  if(sum(x1$n < n) > 0) print(names(rawdat)[i]) # nur NAs
  
  if(all(is.na(safe[[i]][safe$langkurz==2])==T)) {
    
    rawdat[[i]][rawdat$langkurz == 2] <- NA
    
  }
  
  attributes(rawdat[[i]]) <- attributes(safe[[i]])
  
}

# (3) Filterstruktur----
write.csv2(names(rawdat)[which(filter)], "./Hintergrund/filterfuehrung.csv")

# (a) alle Kurzversion-Variablen (schon im Text)

# (b) alle anderen Filter----

rawdat$C0a[rawdat$C0_1 != 1] <- NA
rawdat$C0b[rawdat$C0_2 != 1] <- NA
rawdat$C0c[rawdat$C0_3 != 1] <- NA

rawdat$C1_1[rawdat$C1 != 999996] <- NA
rawdat$C1a[rawdat$C1 == 0 | rawdat$C1_1 == 0] <- NA

rawdat$D1aa[rawdat$D1a != 999996] <- NA

# Beschgruppen
rawdat$D1b_1_1[rawdat$D1b_1 == 2] <- NA
rawdat$D1bb_1_1[rawdat$D1b_1 == 2 | rawdat$D1b_1_1 != 999996] <- NA

rawdat$D1b_1_2[rawdat$D1b_2 == 2] <- NA
rawdat$D1bb_1_2[rawdat$D1b_2 == 2 | rawdat$D1b_1_2 != 999996] <- NA

rawdat$D1b_1_3[rawdat$D1b_3 == 2] <- NA
rawdat$D1bb_1_3[rawdat$D1b_3 == 2 | rawdat$D1b_1_3 != 999996] <- NA

rawdat$D1b_1_4[rawdat$D1b_4 == 2] <- NA
rawdat$D1bb_1_4[rawdat$D1b_4 == 2 | rawdat$D1b_1_4 != 999996] <- NA

rawdat$D1b_1_5[rawdat$D1b_5 == 2] <- NA
rawdat$D1bb_1_5[rawdat$D1b_5 == 2 | rawdat$D1b_1_5 != 999996] <- NA

rawdat$D1b_1_7[rawdat$D1b_7 == 2] <- NA
rawdat$D1bb_1_7[rawdat$D1b_7 == 2 | rawdat$D1b_1_7 != 999996] <- NA

# Fremdfirmen
rawdat$D1c_1_1[rawdat$D1c_1 == 2] <- NA
rawdat$D1cc_1_1[rawdat$D1c_1 == 2 | rawdat$D1c_1_1 != 999996] <- NA

rawdat$D1c_1_2[rawdat$D1c_2 == 2] <- NA
rawdat$D1cc_1_2[rawdat$D1c_2 == 2 | rawdat$D1c_1_2 != 999996] <- NA

rawdat$D1c_1_3[rawdat$D1c_3 == 2] <- NA
rawdat$D1cc_1_3[rawdat$D1c_3 == 2 | rawdat$D1c_1_3 != 999996] <- NA

rawdat$D1c_2_3[!(rawdat$D1c_3 == 1)] <- NA

# Quali
rawdat$D1d_1_1[!(rawdat$D1d_1 == 1)] <- NA
rawdat$D1dd_1_1[!(rawdat$D1d_1 == 1)| rawdat$D1d_1_1 != 999996] <- NA

rawdat$D1d_1_2[!(rawdat$D1d_2 == 1)] <- NA
rawdat$D1dd_1_2[!(rawdat$D1d_2 == 1) | rawdat$D1d_1_2 != 999996] <- NA

rawdat$D1d_1_3[!(rawdat$D1d_3 == 1)] <- NA
rawdat$D1dd_1_3[!(rawdat$D1d_3 == 1) | rawdat$D1d_1_3 != 999996] <- NA

# Alter
rawdat$D1ee_1[rawdat$D1e != 999996] <- NA
rawdat$D1f[rawdat$D1e >= rawdat$D1 & rawdat$D1e < 999996] <- NA
rawdat$D1ff_1[rawdat$D1f != 999996 | is.na(rawdat$D1f)] <- NA

# Migration
rawdat$D1gg_1[rawdat$D1g != 999996] <- NA

# Stellenabbau
rawdat$D2a_1[rawdat$D2 != 1] <- NA
rawdat$D2a_2[rawdat$D2 != 1] <- NA
rawdat$D2a_3[rawdat$D2 != 1] <- NA
rawdat$D2a_4[rawdat$D2 != 1] <- NA
rawdat$D2a_5[rawdat$D2 != 1] <- NA
rawdat$D2a_6[rawdat$D2 != 1] <- NA
rawdat$D2a_7[rawdat$D2 != 1] <- NA
rawdat$D2a_8[rawdat$D2 != 1] <- NA
rawdat$D2a_9[rawdat$D2 != 1] <- NA

# D4
rawdat$D4a_1[rawdat$D4 != 1] <- NA
rawdat$D4a_2[rawdat$D4 != 1] <- NA
rawdat$D4a_3[rawdat$D4 != 1] <- NA
rawdat$D4a_4[rawdat$D4 != 1] <- NA
rawdat$D4a_5[rawdat$D4 != 1] <- NA
rawdat$D4a_6[rawdat$D4 != 1] <- NA
rawdat$D4a_7[rawdat$D4 != 1] <- NA
rawdat$D4a_9[rawdat$D4 != 1] <- NA
rawdat$D4a_10[rawdat$D4 != 1] <- NA

# D5
rawdat$D5a[!(rawdat$D5_1 == 1 | rawdat$D5_2 == 1 | rawdat$D5_3 == 1)] <- NA

# Altersvorsorge
rawdat$F2[rawdat$F1 != 1] <- NA
rawdat$F3[rawdat$F1 != 1] <- NA
rawdat$F3_1[rawdat$F1 != 1 | rawdat$F3 != 999996] <- NA

# Arbeitszeiten
rawdat$H3a_1[rawdat$H3A != 1] <- NA
rawdat$H3b_1[rawdat$H3B != 1] <- NA
rawdat$H3c_1[rawdat$H3C != 1] <- NA
rawdat$H3d_1[rawdat$H3D != 1] <- NA
rawdat$H3e_1[rawdat$H3E != 1] <- NA

rawdat$H4a_1[rawdat$H4 != 1] <- NA
rawdat$H4a_2[rawdat$H4 != 1] <- NA

rawdat$H5a[rawdat$H5 != 1] <- NA
rawdat$H6a[rawdat$H6 != 1] <- NA

# I: Gesundheit
rawdat$I2a[rawdat$I2 != 1] <- NA
rawdat$I2b[rawdat$I2 != 1] <- NA

rawdat$I3a_1[!(rawdat$I3_1 < 3 & rawdat$I3_7 >= 3)] <- NA
rawdat$I3a_2[!(rawdat$I3_1 < 3 & rawdat$I3_7 >= 3)] <- NA
rawdat$I3a_3[!(rawdat$I3_1 < 3 & rawdat$I3_7 >= 3)] <- NA
rawdat$I3a_4[!(rawdat$I3_1 < 3 & rawdat$I3_7 >= 3)] <- NA
rawdat$I3a_5[!(rawdat$I3_1 < 3 & rawdat$I3_7 >= 3)] <- NA

rawdat$I3b_1[!(rawdat$I3_1 >= 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3b_2[!(rawdat$I3_1 >= 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3b_3[!(rawdat$I3_1 >= 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3b_4[!(rawdat$I3_1 >= 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3b_5[!(rawdat$I3_1 >= 3 & rawdat$I3_7 < 3)] <- NA

rawdat$I3c_1[!(rawdat$I3_1 < 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3c_2[!(rawdat$I3_1 < 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3c_3[!(rawdat$I3_1 < 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3c_4[!(rawdat$I3_1 < 3 & rawdat$I3_7 < 3)] <- NA
rawdat$I3c_5[!(rawdat$I3_1 < 3 & rawdat$I3_7 < 3)] <- NA

rawdat$I5_1[!(rawdat$I3z == 1)] <- NA
rawdat$I5_2[!(rawdat$I3z == 1)] <- NA
rawdat$I5_3[!(rawdat$I3z == 1)] <- NA
rawdat$I5_4[!(rawdat$I3z == 1)] <- NA

rawdat$I9[rawdat$I8 != 1] <- NA
rawdat$I10[rawdat$I8 != 1 | rawdat$I9 == 0] <- NA

rawdat$I11a[rawdat$I11 != 1] <- NA

rawdat$I12_1[!(rawdat$I11a %in% c(2,3))] <- NA
rawdat$I12_2[!(rawdat$I11a %in% c(2,3))] <- NA
rawdat$I12_3[!(rawdat$I11a %in% c(2,3))] <- NA
rawdat$I12_4[!(rawdat$I11a %in% c(2,3))] <- NA
rawdat$I12_5[!(rawdat$I11a %in% c(2,3))] <- NA
rawdat$I12_6[!(rawdat$I11a %in% c(2,3))] <- NA
rawdat$I12_7[!(rawdat$I11a %in% c(2,3))] <- NA

# Löhne
rawdat$J4a_1[rawdat$J4 != 1] <- NA
rawdat$J4a_2[rawdat$J4 != 1] <- NA
rawdat$J4a_3[rawdat$J4 != 1] <- NA

rawdat$J5a[rawdat$J5 != 1] <- NA
rawdat$J5b[rawdat$J5 != 1] <- NA

# Tarif und BV
rawdat$K2a_1[rawdat$K2 != 1] <- NA
rawdat$K2a_2[rawdat$K2 != 1] <- NA

rawdat$K3_1[!(rawdat$K1 == 2 & rawdat$K2 == 2)] <- NA
rawdat$K3_2[!(rawdat$K1 == 2 & rawdat$K2 == 2)] <- NA

rawdat$K4[!(rawdat$K1 == 1 | rawdat$K2 == 1)] <- NA

rawdat$K4a_1[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K4 != 1] <- NA
rawdat$K4a_2[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K4 != 1] <- NA
rawdat$K4a_3[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K4 != 1] <- NA
rawdat$K4a_4[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K4 != 1] <- NA

rawdat$K5[!(rawdat$K1 == 1 | rawdat$K2 == 1)] <- NA

rawdat$K5a_1[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K5 != 1] <- NA
rawdat$K5a_2[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K5 != 1] <- NA
rawdat$K5a_3[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K5 != 1] <- NA
rawdat$K5a_4[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K5 != 1] <- NA
rawdat$K5a_5[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K5 != 1] <- NA

rawdat$K6[!(rawdat$K1 == 1 | rawdat$K2 == 1)] <- NA

rawdat$K6a_1[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K6 != 1] <- NA
rawdat$K6a_2[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K6 != 1] <- NA
rawdat$K6a_3[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K6 != 1] <- NA
rawdat$K6a_4[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K6 != 1] <- NA
rawdat$K6a_5[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K6 != 1] <- NA
rawdat$K6a_6[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K6 != 1 |
               (rawdat$K6a_1 == 2 & rawdat$K6a_2 == 2 & rawdat$K6a_3 == 2 &
                  rawdat$K6a_4 == 2 & rawdat$K6a_5 == 2)] <- NA

rawdat$K7[!(rawdat$K1 == 1 | rawdat$K2 == 1)] <- NA

rawdat$K7a_1[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K7 != 1] <- NA
rawdat$K7a_2[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K7 != 1] <- NA
rawdat$K7a_3[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K7 != 1] <- NA
rawdat$K7a_4[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K7 != 1] <- NA
rawdat$K7a_5[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K7 != 1] <- NA
rawdat$K7a_6[!(rawdat$K1 == 1 | rawdat$K2 == 1) | rawdat$K7 != 1 |
               (rawdat$K7a_1 == 2 & rawdat$K7a_2 == 2 & rawdat$K7a_3 == 2 &
                  rawdat$K7a_4 == 2 & rawdat$K7a_5 == 2)] <- NA

rawdat$K8[!(rawdat$K1 == 2 & rawdat$K2 == 2)] <- NA
rawdat$K8a[!(rawdat$K1 == 2 & rawdat$K2 == 2) | !(rawdat$K8 %in% c(1,2))] <- NA

rawdat$K9[!(rawdat$K1 == 2 & rawdat$K2 == 2) | !(rawdat$K8 %in% c(1,2))] <- NA

rawdat$K9a_1[rawdat$K9 != 1 | is.na(rawdat$K9) == T] <- NA
rawdat$K9a_2[rawdat$K9 != 1 | is.na(rawdat$K9) == T] <- NA
rawdat$K9a_3[rawdat$K9 != 1 | is.na(rawdat$K9) == T] <- NA
rawdat$K9a_4[rawdat$K9 != 1 | is.na(rawdat$K9) == T |
               (rawdat$K9a_1 == 2 & rawdat$K9a_2 == 2 & rawdat$K9a_3) ] <- NA

for(j in 1:28) {
  rawdat[[paste0("K11_", j)]][!(rawdat$K10 == 1) %in% T] <- NA
  rawdat[[paste0("K11a_", j)]][!(rawdat[[paste0("K11_", j)]] == 1) %in% T] <- NA
}

rawdat$K12[!(rawdat$K10 == 1) %in% T] <- NA
rawdat$K12a_1[!(rawdat$K12 > 0 & rawdat$K12 < 997  & (
  rawdat$C0_1 == 1 | rawdat$C0_2 == 1 | rawdat$C0_3 == 1
)) %in% T] <- NA
rawdat$K12a_2[!(rawdat$K12 > 0 & rawdat$K12 < 997  & (
  rawdat$C0_1 == 1 | rawdat$C0_2 == 1 | rawdat$C0_3 == 1
)) %in% T] <- NA

rawdat$K12b_1_1[!(rawdat$K12 > 0  & rawdat$K12 < 997  & (
  rawdat$C0_1 == 1 | rawdat$C0_2 == 1 | rawdat$C0_3 == 1
)) %in% T] <- NA
rawdat$K12b_1_2[!(rawdat$K12 > 0  & rawdat$K12 < 997  & (
  rawdat$C0_1 == 1 | rawdat$C0_2 == 1 | rawdat$C0_3 == 1
)) %in% T] <- NA

rawdat$K12b_2[!(rawdat$K12 > 0  & rawdat$K12 < 997  & is.na(rawdat$K12b_1_1) == F) %in% T] <- NA

rawdat$K12c[!(rawdat$K12 > 0) %in% T] <- NA

# Streik
rawdat$L2_1[!(rawdat$L1 %in% c(1,2)) %in% T] <- NA
rawdat$L2_2[!(rawdat$L1 %in% c(1,2)) %in% T] <- NA
rawdat$L2_3[!(rawdat$L1 %in% c(1,2)) %in% T] <- NA
rawdat$L2_4[!(rawdat$L1 %in% c(1,2)) %in% T] <- NA

# BR
rawdat$M4[!(rawdat$D1a > 0 | rawdat$D1aa > 0) %in% T] <- NA
rawdat$M5a[!(rawdat$C1 > 0 | rawdat$C1_1 > 0) %in% T] <- NA
rawdat$M5b[!(rawdat$M5a > 0 & rawdat$M5a < 97) %in% T] <- NA
rawdat$M6[!(rawdat$D1d_1 == 1) %in% T] <- NA
rawdat$M6a[((rawdat$M6 >= rawdat$M3) & rawdat$M6 < 997) | rawdat$D1d_2 != 1] <- NA
rawdat$M6b[!(rawdat$D1b_2 == 1) %in% T] <- NA
rawdat$M6c[!(rawdat$D1b_4 == 1) %in% T] <- NA
rawdat$M6e[!(rawdat$D1g > 0 | rawdat$D1gg_1 > 0) %in% T] <- NA
rawdat$M6f[!(rawdat$D1e > 0 | rawdat$D1ee_1 > 0) %in% T] <- NA
rawdat$M6g[!(rawdat$D1f > 0 | rawdat$D1ff_1 > 0) %in% T] <- NA
rawdat$M10[!(rawdat$C0d) %in% T] <- NA

rawdat$M11a[((rawdat$M11 >= rawdat$M3) & rawdat$M11 < 997) | is.na(rawdat$M11)] <- NA
rawdat$M11b[((rawdat$M11 + rawdat$M11a >= rawdat$M3) & rawdat$M11 < 997 & rawdat$M11 < 997) | is.na(rawdat$M11) | is.na(rawdat$M11a)] <- NA

# Verhältnis
rawdat$N2a[!(rawdat$N2 %in% c(1,2)) %in% T] <- NA
rawdat$N5a[!(rawdat$N5 == 1) %in% T] <- NA
rawdat$N6a_1[!(rawdat$N6 == 1) %in% T] <- NA

rawdat$P3a_1[!(rawdat$P3_1 == 1) %in% T] <- NA
rawdat$P3a_2[!(rawdat$P3_1 == 1) %in% T] <- NA
rawdat$P3a_3[!(rawdat$P3_1 == 1) %in% T] <- NA
rawdat$P3a_4[!(rawdat$P3_1 == 1) %in% T] <- NA
rawdat$P3a_5[!(rawdat$P3_1 == 1) %in% T] <- NA
rawdat$P3a_6[!(rawdat$P3_1 == 1) %in% T] <- NA
rawdat$P3a_7[!(rawdat$P3_1 == 1) %in% T] <- NA

# ZP usw
rawdat$Q7[!(rawdat$Q6 == 1) %in% T] <- NA

rawdat$R1a[!(rawdat$R1 == 1) %in% T] <- NA
rawdat$R1b[!(rawdat$R1 == 1) %in% T] <- NA
rawdat$R1c[!(rawdat$R1 == 1) %in% T] <- NA

rawdat$S2[!(rawdat$S1 == 1) %in% T] <- NA

# check----


# Minima
minima <- data.frame(
  var = NA, min = NA, value = NA
)

num <- 0

for(i in 2:ncol(rawdat)) {
  
  # check if still unique...?
  n <- 10
  x1 <- rawdat %>% select(i) %>% group_by_at(1) %>% mutate(n=n()) %>% ungroup() 
  if(sum(x1$n < n) > 0) {
    num <- num + 1
    minima[num,1] <- names(rawdat)[i]
    minima[num,2] <- min(table(rawdat[[i]]))
    minima[num,3] <- names(which(table(rawdat[[i]])==min(table(rawdat[[i]]))))[1]
    
  }
  
}

write.csv2(minima, paste0("./Output/Berichte/", Sys.Date(), "_minima.csv"))

minima

# Verteilungen

wb <- createWorkbook()
addWorksheet(wb, "summary")
num <- 1
name <- createStyle(fontSize=12, fontColour="blue", textDecoration = "bold")


for(i in 1:ncol(rawdat)) {
  
  writeData(wb, "summary", startCol = 1, startRow = num, x=names(safe)[i])
  addStyle(wb, "summary", style=name, rows=num, cols=1)
  
  num <- num+1
  
  writeData(wb, "summary", startCol = 1, startRow = num, x=t(summary(safe[[i]])))
  
  num <- num + 3
  
  writeData(wb, "summary", startCol = 1, startRow = num, x=t(summary(rawdat[[i]])))
  
  num <- num + 3
  
}

saveWorkbook(wb, paste0("./Output/Berichte/", Sys.Date(), "_pruefung.xlsx"), overwrite=T)

write_dta(rawdat, paste0("./Output/Datensaetze/", Sys.Date(), "_KDFV_WSI-Betriebsraetebefragung_2015.dta"))
write_sav(rawdat, paste0("./Output/Datensaetze/", Sys.Date(), "_KDFV_WSI-Betriebsraetebefragung_2015.sav"))

abweichungen <- data.frame(name=NA, safe=NA, rawdat=NA)
num <- 0

for(i in 1:ncol(rawdat)) {
  
  x <- sum(is.na(safe[[i]]))
  y <- sum(is.na(rawdat[[i]]))
  
  if(abs((x-y)/(x+0.01)) > 0.1 ) {
    
    num <- num+1
    abweichungen[num, 1] <- names(safe)[i]
    abweichungen[num, 2] <- x
    abweichungen[num, 3] <- y
    
    
  }
  
}

write.csv2(abweichungen, paste0("./Output/Berichte/", Sys.Date(), "_abweichungen.csv"))
