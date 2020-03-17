# Gewichtungs- und Interviewerdaten mergen

# Gewichte----
gew <- gew[,c("lfd", "gewab_k", "gewab_l", "gewbr_k", "gewbr_l")]
rawdat <- left_join(rawdat, gew, by="lfd")

# Interviewerdaten---- 

## 1. Interviewernummer über Methodendatensatz----
meth <- meth[, c("lfd", "internr_n")]

rawdat <- left_join(rawdat, meth, by="lfd")

## 2. Ostwest & Bundesland über Stichprobenmerkmale----
 # Branche

sti$branche10 <- NA
sti$branche10[as.numeric(sti$w08_abschnitt) %in% c(01:03, 90:96)] <- 10
sti$branche10[as.numeric(sti$w08_abschnitt) %in% c(5:24, 35:39)] <- 1
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 25:33] <- 2
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 41:43] <- 3
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 45:47] <- 4
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 49:56] <- 5
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 58:63] <- 6
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 64:66] <- 7
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 68:82] <- 8
sti$branche10[as.numeric(sti$w08_abschnitt) %in% 85:88] <- 9
sti$branche10 <- labelled(
  sti$branche10, 
  c(
    "Bergbau / Prod. Gewerbe (WZ 5-24; 35-39)" = 1,
    "Investitionsgüter (WZ 25-33)" = 2,
    "Baugewerbe (WZ 41-43)" = 3,
    "Handel (WZ 45-47)" = 4,
    "Verkehr und Lagerei / Gastgewerbe (WZ 49-56)" = 5,
    "Information und Kommunikation (WZ 58-63)" = 6,
    "Finanz- und Versicherungsdienstleister (WZ 64-66)" = 7,
    "Unternehmensnahe Dienstleistungen (WZ 68-82)" = 8,
    "Öffentliche Dienstleistungen / Erziehung / Gesundheit (WZ 85-88)" = 9,
    "Sonstige Branchen (WZ 1-3; 90-96)" = 10
    ),
  label="Branche (WZ 2008)"
)

sti <- sti[, c("lfd", "westost", "branche10")]

rawdat <- left_join(rawdat, sti, by="lfd")

rm(list=c("gew", "meth", "int", "sti"))
