Datensatz_Intern$w08_abschnitt <- as.numeric(Datensatz_Intern$w08_abschnitt)
Datensatz_Intern$branche10 <- NA
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt <= 03
                   | (Datensatz_Intern$w08_abschnitt >= 90 &
                              Datensatz_Intern$w08_abschnitt <= 96)] <- 10
Datensatz_Intern$branche10[(Datensatz_Intern$w08_abschnitt >= 05 &
                            Datensatz_Intern$w08_abschnitt <= 24)      
                   | (Datensatz_Intern$w08_abschnitt >= 35 &
                              Datensatz_Intern$w08_abschnitt <= 39)] <- 1
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 25 & 
                           Datensatz_Intern$w08_abschnitt <= 33] <- 2
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 41 & 
                           Datensatz_Intern$w08_abschnitt <= 43] <- 3
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 45 & 
                           Datensatz_Intern$w08_abschnitt <= 47] <- 4
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 49 & 
                           Datensatz_Intern$w08_abschnitt <= 56] <- 5
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 58 & 
                           Datensatz_Intern$w08_abschnitt <= 63] <- 6
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 64 & 
                           Datensatz_Intern$w08_abschnitt <= 66] <- 7
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 68 & 
                           Datensatz_Intern$w08_abschnitt <= 82] <- 8
Datensatz_Intern$branche10[Datensatz_Intern$w08_abschnitt >= 85 & 
                           Datensatz_Intern$w08_abschnitt <= 88] <- 9
Datensatz_Intern$branche10 <- factor(Datensatz_Intern$branche10,
                             labels=c(
                                     "Bergbau / Prod. Gewerbe ohne Baugewerbe und ohne Investitionsg?ter",
                                     "Investitionsgüter",
                                     "Baugewerbe",
                                     "Handel",
                                     "Verkehr und Lagerei / Gastgewerbe",
                                     "Information und Kommunikation",
                                     "Finanz- und Versicherungsdienstleister",
                                     "Unternehmensnahe Dienstleistungen",
                                     "Öffentliche Dienstleistungen / Erziehung / Gesundheit",
                                     "Sonstige Branchen"
                             )
)
Datensatz_Intern$branche8 <- NA
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[1] | 
                          Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[2]] <- 1
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[3]] <- 2
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[4]] <- 3
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[5]] <- 4
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[6] | 
                          Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[7]] <- 5
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[8]] <- 6
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[9]] <- 7
Datensatz_Intern$branche8[Datensatz_Intern$branche10 == levels(Datensatz_Intern$branche10)[10]] <- 8
Datensatz_Intern$branche8 <- factor(Datensatz_Intern$branche8,
                            labels=c(
                                    "Bergbau / Prod. Gewerbe ohne Baugewerbe",
                                    "Baugewerbe",
                                    "Handel",
                                    "Verkehr und Lagerei / Gastgewerbe",
                                    "Information und Kommunikation / Finanz- und Versicherungsdienstleister",
                                    "Unternehmensnahe Dienstleistungen",
                                    "Öffentliche Dienstleistungen / Erziehung / Gesundheit",
                                    "Sonstige Branchen"
                            )
)
Datensatz_Intern$branche6 <- NA
Datensatz_Intern$branche6[Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[1] | 
                          Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[2]] <- 1
Datensatz_Intern$branche6[Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[3] | 
                          Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[4]] <- 2
Datensatz_Intern$branche6[Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[5]] <- 3
Datensatz_Intern$branche6[Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[6]] <- 4
Datensatz_Intern$branche6[Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[7]] <- 5
Datensatz_Intern$branche6[Datensatz_Intern$branche8 == levels(Datensatz_Intern$branche8)[8]] <- 6
Datensatz_Intern$branche6 <- factor(Datensatz_Intern$branche6,
                            labels=c(
                                    "Bergbau / Produzierendes Gewerbe",
                                    "Handel / Verkehr und Lagerei / Gastgewerbe",
                                    "Information und Kommunikation / Finanz- und Versicherungsdienstleister",
                                    "Unternehmensnahe Dienstleistungen",
                                    "Öffentliche Dienstleistungen / Erziehung / Gesundheit",
                                    "Sonstige Branchen"
                            )
)