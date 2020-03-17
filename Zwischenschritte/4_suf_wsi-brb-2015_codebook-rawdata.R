# für Link zwischen Roh und SUF----
options(encoding="Windows-1252")
fr_b <- read.table("./Hintergrund/bereinigung_suf.txt", sep="\t", header=T)[1:3]
fr_b[,2] <- stri_enc_toutf8(fr_b[,2])
options(encoding="UTF-8")

Codebook_Rawdata <- data.frame(
  "Frage" = NA,
  "Bezeichnung und Labels" = NA,
  "Schlüssel" = NA,
  "n" = NA, 
  "Frage" = NA
)

# Startzähler (für die Zeilen)
num <- 0
#x <- 0 # notwendig?

for(i in 1:ncol(rawdat)) {
  
  num <- num+1
  
  # Variablenname mit Link
  Codebook_Rawdata[num,1] <- paste0(
    "\\textbf{", names(rawdat)[i], "}",
    "\\phantomsection\\label{var:",
    str_replace_all(names(rawdat)[i], fixed("_"), ":"),
    "}"
  )
  
  # Variablenlabel laut Datensatz (mit Fehlern)
  Codebook_Rawdata[num,2] <- paste0(
    "\\textbf{", attr(rawdat[[i]], "label"), "}"
  )
  
  frage  <- fb_link$fragen[fb_link$vars == names(rawdat)[i]]
  
  # welche Frage
  if(!is.na(frage)) {
    
    Codebook_Rawdata[num,5] <- paste0(
      "\\hyperref[",
      str_replace_all(frage, fixed("_"), ":"),
      "]{",
      frage,
      "}"
    )
    
  }
  
  
  
  num <- num+1
  
  values      <- na.omit(unique(sort(rawdat[[i]])))
  valuelabels <- attr(rawdat[[i]], "labels")
  
  # Einträge für Variablen mit Wertelabels
  if(is.null(valuelabels)==FALSE) {
    
    # numerische Variablen (zB Betriebsgröße)
    if(length(values) > length(valuelabels)) {
      
      num <- num+1
      
      if(any(str_detect(values, ".")==T)) {
        Codebook_Rawdata[num,2] <- paste0(
          round(range(values[!values %in% valuelabels])[1], digits=2),
          "--",
          round(range(values[!values %in% valuelabels])[2], digits=2)
        )
        Codebook_Rawdata[num,3] <- paste0(
          round(range(values[!values %in% valuelabels])[1], digits=2),
          "--",
          round(range(values[!values %in% valuelabels])[2], digits=2)
        )
      }
      
      if(any(str_detect(values, ".")==F)) {
        Codebook_Rawdata[num,2] <- paste0(
          range(values[!values %in% valuelabels])[1],
          "--",
          range(values[!values %in% valuelabels])[2]
        )
        Codebook_Rawdata[num,3] <- paste0(
          range(values[!values %in% valuelabels])[1],
          "--",
          range(values[!values %in% valuelabels])[2]
        )
      }
      
      # Häufigkeit numerischer Angaben (ohne gelabelte Werte, zB missings)
      Codebook_Rawdata[num, 4] <- length(
        na.omit(rawdat[[i]][!rawdat[[i]] %in% valuelabels])
      )
      
      num <- num+1
      
      # Gelabelte Werte (hauptsächlich Missings)
      Codebook_Rawdata[num:(num+length(valuelabels)-1),2] <- names(valuelabels)
      Codebook_Rawdata[num:(num+length(valuelabels)-1),3] <- round(valuelabels,
                                                                   digits=0)
      # Häufigkeiten gelabelter Werte
      Codebook_Rawdata[num:(num+length(valuelabels)-1),4] <- 
        unlist(lapply(valuelabels, 
                      function(x) 
                        length(na.omit(rawdat[[i]][rawdat[[i]] == x])))
        )
      
      num <- num+length(valuelabels)-1
      
      # Jetzt vollständig gelabelte Variablen (kategoriale Var.)
    } else {
      
      num <- num+1
      Codebook_Rawdata[num:(num+length(valuelabels)-1),2] <- names(valuelabels)
      Codebook_Rawdata[num:(num+length(valuelabels)-1),3] <- round(valuelabels,
                                                                   digits=0)
      # Häufigkeiten der Werte
      Codebook_Rawdata[num:(num+length(valuelabels)-1),4] <- 
        unlist(lapply(valuelabels, 
                      function(x) 
                        length(na.omit(rawdat[[i]][rawdat[[i]] == x])))
        )
      
      num <- num+length(valuelabels)-1
    }
    # komplett ungelabelte Variablen
  } else {
    
    # numerische ungelabelte Var. (zB laufende Nummer)
    if(!str_detect(attr(rawdat[[i]], "label"), "offen")) {
      
      num <- num+1
      
      Codebook_Rawdata[num,2] <- paste0(
        round(range(values)[1], digits=2),
        "--",
        round(range(values)[2], digits=2)
      )
      Codebook_Rawdata[num,3] <- paste0(
        round(range(values)[1], digits=2),
        "--",
        round(range(values)[2], digits=2)
      )
      
      Codebook_Rawdata[num,4] <- 
        length(na.omit(rawdat[[i]]))
    } else {
      Codebook_Rawdata[num,2] <- "offene Angabe"
      Codebook_Rawdata[num,4] <- length(na.omit(rawdat[[i]]))
    }
  }
  
  num <- num+1
  
  Codebook_Rawdata[num,2] <- paste0(
    "\\textit{", "missing by design", "}"
  )
  Codebook_Rawdata[num,3] <- paste0(
    "\\textit{", ".", "}"
  )
  Codebook_Rawdata[num,4] <- 
    nrow(rawdat) - length(na.omit(rawdat[[i]]))
}

# Unterstriche einführen
# Unterstriche einführen, Silbentrennung----
Codebook_Rawdata <- data.frame(lapply(Codebook_Rawdata, str_replace_all, fixed("_"), "\\_"))

Codebook_Rawdata[,1] <- 
  str_replace(Codebook_Rawdata[,1], "Interviewdauer", "Interview\\-dauer")
Codebook_Rawdata[,2] <- str_replace(Codebook_Rawdata[,2], fixed("e+05"), "00000")
Codebook_Rawdata[,3] <- str_replace(Codebook_Rawdata[,3], fixed("e+05"), "00000")

# subsections()
num1 <- 0
num2 <- 0
index <- which(!is.na(Codebook_Rawdata[,1]))
vars <- str_remove(str_remove(Codebook_Rawdata[index,1], "\\\\textbf\\{"), "\\}.*")
var_ref <- vars[1]
ind_ref <- index[1]

for(b in vars) {
  
  num1 <- num1+1
  num2 <- num2+1
  
  firstletter <- str_extract(b, "^.")
  firstletterbefore <- str_extract(vars[num1-1], "^.")
  
  if(identical(firstletter == firstletterbefore, F) & num2 > 30) {
    
    Codebook_Rawdata[ind_ref+1,2] <- 
      paste0(
        "\\protect\\subsection[Variablen ",
        var_ref, " bis " , vars[num1-1], "]", "{}"
      )
    
    num2 <- 0
    var_ref <- vars[num1]
    ind_ref <- index[num1]
  }
  
}
# in ein xtable überführen (Latex-Format)
Codebook_tex <- xtable(Codebook_Rawdata, digits=0)

# An welchen Stellen sollen midrules eingefügt werden?
midrules <- which(is.na(Codebook_Rawdata[,1])==F)-1
midrules <- midrules[-1]

head <- paste(c(
  paste0("\\section{Codebuch der WSI-Betriebsrätebefragung 2015, Rohdatensatz}",
         "\\label{kap_rohdaten}"
  ),
  paste0("\\begin{longtable}",
         "{| L{2.65cm} | L{8cm} | C{1.5cm} | R{1.25cm} | C{2.25cm}  |}"
  ),
  "\\toprule",
  paste0("\\textbf{Variable} & \\textbf{Bezeichnung und Labels}",
         "& \\textbf{Werte} &  \\textbf{n} & \\textbf{Frage} \\\\"
  ),
  "\\midrule",
  "\\endfirsthead",
  "\\toprule",
  paste0("\\textbf{Variable} & \\textbf{Bezeichnung und Labels}",
         "& \\textbf{Werte} & \\textbf{n} & \\textbf{Frage}  \\\\"
  ),
  "\\midrule",
  "\\endhead",
  "\\midrule",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot"),
  collapse="\n")

table <- print.xtable(Codebook_tex, 
                      include.rownames=F,
                      sanitize.text.function = identity,
                      only.contents=T,
                      booktabs=T,
                      hline.after=midrules,
                      include.colnames=F, 
                      print.results=F
) 

con <- "./Berichte/Codebook/D_Codebook_Rawdata.tex"
writeLines(c(head, table, "\\end{longtable}"), con=con)
