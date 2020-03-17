starttime <- Sys.time()

# Fragebogen einlesen (von Word als .txt, danach bearbeitet)--------------------
options(encoding="Windows-1252")
fb <- readLines("./Hintergrund/Fragebogen_Betriebsrätebefragung_2015_text.txt")
fb <- stri_enc_toutf8(fb)
options(encoding="UTF-8")

fragen <- unique(na.omit(str_extract(fb, "^[A-Z][0-9][[:alnum:]\\_]*")))

# Link zwischen Variablen und Fragen im Fragebogen------------------------------
fb_link <- data.frame(vars = names(rawdat), 
                      fragen = unlist(sapply(names(rawdat), get.qu)), 
                      kurz=as.integer(unlist(sapply(
                        rawdat, get.lk, y=rawdat$langkurz, value=2)))
)

# Fragebogen als Data Frame-----------------------------------------------------
fb_df <- data.frame(Frage = character(length=length(fb)), 
                    Text = character(length=length(fb)), 
                    Variablen=character(length=length(fb)),
                    stringsAsFactors = F)

# Split am ersten Doppelpunkt
fb_df[,1] <- str_extract(fb, "[^:]+:")
fb_df[,2] <- str_remove(fb, "[^:]+: ")

# reiner Text (fett drucken)
rt <- is.na(fb_df[,1]) & fb_df[,2] != ""
fb_df[rt,2] <- paste0("\\textbf{", fb_df[rt,2], "}")

# Wenn/Prog/Int: rot
pwi <- str_detect(fb_df[,1], "PROG:|WENN:|INT:") %in% T
fb_df[pwi,1] <- paste0("\\textcolor{red}{", fb_df[pwi,1],"}")
fb_df[pwi,2] <- paste0("\\textcolor{red}{", fb_df[pwi,2],"}")

# Codes/Items
code <- str_detect(fb_df[,1], 
                   "^[:digit:]{1,6}:|^[:upper:]{1,2}:|^[:upper:][:lower:]{1,6}:") %in% T

fb_df[code,1] <- paste0(
  "\\multicolumn{1}{!{\\color{black}\\vline width 1pt}",
  "r!{\\color{black}\\vline width 1pt}  }{",
  fb_df[code,1],
  "}")

# Blocks
block <- str_detect(fb_df[,1], "Block ..?:") %in% T
fb_df[block,1] <- 
  paste0("\\protect\\subsection[\\parbox{\\mylength}{", 
         fb_df[block,1], "} ", fb_df[block,2], "]", "{", fb_df[block,1], 
         "}"
  )
fb_df[block,2] <- paste0("\\protect\\subsection*{",  fb_df[block,2] , "}" )

# Fragen
frage <- str_detect(fb_df[,1], "^[A-Z][0-9][[:alnum:]\\_]*|Intromail.:") %in% T


# Variablen zu Fragen verlinken
for(i in which(frage)) {
  
  fr <- str_remove(fb_df[i,1], ":")
  vars <- split(fb_link, fb_link[,2])[[fr]]$vars
  varsref <- str_replace_all(vars, fixed("_"), ":")
  laenge <- length(vars)
  
  # Hyperref für alle Variablen
  temp <- paste0("\\hyperref[var:", varsref, "]{", vars, "}")
  
  # Darstellung abhängig von Länge
  if(laenge == 1) fb_df[i,3] <- temp
  if(laenge == 2) fb_df[i,3] <- paste(temp, collapse= ", ")
  if(laenge >  2) fb_df[i,3] <- paste(temp[c(1,laenge)], collapse= " [...] ")
  
}

# Label zum Referenzieren für Fragen
fb_df[frage,1] <- 
  paste0("\\textbf{",
         fb_df[frage,1], "}\\phantomsection\\label{",
         str_replace_all(str_remove(fb_df[frage,1], ":"), fixed("_"), ":"),
         "}")

# Fragentext in fett
fb_df[frage,2] <- paste0("\\textbf{", fb_df[frage,2], "}")

# Bearbeitungen (zB midrules)---------------------------------------------------
midrules <- NA

for(i in which(!is.na(fb_df[,1]))) {
  
  if(str_detect(fb_df[i,1], "\\{[A-Z][0-9][[:alnum:]\\_]*:|Intromail.:")) {
    midrules[i] <- i-1-identical(str_detect(fb_df[i-1,1], "WENN"), T)
  }
  
  if(str_detect(fb_df[i,1], "Block")) {
    midrules[i] <- i-1
  }
  
}

midrules <- na.omit(midrules)[-1]

# Farbige Zeilen----
switch <- 0

for(i in seq_along(fb)) {
  
  x <- str_extract(fb[i], "^[A-Z][0-9][[:alnum:]\\_]*")
  
  if(x %in% na.omit(fb_link$fragen[fb_link$kurz == 1])) switch <- 1
  if(x %in% na.omit(fb_link$fragen[fb_link$kurz == 0])) switch <- 0
  if(switch == 1) fb_df[i,1] <- paste0("\\rowcolor[gray]{.9}", fb_df[i,1])
  
}

# Hyperlinks für Filterführungen----
for(i in 
    which(str_detect(
      fb_df[,2], "[A-Z][0-9]") & str_detect(fb_df[,1], "PROG:|WENN:")
    )) {
  
  x <- str_extract_all(fb_df[i,2], "[A-Z][0-9][[:alnum:]\\_]{0,6}", simplify=T)
  z <- str_extract(x, "\\_[:upper:]")
  x <- str_replace(x, "\\_[:upper:]", "")
  x <- paste0("\\hyperref[", str_replace(x, fixed("_"), ":"), "]{", x, "}")
  x <- paste0(x, z)
  y <- str_split(fb_df[i,2], "[A-Z][0-9][[:alnum:]\\_]{0,6}", simplify=T)
  
  fb_df[i,2] <- paste0(
    paste(y[1:length(x)], x, collapse=""), y[length(y)], 
    collapse="")
  
}

# Unterstriche einführen, NAs löschen----
fb_df <- data.frame(lapply(fb_df, str_replace_all, fixed("_"), "\\_"))
fb_df <- data.frame(lapply(fb_df, str_replace_all, fixed("NA"), ""))
fb_df <- data.frame(lapply(fb_df, str_replace_na, ""))

# in ein xtable überführen (Latex-Format)---------------------------------------
fb_tex <- xtable(fb_df, digits=0)



head <- paste(
  c(paste0("\\section[Fragebogen der WSI-Betriebsrätebefragung 2015]",
           "{Fragebogen der WSI-Betriebsrätebefragung 2015, Stand: 12.01.2015}",
           "\\label{kap_fragebogen}"),
    paste0("\\begin{longtable}{",
           "!{\\color{black}\\vline width 1pt}  L{2cm}",
           "!{\\color{black}\\vline width 1pt} L{12.25cm}",
           "!{\\color{black}\\vline width 1pt}  L{2.5cm}",
           "!{\\color{black}\\vline width 1pt}}"),
    "\\toprule",
    "\\textbf{Schlüssel} & \\textbf{Text / Anweisung}  & \\textbf{Variablen} \\\\ ",
    rep("\\midrule", 2), "\\endfirsthead", "\\toprule",
    "\\textbf{Schlüssel} & \\textbf{Text / Anweisung}  & \\textbf{Variablen} \\\\ ",
    rep("\\midrule", 2), "\\endhead", rep("\\midrule", 2),
    paste0("\\multicolumn{1}{l}{\\textit{Erläuterung:}} &",
           "\\multicolumn{2}{l}{\\cellcolor[gray]{.9}",
           "Bereich grau hinterlegt: Frage wurde nur in der ",
           "\\hyperref[langkurz]{Kurzversion} des Fragebogens erhoben}\\\\"
    ),
    paste0("\\multicolumn{1}{l}{} & \\multicolumn{2}{l}{\\textcolor{red}{",
           "\\glqq INT\\grqq\\xspace = Interviewerhinweis/-anweisung}} \\\\"),
    paste0("\\multicolumn{1}{l}{} &	\\multicolumn{2}{l}{\\textcolor{red}{",
           "\\glqq PROG/WENN\\grqq\\xspace = Programmierhinweis/Filterführung}}",
           "\\\\"),
    "\\endfoot", "\\bottomrule", "\\endlastfoot"
  ),
  collapse="\n")

table <- print.xtable(fb_tex, 
                      include.rownames=F,
                      sanitize.text.function = identity,
                      only.contents=T,
                      booktabs=T,
                      hline.after=midrules,
                      include.colnames=F,
                      print.results=F
) 
con <- "./Berichte/Codebook/C_Fragebogen.tex"
writeLines(c(head, table, "\\end{longtable}"), con=con)

endtime <- Sys.time()
endtime - starttime

View(fb_df)
