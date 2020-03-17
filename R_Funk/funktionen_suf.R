# Dateien verschieben----

moving <- function(path) {
  
  files <- list.files(path, full.names=T, recursive=F)
  # Unterordner ausklammern:
  files <- setdiff(files, list.dirs(path, recursive=F))
  movers <- !str_detect(files, format(Sys.time(), "%Y-%m-%d"))
  file.move(files[movers], paste0(path, "/bin"))
  
}
# Doppelte Dateien löschen----

delete.identical <- function(dir, automate=F, all=F, full=F) {
  
  functions <- list(
    .sav = read_sav,
    .dta = read_dta,
    .pdf = file.size,
    .txt = readLines
  )
  
  change <- T
  lauf <- 0
  
  while(change == T) {
    
    dateien <- rev(list.files(dir, full.names=T, recursive=full))
    print(lauf <- lauf+1)
    
    for(b in dateien) {
      
      verblieben <- rev(list.files(dir, full.names=T, recursive=full))
      index <- which(str_detect(verblieben, b))
      endung <- str_extract(b, "\\.[:alnum:]{1,4}$")
      same <- which(str_detect(verblieben, endung))
      nextdat <- same[same > index][unique((1:length(same[same > index]))^all)]
      
      if(all(!is.na(nextdat))) {
        
        if(!(endung %in% names(functions))) functions[[endung]] <- file.size
        dat1 <- functions[[endung]](verblieben[index])
        
        for(i in nextdat) {
          dat2 <- functions[[endung]](verblieben[i])
          
          if(identical(dat1, dat2)) {
            
            if(automate & file.exists(b)) file.remove(b)
            print(c(b, verblieben[i]))
            
          }
          
        }
          
      }
      
    }
    
    change <- length(verblieben) < length(dateien)
    
  }
  
}

# Funktionen: get.questions (get.qu) und get.langkurz (get.lk)----

get.qu <- function(names) {
  
  # Erster Schritt: Variablen am Unterstrich splitten
  var <- unlist(str_split(names, "\\_"))
  # Wenn EIN Element einer Frage entspricht: ablegen (wichtig für "Order")
  frage <- fragen[tolower(fragen) == tolower(var[which(var %in% fragen)])]
  
  # Jetzt strings zusammenpacken; 
  # der längste String, der einer Frage entspricht, ist korrekt.
  for(i in 1:length(var)) {
    
    var_c <- paste(var[1:i], collapse="_")
    
    # nur wenn ein zusammengesetzter String einer Frage entspricht:
    if(any(tolower(var_c) %in% tolower(fragen))) {
      frage <- fragen[tolower(fragen) == tolower(var_c[which(var_c %in% fragen)])]
    }
    
  }
  
  # Ergebnis: Entweder eine Frage oder NA
  if(length(frage) > 0) { return(frage) } else { return(NA) }
  
}

get.lk <- function(x, y, value) { 
  
  return(all(is.na(x[y==value])==T))
  
}