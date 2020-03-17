# ist die 1 gelabelt und die 2 nicht?

for(b in names(rawdat)) {
  
  labs <- attributes(rawdat[[b]])$labels
  
  # ist die 1 gelabelt und die 2 nicht?
  check <- 1 %in% labs & !(2 %in% labs) &
    length(labs[!str_detect(names(labs), "verw|weiß|nicht zu")]) > 1
  
  if(identical(check, T)) {
    
    # Index vom zweiten zum letzten Wert der Skala (-1)
    
    index <- 2:(attributes(rawdat[[b]])$labels[2]-1)
    names(index) <- index
    attributes(rawdat[[b]])$labels <- c(labs[1], index, labs[2:length(labs)])
    
  }
  
}
