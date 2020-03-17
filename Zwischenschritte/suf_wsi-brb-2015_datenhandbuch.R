texi2pdf(
  "./Berichte/Codebook/2018-05-17_Codebook-Rawdata-BRB-2015.tex",
  clean=T)

zeit <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
# Bericht ablegen
file.rename(
  "2018-05-17_Codebook-Rawdata-BRB-2015.pdf",
  paste0(zeit,"_Codebook_WSI-Betriebsrätebefragung-2015.pdf")
)
file.move(paste0(zeit,"_Codebook_WSI-Betriebsrätebefragung-2015.pdf"), 
                 "./Output/Berichte/")
file.remove(
  list.files()[which(str_detect(list.files(), "2018-05-17"))]
)
