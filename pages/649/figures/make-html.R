fn <- list.files()
fn <- fn[grep("\\.png",fn)]

Names <- sub("\\.png","",fn)
Names <- strsplit(Names,"-")
sink("index.html")
cat("<html>\n")
cat("<style type=\"text/css\">\n")
cat("ADDRESS {font-family: Arial, Helvetica;}\n")
cat("BODY    {font-family: Arial, Helvetica;}\n")
cat("TD      {font-family: Arial, Helvetica; font-weight: Bold;}\n")
cat("P       {font-family: Arial, Helvetica; text-align: justify;}\n")
cat("A:link  {text-decoration:none;}\n")
cat("A:vlink  {text-decoration:none;}\n")
cat("</style>\n\n")
cat( "<head>\n")
cat("<title>Figures for 644</title>\n")
cat("</head>\n")

cat("<UL>\n")
current <- 0 ##this the section
for(i in seq(along=Names)){
  tmp <- Names[[i]]
  numbers <- as.numeric(tmp[-1])
  section <- numbers[1]
  if(section!=current){
    if(current>0) cat("</UL>\n")
    current <- section
    cat(paste("<LI> Section",current,"\n"))
    cat("<UL>\n")
  }
  tmp <- paste(numbers[-1],collapse=".")
  cat(paste("<LI> <a href=\"",fn[i],"\"> Figure ",tmp,"</a>\n",sep=""))
}
cat("</UL>\n")
cat("</UL>\n")
cat("</HTML>")
sink()
system("cp -r ../Plots ~/public_html/Teaching/644/figures")

        
    
