getExtension <- function(file){ 
   install.packages(c('readxl', 'rjson', 'XML', 'foreign'))
   library(readxl)
   library(rjson)
   library(XML)
   library(foreign)
   ex <- strsplit(basename(file), split="\\.")[[1]]
   return(ex[-1])
} 
