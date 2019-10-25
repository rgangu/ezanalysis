#' Return dataframe from data file.
#'
#' @param file A file, of any type.
#' @return  A dataframe of your data file.
#' @examples
#' read_file('cool.sas7bdat')
#' read_file('data.csv')
read_file <- function(file){
   source('getExtension.R')
   if (getExtension(file) == "csv") {
      df = read.csv(file)
   }
   else if (getExtension(file) == "txt") {
      df = read.table(file)
   }
   else if (getExtension(file) == "xls" | "xlsx") {
      df = read_excel(file)
   }
   else if (getExtension(file) == "json") {
      df <- fromJSON(file= file)
   }
   else if (getExtension(file) == "xml") {
      xmlfile <- xmlTreeParse(file)
      topxml <- xmlRoot(xmlfile)
      topxml <- xmlSApply(topxml,
                          function(x) xmlSApply(x, xmlValue))
      df <- data.frame(t(topxml),
                       row.names=NULL)

   }
   else if (getExtension(file) == "html") {
      df <- readHTMLTable(file, which=3)
   }
   else if (getExtension(file) == "sav") {
      df <- read.spss(file, to.data.frame=TRUE, use.value.labels=FALSE)
   }
   else if (getExtension(file) == "dta") {
      df <- read.dta(file)
   }
   else if (getExtension(file) == "systat") {
      df <- read.systat(file)
   }
   else if (getExtension(file) == "sas7bdat") {
      df <- read.sas7bdat(file)
   }
   else if (getExtension(file) == "mtp") {
      df <- read.mtp(file)
   }
   return(df)
}


