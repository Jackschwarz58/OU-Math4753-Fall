#' myreadin(filename)
#'
#' Reading in Data Files Function.
#'
#' This function is meant to take the small annoyance out of having to load in either a CSV or XLS and
#' does the work for you. You pass in the file name WITHOUT the extension and the function will check if
#' it exists, whether its a CSV or XLS, read it in, and return the data to be caught in a variable. If the
#' file doesn't exist/can't be found, it spits out a File Not Found message with a sad face.
#' Make sure the data is located in the **_caller file's_** current working directory.
#'
#' @param filename The name of the file to be read in, excluding the extension. Must be either CSV or XLS
#'
#' @return A data frame with data from the file that was read in
#' @export
#'
#' @examples
#' myreadin('CIVILSAL')
#' myreadin('NZBIRDS')
#' myreadin('DDT')
myreadin = function(filename) {
  fileToFindCSV <- gsub(" ", "", paste(filename,'.csv'))
  fileToFindXLS <- gsub(" ", "", paste(filename,'.XLS'))

  output <- ''
  if(file.exists(fileToFindCSV)) {
    output <- read.csv(fileToFindCSV)
  } else if (file.exists(fileToFindXLS)){
    output <- readxl::read_xls(fileToFindXLS)
  } else {
    output <- 'File Not Found :('
  }
  return(output)
}
