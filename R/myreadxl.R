#' Read in all excel files
#'
#'@description This will work well with reading in all excel files in a particular directory
#'
#' @param dird This is the directory structure to your excel folder
#'
#' @return A named list of all the data read into R, each component of the list is a data frame
#' @export
#'
#' @examples
#' v <-myreadxl()
myreadxl <- function(dird = "D:/MATH4773-5773/DATA/Excel/"){


  files = list.files(dird)


  myconvert = function(xl) {
    if(stringr::str_ends(xl, "XLS") | stringr::str_ends(xl, "xls")){
      v=try(readxl::read_xls(paste0(dird, xl)), silent = TRUE)
    }
    else{
      v = NA
    }
    v
  }

  v  = purrr::map(files, ~myconvert(.x))
  l <- stringr::str_length(files)
  #l
  newnames <- stringr::str_sub(files,1,l-4)
  #new names
  names(v) <- newnames
  invisible(v)
}
