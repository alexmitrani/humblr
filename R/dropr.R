# Version history
# 20200911 v1 01 by Alex Mitrani. This function should behave in a similar way to the Stata program "droploop".  The arguments are a dataframe followed by a list of variables to be dropped.
# 20200915 v1 02 by Alex Mitrani. Updated to modify the requested dataframe in the calling environment.
# 20200917 v1 03 by Alex Mitrani. Simplified the return lines at the end.
# 20210104 v1 04 by Alex Mitrani, Added Roxygen skeleton and started adding documentation content.
# 20210108 v1 05 by Alex Mitrani, Improved example.
# 20210125 v1 06 by Alex Mitrani, Added information about object size before and after removing variables and RAM saved.

#' @name dropr
#' @title drops a specified list of variables and keeps the rest
#' @description dropr is used to drop a list of variables from a dataframe (and to keep the rest).  Inspired by the "drop" command in Stata.
#' @details dropr is used internally by the fsm package.
#'
#' @import tidyverse
#' @import crayon
#' @import rlang
#'
#' @param mydf the dataframe to be modified.
#' @param ... a list of the variables to be dropped.
#'
#' @return
#' @export
#'
#' @examples
#' mydf <- mtcars
#' mydf <- dropr(mydf, "am", "gear", "carb")
#' mydf
#'

dropr <- function(mydf,...) {

  my_return_name <- deparse(substitute(mydf))

  myinitialsize <- round(object.size(mydf)/1000000, digits = 3)
  cat(paste0("Size of ", my_return_name, " before removing variables: ", myinitialsize, " MB. \n"))

  names_to_drop <- c(...)
  mytext <- paste("The following variables will be dropped from ", my_return_name, ": ", sep = "")
  print(mytext)
  print(names_to_drop)
  mydf <- mydf[,!names(mydf) %in% names_to_drop]

  myfinalsize <- round(object.size(mydf)/1000000, digits = 3)
  cat(paste0("Size of ", my_return_name, " after removing variables: ", myfinalsize, " MB. \n"))
  ramsaved <- round(myinitialsize - myfinalsize, digits = 3)
  cat(paste0("RAM saved: ", ramsaved, " MB. \n"))

  return(mydf)

}

#

