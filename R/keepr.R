# Version history
# 20200915 v1 01 by Alex Mitrani.  This function should behave in a similar way to "keep" in Stata.  The arguments are a dataframe followed by a list of variables to be kept.
# 20200915 v1 02 by Alex Mitrani.  Updated to modify the requested dataframe in the calling environment.
# 20200917 v1 03 by Alex Mitrani.  Simplified the return lines at the end.
# 20210104 v1 04 by Alex Mitrani,  Added Roxygen skeleton and started adding documentation content.
# 20210107 v1 05 by Alex Mitrani,  Added example command lines.
# 20210108 v1 06 by Alex Mitrani, Improved example.
# 20210125 v1 07 by Alex Mitrani, Added information about object size before and after removing variables and RAM saved.  

#' @name keepr
#' @title keeps a specified list of variables and drops the rest
#' @description keepr is used to keep a list of variables in a dataframe (and to drop the rest).  Inspired by the "keep" command in Stata.
#' @details keepr is used internally by the fsm package.
#'
#'
#' @param mydf the dataframe to be modified.
#' @param ... a list of the variables to be kept.
#'
#' @return
#' @export
#'
#' @examples
#' fs36idfrac01
#' mydf <- keepr(mydf = fs36idfrac01, "id", "mode_option", "choice")
#' mydf
#'
keepr <- function(mydf,...) {
  
  my_return_name <- deparse(substitute(mydf))
  
  myinitialsize <- round(object.size(mydf)/1000000, digits = 3)
  cat(paste0("Size of ", my_return_name, " before removing variables: ", myinitialsize, " MB. \n"))
  
  names_to_keep <- c(...)
  mytext <- paste("The following variables will be kept from ", my_return_name, ": ", sep = "")
  print(mytext)
  print(names_to_keep)
  mydf <- mydf[,names(mydf) %in% names_to_keep]
  
  myfinalsize <- round(object.size(mydf)/1000000, digits = 3)
  cat(paste0("Size of ", my_return_name, " after removing variables: ", myfinalsize, " MB. \n"))
  ramsaved <- round(myinitialsize - myfinalsize, digits = 3)
  cat(paste0("RAM saved: ", ramsaved, " MB. \n"))
  
  return(mydf)

}

#

