# Version history
# 20211005 v1 01 by Alex Mitrani, first version.

#' @name chunkr
#' @title chunkr splits a dataset into chunks
#' @description splits a dataset into a specified number of chunks so that they can be processed sequentially
#' @details The splitting is done in terms of units defined by the combinations of the levels of one or more variables.
#' Any output files will be produced in .rds format.
#' 
#' @import tidyverse
#' @import crayon
#' @import rlang
#'
#' @param mydf the dataframe to be used.
#' @param nchunks an integer target number of chunks in which the dataframe is to be split (1 <= nchunks <= N) where N is the number of units.  
#' @param ... a list of one or more variables that defines the units which should not be split apart.  
#'
#' @return
#' @export
#'
#' @examples
#' mydf <- mtcars
#' nchunks <- chunkr(mydf, nchunks = 100L, "gear")
#' nchunks
#' 
#'
chunkr <- function(mydf, nchunks = 1L, ...) {
  
  # browser()
  outputlabel <- toString(match.call())
  
  cat(yellow("\n", "chunkr.R running with the following command line:", outputlabel, "\n \n"))
  
  #sense checks
  stopifnot(is.integer(nchunks)==TRUE)
  stopifnot(nchunks>=1L)
  
  my_return_name <- deparse(substitute(mydf))
  

  myvarlist <- c(...)
  varlist <- list(myvarlist)
  stringvarlist <- toString(myvarlist)
  
  for (x in myvarlist) {
    
    varexists <- x %in% names(mydf)
    stopifnot(varexists==TRUE)
    
  }
  
  mydfrecords <- nrow(mydf)
  cat(yellow(paste0("Number of records in full dataframe = ", mydfrecords, ". ", "\n \n")))

  mydf2 <- mydf %>%
    group_by(across(all_of(myvarlist))) %>%
    slice_head(n=1) %>%
    ungroup()
  
  mydf2 <- keepr(mydf2, myvarlist)
  
  myunits <- nrow(mydf2)
  
  cat(yellow(paste0("\n", "Number of units defined by ", stringvarlist,": ", myunits, ".  This is the maximum number of chunks.", "\n \n")))
  
  if (nchunks > myunits) {

    cat(yellow(paste0(nchunks, " chunks were requested, but the maximum possible number of chunks is ", myunits, ".  The data will be split into ", myunits, " chunks.  \n \n")))
    nchunks <-  myunits
    
  }

  cat(yellow(paste0("Target number of chunks = ", nchunks, ".", "\n \n")))
  rawchunksize <- myunits/nchunks
  floorchunksize <- floor(rawchunksize)
  checksumofunits <- floorchunksize*nchunks
  checkdeficit <- myunits - checksumofunits
  normalchunksize <- floorchunksize
  lastchunksize <- normalchunksize + checkdeficit
  checksumofunitsagain <- (nchunks-1)*normalchunksize + lastchunksize
  mycheck <- myunits==checksumofunitsagain
  
  if (nchunks==2) {
    
    mytext <- " chunk of "
    
  } else {
    
    mytext <- " chunks of "
    
  }
   
  cat(yellow(paste0("The data will be divided into ", nchunks-1, mytext, normalchunksize, " units and 1 chunk of ", lastchunksize, " units.", "\n \n")))
  cat(yellow(paste0("Correct? ", mycheck, ".", "\n \n")))
  
  mydf2$nchunk <- 0
  
  for(x in 1:nchunks) {
    
    myfirstrow <- (x-1)*normalchunksize+1
    
    if(x<nchunks) {
      
      mylastrow <- myfirstrow+normalchunksize-1
      
    } else {

      mylastrow <- myunits
      
    }
    
    cat(yellow(paste0("Identifying chunk ", x, " ...", "\n \n")))
    
    mydf2 <- mydf2 %>%
      mutate(myrownumber = row_number()) %>%
      mutate(nchunk = ifelse(myrownumber>=myfirstrow & myrownumber<=mylastrow, x, nchunk))
    
    # mydf2 <- dropr(mydf2, myrownumber)
    
  }

  mydf <- mydf %>% 
    left_join(mydf2)
  
  cat(yellow(paste0(" \n")))  
  
  mychunkedrecords <- 0
  
  for(x in 1:nchunks) {
    
    mydf3 <- mydf %>%
      filter(nchunk==x)
    
    myrecords <- nrow(mydf3)
    
    mychunkfilename <- paste0("fastchunk", x , ".rds")
    
    cat(yellow(paste0("Saving ", mychunkfilename, " with ", myrecords, " records...", "\n \n")))
    
    saveRDS(mydf3, mychunkfilename)
    
    mychunkedrecords <- mychunkedrecords + myrecords
    
  }
  
  cat(yellow(paste0("Total number of chunked records = ", mychunkedrecords, ". ", "\n \n")))
  
  myfinalcheck <- mydfrecords==mychunkedrecords
  
  cat(yellow(paste0("All records in full dataframe successfully chunked? ", myfinalcheck, ". ", "\n \n")))
  
  return(nchunks)
  
}

#

