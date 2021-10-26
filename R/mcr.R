# Version history
# 20211025 v1 01 by Alex Mitrani.  First version.
# 20211026 v1 02 by Alex Mitrani.  Refined code to add variable names to the output dataframe. Added " mydf <- as.data.frame(mydf)" and related lines.  

#' @name mcr
#' @title monte carlo simulation using mc2d
#' @description a wrapper for mc2d
#' @details The input spreadsheet should have only one sheet with the following columns and one row for each variable to be simulated:
#' - Index (numeric)
#' - Variable (character)
#' - codename (character)
#' - Base (numeric)
#' - Minimum (numeric)
#' - Mode (numeric)
#' - Maximum (numeric)
#' - Distribution (character) - Uniform, Triangular, or PERT.
#' - Shape (numeric) - only needed if distribution is "PERT"
#'
#' @import tidyverse
#' @import crayon
#' @import readxl
#' @import openxlsx
#' @import mc2d
#'
#' @param myfilename is the name of the input Excel file
#' @param myseed the random number seed to be used
#' @param nsims the desired number of Monte Carlo simulations
#'
#' @return
#' @export
#'
#' @examples
#' myinputfile <- system.file("extdata", "risk_variable_distributions.xlsx", package = "humblr")
#' mytest1 <- mcr(myinputfile, nsims = 1000, myseed = 12345L)
#' mytest1
#'


mcr <- function(myfilename = NULL, myseed = 12345L, nsims = 1000) {
  
  datestring <- datestampr(myusername=TRUE)
  now1 <- Sys.time()
  logrun <- TRUE
  
  
  if (logrun==TRUE) {
    
    mylogfilename <- paste0(datestring, "_mcr", ".txt")
    sink()
    sink(mylogfilename, split=TRUE)
    cat(yellow(paste0("A log of the output will be saved to ", mylogfilename, ". \n \n")))
    
  }
  

# define the number of simulations ----------------------------------------

  ndvar(nsims)  
  
  # import table with parameters of the distributions
  mydistributions <- read_excel(myfilename)
  nvar <- nrow(mydistributions)
  
  if (is.null(myseed)==TRUE) {
    
    myseed <- round(100000*runif(1))
    
  }
  
  # to import the factor distributions --------------------------------------
  
  for (myvar in 1:nvar) {
    
    varname <- as.character(mydistributions[myvar,2])
    assign(paste0("varname", myvar), varname)
    
    codename <- as.character(mydistributions[myvar,3])
    assign(paste0("codename", myvar), codename)
    
    mymin <- as.numeric(mydistributions[myvar,5])
    assign(paste0("min", myvar), mymin)
    
    mymode <- as.numeric(mydistributions[myvar,6])
    assign(paste0("mode", myvar), mymode)
    
    mymax <- as.numeric(mydistributions[myvar,7])
    assign(paste0("max", myvar), mymax)
    
    distribution <- as.character(mydistributions[myvar,8])
    assign(paste0("distribution", myvar), distribution)
    
    shape <- as.numeric(mydistributions[myvar,9])
    assign(paste0("shape", myvar), shape)
    
  }
  
  for (myvar in 1:nvar) {
    
    myvarname <- rlang::sym(paste0("varname", myvar))
    mydistributionname <- rlang::sym(paste0("distribution", myvar))
    myminname <- rlang::sym(paste0("min", myvar))
    mymodename <- rlang::sym(paste0("mode", myvar))
    mymaxname <- rlang::sym(paste0("max", myvar))
    myshapename <- rlang::sym(paste0("shape", myvar))
    cat(yellow(paste0(eval(myvarname), "\n")))
    cat(blue(paste0("Distribution: ", eval(mydistributionname), "\n")))
    cat(blue(paste0("Min: ", eval(myminname), "\n")))
    cat(blue(paste0("Mode: ", eval(mymodename), "\n")))
    cat(blue(paste0("Max: ", eval(mymaxname), "\n")))
    cat(blue(paste0("Shape: ", eval(myshapename), "\n \n")))
    
  }

  # definition of mc2d nodes using the required distributions -------------------------------
  
  myvars <- NULL
  
  for (myvar in 1:nvar) {
    
    mymin <- rlang::sym(paste0("min", myvar))
    mymode <- rlang::sym(paste0("mode", myvar))
    mymax <- rlang::sym(paste0("max", myvar))
    myshape <- rlang::sym(paste0("shape", myvar))
    mydist <- rlang::sym(paste0("distribution", myvar))
    myvarname <- paste0("var", myvar)
    
    if (eval(mydist)=="PERT") {
      
      mynode <- mcstoc(rpert, min = eval(mymin), mode = eval(mymode), max = eval(mymax), shape=eval(myshape))
      
    }
    
    if (eval(mydist)=="Triangular") {
      
      mynode <- mcstoc(rtriang, min = eval(mymin), max = eval(mymax), mode = eval(mymode))
      
    }
    
    if (eval(mydist)=="Uniform") {
      
      mynode <- mcstoc(runif, min = eval(mymin), max = eval(mymax))
      
    }
    
    assign(eval(myvarname), mynode)
    
    if(myvar==1) {
      
      mydf <- get(eval(myvarname))
      
    } else {
    
      mydf <- cbind(mydf, get(eval(myvarname)))  
      
    }
    
    colnames(mydf)[1] <- "V1"
    
  }
  
  mydf <- as.data.frame(mydf)
  
  for (myvar in 1:nvar) {
    
    mycode <- rlang::sym(paste0("codename", myvar))
    myoldvarname <- paste0("V", myvar)
    
    names(mydf)[names(mydf)==myoldvarname] <- eval(mycode)
    
  }  
  
  # to export the results ---------------------------------
  

  wb <- createWorkbook()
  
  sheetname <- "distributions"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydistributions)
  
  
  sheetname <- "mc_variables"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf)
  

  file_name <- paste0(datestring, "_mcr", ".xlsx")
  saveWorkbook(wb, file = file_name, overwrite = TRUE)
  
  
  # to finish up ------------------------------------------------------------
  
  cat(yellow(paste0("The results have been saved to ", file_name, ". \n \n")))
  
  if (logrun==TRUE) {
    sink()
    cat(yellow(paste0("A log of the output has been saved to ", mylogfilename, ". \n \n")))
  }
  
  print(gc())
  
  now2 <- Sys.time()
  elapsed_time <- now2 - now1
  
  cat(yellow(paste0("\n \n", "Elapsed time: \n ")))
  print(elapsed_time)
  cat(yellow(paste0("\n \n")))
  
  return(mydf)
  
}
