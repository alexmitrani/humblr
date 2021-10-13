# Version history
# 20211013 v1 01 by Alex Mitrani.  First version.
# 20211013 v1 02 by Alex Mitrani.  Improvements to documentation.
# 20211013 v1 03 by Alex Mitrani.  Improvements to graph names.
# 20211013 v1 04 by Alex Mitrani.  Added "selected" to the optimisation summary table.  
# 20211013 v1 05 by Alex Mitrani.  Improvements to documentation.

#' @name lhs_sampling
#' @title generates a Latin hypercube based on inputs provided in an Excel spreadsheet.
#' @description facilitates the generation of a Latin hypercubes for a set of variables based on inputs specified in a spreadsheet table.
#' @details The spreadsheet should have only one sheet with the following columns and one row for each variable to be simulated:
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
#' Six types of optimisation algorithm are tested: 
#' - randomLHS
#' - optimumLHS
#' - maximinLHS
#' - improvedLHS
#' - geneticsLHS - genetic algorithm with "S" criterium
#' - geneticmLHS - genetic algorithm with "Maximin" criterium
#' 
#' The algorithm that minimises the maximum correlation ("max_corr") is selected to produce the recommended Latin hypercube.  
#' The details of the selected algorithm and the other algorithms tested are added to the sheet "optimisation_summary" of the results workbook.
#'
#' @import tidyverse
#' @import crayon
#' @import readxl
#' @import openxlsx
#' @import mc2d
#'
#' @param myfilename is the name of the input Excel file
#' @param mytestspervariable the number of tests per variable in the input file
#' @param myseed the random number seed to be used
#' @param mymaxsweeps see lhs package documentation
#' @param myeps see lhs package documentation
#' @param mydup see lhs package documentation
#' @param mypop see lhs package documentation
#' @param mygen see lhs package documentation
#' @param mypmut see lhs package documentation
#' @param mygraphsize pixel dimension to be used for scatter graph matrices. see "width" and "height" in png {grDevices}.
#' @param mypch see "pch" in par {graphics}
#' @param mycol see "col" in par {graphics}
#' @param mycex see "cex" in par {graphics}
#'
#' @return
#' @export
#'
#' @examples
#' myinputfile <- system.file("extdata", "Risk variable distributions for Latin Hypercube sample.xlsx", package = "humblr")
#' mytest1 <- lhs_sampling(myinputfile, mytestspervariable = 10, myseed = 12345L, mymaxsweeps = 4, myeps = 0.01, mydup = 5, mypop = 1000, mygen = 8, mypmut = 0.1, mygraphsize = 1000, mypch = 19, mycol = "blue", mycex = 0.5)
#' mytest1
#'


lhs_sampling <- function(myfilename = NULL, mytestspervariable = 10, myseed = 12345L, mymaxsweeps = 4, myeps = 0.01, mydup = 5, mypop = 1000, mygen = 8, mypmut = 0.1, mygraphsize = 1000, mypch = 19, mycol = "blue", mycex = 0.5) {


  datestring <- datestampr(myusername=TRUE)
  logrun <- TRUE

  if (logrun==TRUE) {

  	mylogfilename <- paste0("lhs_sampling_", datestring,".txt")
  	sink()
  	sink(mylogfilename, split=TRUE)
  	cat(yellow(paste0("A log of the output will be saved to ", mylogfilename, ". \n \n")))

  }

  # import table with parameters of the distributions
  mydistributions <- read_excel(myfilename)

  nvar <- nrow(mydistributions)
  nruns <- mytestspervariable*nvar

  if (is.null(myseed)==TRUE) {

    myseed <- round(100000*runif(1))

  }

  mygraphname <- paste0(datestring, "_randomLHS")
  mytest1 <- hypercuber(mygraphname = mygraphname, myseed = myseed, myn=nruns, myk=nvar, myalgorithm = "randomLHS", mygraphsize = mygraphsize, mypch = mypch, mycol = mycol, mycex = mycex)
  mydf1 <- as.data.frame(mytest1[1])
  sum1 <- as.data.frame(mytest1[2])

  mygraphname <- paste0(datestring, "_optimumLHS")
  mytest2 <- hypercuber(mygraphname = mygraphname, myseed, myn=nruns, myk=nvar, myalgorithm = "optimumLHS", mymaxsweeps = mymaxsweeps, myeps = myeps, mygraphsize = mygraphsize, mypch = mypch, mycol = mycol, mycex = mycex)
  mydf2 <- as.data.frame(mytest2[1])
  sum2 <- as.data.frame(mytest2[2])

  mygraphname <- paste0(datestring, "_maximinLHS")
  mytest3 <- hypercuber(mygraphname = mygraphname, myseed, myn=nruns, myk=nvar, myalgorithm = "maximinLHS", mydup = mydup, mygraphsize = mygraphsize, mypch = mypch, mycol = mycol, mycex = mycex)
  mydf3 <- as.data.frame(mytest3[1])
  sum3 <- as.data.frame(mytest3[2])

  mygraphname <- paste0(datestring, "_improvedLHS")
  mytest4 <- hypercuber(mygraphname = mygraphname, myseed, myn=nruns, myk=nvar, myalgorithm = "improvedLHS", mydup = mydup, mygraphsize = mygraphsize, mypch = mypch, mycol = mycol, mycex = mycex)
  mydf4 <- as.data.frame(mytest4[1])
  sum4 <- as.data.frame(mytest4[2])

  mygraphname <- paste0(datestring, "_geneticsLHS")
  mytest5 <- hypercuber(mygraphname = mygraphname, myseed, myn=nruns, myk=nvar, myalgorithm = "geneticLHS", mypop = mypop, mygen = mygen, mypmut = mypmut, mycriterium = "S", mygraphsize = mygraphsize, mypch = mypch, mycol = mycol, mycex = mycex)
  mydf5 <- as.data.frame(mytest5[1])
  sum5 <- as.data.frame(mytest5[2])

  mygraphname <- paste0(datestring, "_geneticmLHS")
  mytest6 <- hypercuber(mygraphname = mygraphname, myseed, myn=nruns, myk=nvar, myalgorithm = "geneticLHS", mypop = mypop, mygen = mygen, mypmut = mypmut, mycriterium = "Maximin", mygraphsize = mygraphsize, mypch = mypch, mycol = mycol, mycex = mycex)
  mydf6 <- as.data.frame(mytest6[1])
  sum6 <- as.data.frame(mytest6[2])

  # produce a table to compare the tests
  optimisation_summary <- rbind(sum1, sum2, sum3, sum4, sum5, sum6)

  # sort the summary dataframe in ascending order of maximum correlation and indicate the selected option
  optimisation_summary <- optimisation_summary %>%
    arrange(max_corr) %>%
    mutate(selected = ifelse(row_number()==1,1,0))

  # define the best option as being that with the minimum value of maximum correlation
  best_option <- as.character(optimisation_summary[1,2])

  if(best_option == "randomLHS") {

    mydfname <- rlang::sym(paste0("mydf1"))
    mygraphname <- "randomLHS"

  } else if (best_option == "optimumLHS") {

    mydfname <- rlang::sym(paste0("mydf2"))
    mygraphname <- "optimumLHS"

  } else if (best_option == "maximinLHS") {

    mydfname <- rlang::sym(paste0("mydf3"))
    mygraphname <- "maximinLHS"

  } else if (best_option == "improvedLHS") {

    mydfname <- rlang::sym(paste0("mydf4"))

  } else if (best_option == "geneticLHS" & criterium == "S") {

    mydfname <- rlang::sym(paste0("mydf5"))
    mygraphname <- "geneticsLHS"

  } else if (best_option == "geneticLHS" & criterium == "Maximin") {

    mydfname <- rlang::sym(paste0("mydf6"))
    mygraphname <- "geneticmLHS"

  }

  assign("mydf", eval(mydfname))


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


# to graph the distributions ----------------------------------------------

  for (myvar in 1:nvar) {

    mymin <- rlang::sym(paste0("min", myvar))
    mymode <- rlang::sym(paste0("mode", myvar))
    mymax <- rlang::sym(paste0("max", myvar))
    myshape <- rlang::sym(paste0("shape", myvar))
    mydist <- rlang::sym(paste0("distribution", myvar))
    myvarname <- rlang::sym(paste0("varname", myvar))
    myplotfile <- rlang::sym(paste0("codename", myvar))
    myplotfilename <- paste0(datestring, "_", eval(myplotfile), ".png")

    x <- seq(eval(mymin), eval(mymax), length=100)

    if (eval(mydist)=="PERT") {

      hx <- dpert(x, min=eval(mymin), mode=eval(mymode), max=eval(mymax), shape=eval(myshape))
      mymaintext <- paste0(eval(mydist), ", shape = ", eval(myshape), ", min = ", round(eval(mymin), digits=2), ", mode = ", round(eval(mymode), digits=2), ", max = ", round(eval(mymax), digits=2))

    }

    if (eval(mydist)=="Triangular") {

      hx <- dtriang(x, min=eval(mymin), mode=eval(mymode), max=eval(mymax))
      mymaintext <- paste0(eval(mydist), ", shape = ", eval(myshape), ", min = ", round(eval(mymin), digits=2), ", mode = ", round(eval(mymode), digits=2), ", max = ", round(eval(mymax), digits=2))

    }

    if (eval(mydist)=="Uniform") {

      hx <- dunif(x, min=eval(mymin), max=eval(mymax))
      mymaintext <- paste0(eval(mydist), ", shape = ", eval(myshape), ", min = ", round(eval(mymin), digits=2), ", max = ", round(eval(mymax), digits=2))

    }

    png(myplotfilename, width = 500, height = 400)
    plot(x, hx, type="l", lty=2, xlab=eval(myvarname), ylab="Density", main = mymaintext)
    dev.off()

    cat(yellow(paste0("Saved graph of ", eval(myvarname)," to ", myplotfilename, ". \n \n")))

  }



# transformations to required distributions -------------------------------


  B <- matrix(nrow = nrow(mydf), ncol = ncol(mydf2)-1)

  for (myvar in 1:nvar) {

    mymin <- rlang::sym(paste0("min", myvar))
    mymode <- rlang::sym(paste0("mode", myvar))
    mymax <- rlang::sym(paste0("max", myvar))
    myshape <- rlang::sym(paste0("shape", myvar))
    mydist <- rlang::sym(paste0("distribution", myvar))

    if (eval(mydist)=="PERT") {

      B[, myvar] <- qpert(mydf[, myvar+1], min = eval(mymin), mode = eval(mymode), max = eval(mymax), shape=eval(myshape))

    }

    if (eval(mydist)=="Triangular") {

      B[, myvar] <- qtriang(mydf[, myvar+1], min = eval(mymin), mode = eval(mymode), max = eval(mymax))

    }

    if (eval(mydist)=="Uniform") {

      B[, myvar] <- qunif(mydf[, myvar+1], min = eval(mymin), max = eval(mymax))

    }


  }

  mygraphfilename <- paste0(datestring, "_", mygraphname, ".png")
  png(mygraphfilename, width = mygraphsize, height = mygraphsize)
  pairs(B, pch = mypch, col = mycol, cex = mycex)
  dev.off()

  mydf7 <- as.data.frame(B) %>%
    mutate(id = row_number()) %>%
    relocate(id)

  # to export the resulting Latin Hypercubes ---------------------------------

  for (myvar in 1:7) {

    mydfname <- rlang::sym(paste0("mydf", myvar))
    assign("mydf", eval(mydfname))

    for (myvar2 in 1:nvar) {

      mycode <- rlang::sym(paste0("codename", myvar2))
      myoldvarname <- paste0("V", myvar2)

      names(mydf)[names(mydf)==myoldvarname] <- eval(mycode)

    }

      assign(paste0(mydfname), mydf)

  }

  wb <- createWorkbook()

  sheetname <- "distributions"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydistributions)

  sheetname <- "randomLHS_uniform"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf1)

  sheetname <- "optimumLHS_uniform"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf2)

  sheetname <- "maximinLHS_uniform"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf3)

  sheetname <- "improvedLHS_uniform"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf4)

  sheetname <- "geneticsLHS_uniform"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf5)

  sheetname <- "geneticmLHS_uniform"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf6)

  sheetname <- "selectedLHS_various"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf7)

  sheetname <- "optimisation_summary"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, optimisation_summary)

  file_name <- paste0(datestring, "_lhs", ".xlsx")
  saveWorkbook(wb, file = file_name, overwrite = TRUE)


  # to finish up ------------------------------------------------------------

  cat(yellow(paste0("The results have been saved to ", file_name, ". \n \n")))

  if (logrun==TRUE) {
    sink()
    cat(yellow(paste0("A log of the output has been saved to ", mylogfilename, ". \n \n")))
  }

  print(gc())

  return(optimisation_summary)

}
