


#' @name model_ols_gpm
#' @title Model with OLS and GPM Components
#' @description produces a model with Ordinary Least Squares (OLS) and Gaussian Process Model (GPM) components, based on input data provided in an Excel spreadsheet.
#' @details The spreadsheet requires at least one sheet, that must contain the dependent and independent variables.
#'
#' @import tidyverse
#' @import readxl
#' @import openxlsx
#' @import caret
#' @import kernlab
#'
#' @param myinputspreadsheet is the name of the spreadsheet with the input data.
#' @param myinputdatasheet the name of the sheet with the input data.
#' @param mypredictionsheet (optional) the name of a sheet containing test data to which the model should be applied.
#' @param mytag a character string that will be used in the output file names.  #'
#' @param myterms a character vector containing the names of the independent variables.
#' @param sigmafrom the lower limit of the test range for sigma
#' @param sigmato the upper limit of the test range for sigma
#' @param sigmaby the increment of the test values for sigma
#' @param myfolds the number of folds for k-fold cross-validation
#' @param myseed1 the first random number seed
#' @param myseed2 the second random number seed
#' @param mygraphsize the pixel dimension for the output graphs
#' @param myresponse_ols the name of the dependent variable for the OLS model
#' @param myresponse_gpm the name of the dependent variable for the GPM model.  This will normally be the residuals of the OLS model: "residuals".
#'
#' @return
#' @export
#'
#' @examples
#' myinputfile <- system.file("extdata", "model_ols_gpm_test.xlsx", package = "humblr")
#' mytestmodel <- model_ols_gpm(myinputspreadsheet = myinputfile, myinputdatasheet = "data", mypredictionsheet = "prediction_tests", mytag = "model_ols_gpm_test", myresponse_ols = "depvar", myresponse_gpm = "residuals", myterms = c("var2",	"var3",	"var4",	"var5",	"var6",	"var7",	"var8",	"var9",	"var10"), sigmafrom = 1, sigmato= 10, sigmaby = 1, myfolds = 10, myseed1 = 123, myseed2 = 456, mygraphsize = 1000)
#'
#'
model_ols_gpm <- function(myinputspreadsheet = NULL, myinputdatasheet = NULL, mytag = NULL, mypredictionsheet = NULL, myresponse_ols = NULL, myresponse_gpm = "residuals", myterms = NULL, sigmafrom = 0.01, sigmato= 2, sigmaby = 0.01, myfolds = 10, myseed1 = 123, myseed2 = 456, mygraphsize = 1000) {

# Version register --------------------------------------------------------

  # 20211021 v1 01 by Alex Mitrani, initial version.
  # 20211022 v1 02 by Alex Mitrani, documented version.
  # 20211022 v1 03 by Alex Mitrani, put example back in.
  # 20211025 v1 04 by Alex Mitrani, Moved "cat(yellow(paste0("Sigma = ", mysigma, " produced the smallest value of RMSE out of all the values of sigma tested in the range from ", sigmafrom, " to ", sigmato, ". \n \n")))" and "stopifnot((mysigma > sigmafrom) & (mysigma < sigmato))" to model_gpm.

# Top ---------------------------------------------------------------------

  now1 <- Sys.time()
  datestring <- datestampr(myusername=TRUE)
  myinputspreadsheetprefix <- mytag

  logrun <- TRUE

  if (logrun==TRUE) {

    mylogfilename <- paste0(datestring, "_", myinputspreadsheetprefix,".txt")
    sink()
    sink(mylogfilename, split=TRUE)
    cat(yellow(paste0("A log of the output will be saved to ", mylogfilename, ". \n \n")))

  }

# import data -------------------------------------------------------------

  myinputdf <- read_excel(myinputspreadsheet, sheet = myinputdatasheet)

  myinputdf

# OLS model ---------------------------------------------------------------

  myols <- model_ols(mydf = myinputdf, myresponse = myresponse_ols, myterms = myterms)
  print(myols)

  myolsmodel <- myols[[1]]

  myolsmodeldf <- myols[[2]]

  mygpm_df <- myols[[3]]

  wb <- myols[[4]]

  myols_data <- myols[[5]]

# GPM model ---------------------------------------------------------------

  mygpm <- model_gpm(mydf = mygpm_df, myresponse = myresponse_gpm, myterms = myterms, sigmafrom = sigmafrom, sigmato= sigmato, sigmaby = sigmaby, myfolds = myfolds, myseed1 = myseed1, myseed2 = myseed2)
  print(mygpm)

# Prediction ----------------------------

  myols_data <- myols_data %>%
    mutate(ols_prediction = predict(myolsmodel, myols_data)) %>%
    mutate(gpm_prediction = predict(mygpm, myols_data)) %>%
    mutate(ols_gpm_prediction = ols_prediction + gpm_prediction) %>%
    mutate(id = row_number()) %>%
    relocate(id)

  myobservedresponse <- keepr(myinputdf, "id", myresponse_ols)
  myresponsedata <- keepr(myinputdf, myresponse_ols)

  myols_data <- myols_data %>%
    left_join(myobservedresponse)

  mygraphfilename <- paste0(datestring, "_", myinputspreadsheetprefix, "_scatterplot_ols_gpm.png")
  xvar <- data.matrix(myresponsedata)
  yvar <- myols_data$ols_gpm_prediction
  myylab <- "ols_gpm_prediction"
  png(mygraphfilename, width = mygraphsize, height = mygraphsize)
  plot(xvar, yvar, xlab = myresponse_ols, ylab = myylab, pch = 19, frame = FALSE)
  dev.off()

  cat(yellow(paste0("Scatter plot of ", myylab, " versus ", myresponse_ols,  " saved to ", mygraphfilename, ". \n \n")))

  mygraphfilename <- paste0(datestring, "_", myinputspreadsheetprefix, "_scatterplot_ols.png")
  xvar <- data.matrix(myresponsedata)
  yvar <- myols_data$ols_prediction
  myylab <- "ols_prediction"
  png(mygraphfilename, width = mygraphsize, height = mygraphsize)
  plot(xvar, yvar, xlab = myresponse_ols, ylab = "ols_prediction", pch = 19, frame = FALSE)
  dev.off()

  cat(yellow(paste0("Scatter plot of ", myylab, " versus ", myresponse_ols,  " saved to ", mygraphfilename, ". \n \n")))

  if (is.null(mypredictionsheet) == FALSE) {

    mypredictiondf <- read_excel(myinputspreadsheet, sheet = mypredictionsheet)

    mypredictiondf <- mypredictiondf %>%
      mutate(ols_prediction = predict(myolsmodel, mypredictiondf)) %>%
      mutate(gpm_prediction = predict(mygpm, mypredictiondf)) %>%
      mutate(prediction = ols_prediction + gpm_prediction) %>%
      mutate(id = row_number()) %>%
      relocate(id)

  }

# Save results workbook -------------------------------------------------

  sheetname <- "model_fit"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, myols_data)

  if (is.null(mypredictionsheet) == FALSE) {

    sheetname <- "predictions"
    addWorksheet(wb, sheetname)
    writeData(wb, sheetname, mypredictiondf)

  }

  myoutputfile <- paste0(datestring, "_", myinputspreadsheetprefix, "_outputs.xlsx")

  saveWorkbook(wb, file = myoutputfile, overwrite = TRUE)

  # tail ------------------------------------------------------------

  cat(yellow(paste0("The results have been saved to ", myoutputfile, ". \n \n")))

  print(gc())

  now2 <- Sys.time()
  elapsed_time <- now2 - now1

  cat(yellow(paste0("\n \n", "Elapsed time: \n ")))
  print(elapsed_time)
  cat(yellow(paste0("\n \n")))

  returnlist <- list(myols, mygpm)

  if (logrun==TRUE) {
    sink()
    cat(yellow(paste0("A log of the output has been saved to ", mylogfilename, ". \n \n")))
  }

  return(returnlist)

}

#

