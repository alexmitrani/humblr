


#' @name model_ols
#' @title Ordinary Least Squares Model
#' @description Estimates an Ordinary Least Squares model
#' @details The spreadsheet should have only one sheet with the following columns and one row for each variable to be simulated:
#'
#' @param mydf input data frame
#' @param myresponse the dependent variable
#' @param myterms a character vector containing the names of the independent variables.
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
#'
model_ols <- function(mydf = NULL, myresponse = NULL, myterms = NULL) {



# Version register --------------------------------------------------------

# 20211020 v1 01 by Alex Mitrani, Added several objects to a list that will be returned by the function.
# 20211021 v1 02 by Alex Mitrani, Added kernlab to the libraries.
# 20211021 v1 03 by Alex Mitrani, Removed gaupro and kernlab libraries - the gaussian process modelling will be done separately.
# 20211021 v1 04 by Alex Mitrani, Revised variable names to make them specific to the OLS model, added return list including the OLS model object.
# 20211021 v1 05 by Alex Mitrani, Rewrote to make the code generic
# 20211022 v1 06 by Alex Mitrani, documented version.


# examples of use ----------------------------------------------------------

  # OLS model ---------------------------------------------------------------

  myformula <- reformulate(termlabels = myterms, response = myresponse)

  myolsmodel <- lm(myformula, data = mydf)

  myolsmodel

  summary(myolsmodel)

  myolsmodeldf <- myolsmodel[["coefficients"]]

  myolsmodeldf <- as.data.frame(myolsmodeldf) %>%
    rownames_to_column(var = "variable")

  myolsmodeldf <- myolsmodeldf %>%
    rename(parameter = myolsmodeldf)

  myolsmodeldf <- myolsmodeldf %>%
    mutate(variable=ifelse(variable=="(Intercept)", "constant", variable))

  mydf_fit <- mydf %>%
    mutate(fitted = fitted.values(myolsmodel)) %>%
    mutate(residuals = resid(myolsmodel))

  myols_data <- keepr(mydf_fit, c(myterms))

  mygpm_data <- keepr(mydf_fit, c(myterms, "residuals"))

  # creates workbook object for use with openxlsx
  wb <- createWorkbook()

  sheetname <- "gpm_data"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mygpm_data)

  sheetname <- "ols_model_fit"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, mydf_fit)

  sheetname <- "ols_model_parameters"
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, myolsmodeldf)

  myreturnlist <- list(myolsmodel, myolsmodeldf, mygpm_data, wb, myols_data)

  return(myreturnlist)

}
