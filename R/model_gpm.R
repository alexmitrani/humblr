


#' @name model_gpm
#' @title Gaussian Process Model.
#' @description Estimates a Gaussian Process Model.  
#' @details 
#'
#' @param mydf input data frame
#' @param sigmafrom the lower limit of the test range for sigma
#' @param sigmato the upper limit of the test range for sigma
#' @param sigmaby the increment of the test values for sigma
#' @param myfolds the number of folds for k-fold cross-validation
#' @param myseed1 the first random number seed
#' @param myseed2 the second random number seed
#' @param myresponse the name of the dependent variable for the GPM model.  This will normally be the residuals of the OLS model: "residuals".  
#' @param myterms a character vector containing the names of the independent variables.  
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
model_gpm <- function(mydf = NULL, myresponse = NULL, myterms = NULL, sigmafrom = 0.01, sigmato= 2, sigmaby = 0.01, myfolds = 10, myseed1 = 123, myseed2 = 456) {
  
# Version register --------------------------------------------------------

  # 20211021 v1 01 by Alex Mitrani, initial version.  
  # 20211021 v1 02 by Alex Mitrani, added use of kernlab for the gaussian process modelling, and caret for the k-fold cross-validation.  
  # 20211022 v1 03 by Alex Mitrani, documented version.   
  # 20211025 v1 04 by Alex Mitrani, Added "myfinalmodel <- mymodel[["finalModel"]]" and "return(myfinalmodel)"
  # 20211025 v1 05 by Alex Mitrani, Moved in "cat(yellow(paste0("Sigma = ", mysigma, " produced the smallest value of RMSE out of all the values of sigma tested in the range from ", sigmafrom, " to ", sigmato, ". \n \n")))" and "stopifnot((mysigma > sigmafrom) & (mysigma < sigmato))" from model_ols_gpm.
  # 20211028 v1 06 by Alex Mitrani, Changed line that defines final model: "myfinalmodel <- gausspr(x = myformula, data = mydf, type = "regression", kernel = "rbfdot", kpar = list(sigma = mysigma), variance.model = TRUE)"

# Train model -------------------------------------------------------------

  gpmgrid <- expand.grid(sigma = seq(from = sigmafrom, to = sigmato, by = sigmaby))
  
  set.seed(myseed1)
  
  train_control<- trainControl(method="cv", number = myfolds, savePredictions = TRUE)
  
  set.seed(myseed2)
  
  myformula <- reformulate(termlabels = myterms, response = myresponse)
  
  mymodel <- train(myformula, data = mydf, 
                    method = "gaussprRadial", 
                    trControl = train_control,
                    tuneGrid = gpmgrid)
  
  mysigma <- as.numeric(mymodel[["bestTune"]][["sigma"]])
  
  cat(yellow(paste0("Sigma = ", mysigma, " produced the smallest value of RMSE out of all the values of sigma tested in the range from ", sigmafrom, " to ", sigmato, ". \n \n")))
  
  stopifnot((mysigma > sigmafrom) & (mysigma < sigmato))
  
  myfinalmodel <- gausspr(x = myformula, data = mydf, type = "regression", kernel = "rbfdot", kpar = list(sigma = mysigma), variance.model = TRUE)
  
  # myfinalmodel <- mymodel[["finalModel"]]
  
# Return results ----------------------------------------------------------
  
  return(myfinalmodel)

}

