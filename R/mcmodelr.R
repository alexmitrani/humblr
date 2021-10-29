# Version history
# 20211028 v1 01 by Alex Mitrani.  First version.

#' @name mcmodelr
#' @title monte carlo simulation function for use with mcr and model_ols_gpm
#' @description takes model_ols_gpm, passes it input data produced with mcr, and passes back key results
#'
#' @import ggplot2
#'
#' @param myinputspreadsheet is the name of the spreadsheet with the input data.
#' @param myinputdatasheet the name of the sheet with the input data.
#' @param mypredictiondf (optional) a dataframe to be used as a basis for predictions
#' @param myresponse_ols the name of the dependent variable for the OLS model
#' @param myresponse_gpm the name of the dependent variable for the GPM model.  This will normally be the residuals of the OLS model: "residuals".
#' @param myterms a character vector containing the names of the independent variables.
#' @param sigmafrom the lower limit of the test range for sigma
#' @param sigmato the upper limit of the test range for sigma
#' @param sigmaby the increment of the test values for sigma
#' @param myfolds the number of folds for k-fold cross-validation
#' @param myseed1 the first random number seed
#' @param myseed2 the second random number seed
#' @param mygraphsize the pixel dimension for the output graphs
#' @param mygraphtextsize the size of all text elements on the graph.
#' @param mytag a character string that will be used in the output file names.  #'
#' @param nsims the desired number of Monte Carlo simulations
#'
#' @return
#' @export
#'
#' @examples
#'
#' nsims <- 1000
#'
#' myinputfile <- system.file("extdata", "risk_variable_distributions.xlsx", package = "humblr")
#' mytest1 <- mcr(myinputfile, nsims = nsims, myseed = 12345L)
#' mytest1
#'
#' mycorrmatfile <- system.file("extdata", "correlation_matrix.xlsx", package = "humblr")
#' mytest2 <- mcr(myinputfile, mycorrmatfilename = mycorrmatfile, nsims = nsims, myseed = 12345L)
#' mytest2
#'
#' myinputfile <- system.file("extdata", "model_ols_gpm_test.xlsx", package = "humblr")
#' mymodel_results <- mcmodelr(nsims = nsims, myinputspreadsheet = myinputfile, myinputdatasheet = "data", mypredictiondf = mytest2, myresponse_ols = "depvar", myresponse_gpm = "residuals", myterms = c("var2",	"var3",	"var4",	"var5",	"var6",	"var7",	"var8",	"var9",	"var10"), sigmafrom = 1, sigmato= 10, sigmaby = 1, myfolds = 10, myseed1 = 123, myseed2 = 456, mygraphsize = 1000, mygraphtextsize = 20, mytag = "mymodel")
#'
#'
#'
mcmodelr <- function(nsims = NULL, myinputspreadsheet = NULL, myinputdatasheet = NULL, mypredictiondf = NULL, myresponse_ols = NULL, myresponse_gpm = NULL, myterms = NULL, sigmafrom = NULL, sigmato= NULL, sigmaby = NULL, myfolds = NULL, myseed1 = NULL, myseed2 = NULL, mygraphsize = 1000, mygraphtextsize = 10, mytag = NULL){

  mymodel <- model_ols_gpm(myinputspreadsheet = myinputspreadsheet, myinputdatasheet = myinputdatasheet, mypredictiondf = mypredictiondf, myresponse_ols = myresponse_ols, myresponse_gpm = myresponse_gpm, myterms = myterms, sigmafrom = sigmafrom, sigmato= sigmato, sigmaby = sigmaby, myfolds = myfolds, myseed1 = myseed1, myseed2 = myseed2, mygraphsize = mygraphsize, mytag = mytag)
  myolsmodel <- mymodel[[1]]
  mygpmmodel <- mymodel[[2]]
  mypredictions <- mymodel[[3]]

  myprediction <- mypredictions %>%
    select(prediction) %>%
    arrange(prediction) %>%
    mutate(prediction = exp(prediction)) %>%
    mutate(mycounter=1/nsims) %>%
    mutate(cumulative_proportion=cumsum(mycounter))

  myplotfilename <- paste0(mytag, ".png")

  png(file = myplotfilename, width = mygraphsize, height = mygraphsize)

  print({
    p <- ggplot(myprediction, aes(x = prediction, y = cumulative_proportion))
    p + geom_line(size = 1)
    p + theme(text = element_text(size = mygraphtextsize))
  })

  dev.off()

  prediction = as.vector(myprediction$prediction)

  prediction_p0 = min(prediction)
  prediction_p1 = as.numeric(quantile(prediction,probs = 0.01))
  prediction_p10 = as.numeric(quantile(prediction,probs = 0.10))
  prediction_p25 = as.numeric(quantile(prediction,probs = 0.25))
  prediction_p50 = as.numeric(quantile(prediction,probs = 0.25))
  prediction_p75 = as.numeric(quantile(prediction,probs = 0.75))
  prediction_p90 = as.numeric(quantile(prediction,probs = 0.90))
  prediction_p99 = as.numeric(quantile(prediction,probs = 0.99))
  prediction_p100 = max(prediction)

  myprediction_summary <- rbind(prediction_p0, prediction_p1, prediction_p10, prediction_p25, prediction_p50, prediction_p75, prediction_p90, prediction_p99, prediction_p100)

  myreturnlist <- list(myolsmodel, mygpmmodel, mypredictions, myprediction_summary)

  return(myreturnlist)

}
