
# Version history
# 20211008 v1 01 by Alex Mitrani.  First version.
# 20211012 v1 02 by Alex Mitrani.  Added criterium to the list of summary outputs.
# 20211012 v1 03 by Alex Mitrani.  Set most arguments to NULL by default.

#' @name hypercuber
#' @title a wrapper for the lhs package
#' @description facilitates the generation of Latin hypercubes, graphs the results, and returns the results in a dataframe
#' @details based on the examples in the lhs help documentation
#'
#' @import tidyverse
#' @import crayon
#' @import lhs
#'
#' @param myname is the name to be used to name output files
#' @param myseed the random number seed to be used
#' @param myn the number of draws
#' @param myk the number of variables
#' @param myalgorithm the optimisation algorithm to be used: "randomLHS", "optimumLHS", "maximinLHS", "improvedLHS", or "geneticLHS".
#'
#' @return
#' @export
#'
#' @examples
#' mytest1 <- hypercuber(mygraphname = "mytest1", myn=10, myk=5, myalgorithm = "randomLHS")
#' mytest1
#'
#' mytest2 <- hypercuber(mygraphname = "mytest2", myn=10, myk=5, myalgorithm = "optimumLHS", mymaxsweeps = 4, myeps = 0.01)
#' mytest2
#'
#' mytest3 <- hypercuber(mygraphname = "mytest3", myn=10, myk=5, myalgorithm = "maximinLHS", mydup = 5)
#' mytest3
#'
#' mytest4 <- hypercuber(mygraphname = "mytest4", myn=10, myk=5, myalgorithm = "improvedLHS", mydup = 5)
#' mytest4
#'
#' mytest5 <- hypercuber(mygraphname = "mytest5", myn=10, myk=5, myalgorithm = "geneticLHS", mypop = 1000, mygen = 8, mypmut = 0.1, mycriterium = "S")
#' mytest5
#'
#' mytest6 <- hypercuber(mygraphname = "mytest6", myn=10, myk=5, myalgorithm = "geneticLHS", mypop = 1000, mygen = 8, mypmut = 0.1, mycriterium = "Maximin")
#' mytest6
#'

hypercuber <- function(mygraphname = NULL, myseed = 12345L, myn = 99, myk = 9, myalgorithm = "randomLHS", mymaxsweeps = NULL, myeps = NULL, mydup = NULL, mypop = NULL, mygen = NULL, mypmut = NULL, mycriterium = "NA", mygraphsize = 1000, mypch = 19, mycol = "blue", mycex = 0.5) {

  datestring <- datestampr(myusername=TRUE)

  if (is.null(mygraphname)==TRUE) {

    mygraphname <- datestring

  }

  myseed <- as.integer(myseed)
  # set the seed for reproducibility
  set.seed(myseed)

  if (myalgorithm == "randomLHS") {

    mymatrix <- randomLHS(n = myn, k = myk)

  } else if (myalgorithm == "optimumLHS") {

    stopifnot(is.null(mymaxsweeps)==FALSE)
    stopifnot(is.null(myeps)==FALSE)

    mymatrix <- optimumLHS(n = myn, k = myk, maxSweeps = mymaxsweeps, eps = myeps)

  } else if (myalgorithm == "maximinLHS") {

    stopifnot(is.null(mydup)==FALSE)

    mymatrix <- maximinLHS(n = myn, k = myk, dup = mydup)

  } else if (myalgorithm == "improvedLHS") {

    stopifnot(is.null(mydup)==FALSE)

    mymatrix <- improvedLHS(n = myn, k = myk, dup = mydup)

  } else if (myalgorithm == "geneticLHS") {

    stopifnot(is.null(mypop)==FALSE)
    stopifnot(is.null(mygen)==FALSE)
    stopifnot(is.null(mypmut)==FALSE)
    stopifnot(is.null(mycriterium)==FALSE)

    mymatrix <- geneticLHS(n = myn, k = myk, pop = mypop, gen = mygen, pMut = mypmut, criterium = mycriterium)

  }

  cat(yellow("\n", "Min Distance btween pts:", min(dist(mymatrix)), "\n \n"))
  cat(yellow("\n", "Mean Distance btween pts:", mean(dist(mymatrix)), "\n \n"))
  cat(yellow("\n", "Max Correlation btween pts:", max(abs(cor(mymatrix)-diag(myk))), "\n \n"))

  mygraphfilename <- paste0(mygraphname, ".png")

  png(mygraphfilename, width = mygraphsize, height = mygraphsize)
  pairs(mymatrix, pch = mypch, col = mycol, cex = mycex)
  dev.off()

  criterium <- mycriterium

  seed <- myseed
  c1 <- as.data.frame(seed)

  type <- myalgorithm
  c2 <- as.data.frame(type)

  c3 <- as.data.frame(criterium)

  min_dist <- min(dist(mymatrix))
  c4 <- as.data.frame(min_dist)

  mean_dist <- mean(dist(mymatrix))
  c5 <- as.data.frame(mean_dist)

  max_corr <- c(max(abs(cor(mymatrix)-diag(myk))))
  c6 <- as.data.frame(max_corr)

  mydf1 <- as.data.frame(mymatrix) %>%
    mutate(id = row_number()) %>%
    relocate(id)

  mydf2 <- cbind(c1, c2, c3, c4, c5, c6)

  mylist <- list(mydf1, mydf2)

  return(mylist)

}
