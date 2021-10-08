
# Version history
# 20211008 v1 01 by Alex Mitrani.  First version.  

#' @name hypercuber
#' @title a wrapper for the lhs package
#' @description facilitates the generation of Latin hypercubes, graphs the results, and returns the results in a dataframe
#' @details based on the examples in the lhs help documentation
#'
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
#' mytest1 <- hypercuber(myname = "mytest1", myseed = myseed, myn=120, myk=12, myalgorithm = "randomLHS")
#' mytest2 <- hypercuber(myseed, n=120, k=12, "optimumLHS", mymaxsweeps = 4, myeps = 0.01)
#' mytest3 <- hypercuber(myseed, n=120, k=12, "maximinLHS", mydup = 5)
#' mytest4 <- hypercuber(myseed, n=120, k=12, "improvedLHS", mydup = 5)
#' mytest5 <- hypercuber(myseed, n=120, k=12, "geneticLHS", mypop = 1000, mygen = 8, mypmut = 0.1, mycriterium = "S")
#' mytest6 <- hypercuber(myseed, n=120, k=12, "geneticLHS", mypop = 1000, mygen = 8, mypmut = 0.1, mycriterium = "Maximin")
#'

hypercuber <- function(myname = NULL, myseed = 123456789, myn = 99, myk = 9, myalgorithm = "randomLHS", mymaxsweeps = 4, myeps = 0.01, mydup = 5, mypop = 1000, mygen = 8, mypmut = 0.1, mycriterium = "S", mygraphsize = 1000, mypch = 19, mycol = "blue", mycex = 0.5) {

  datestring <- datestampr(myusername=TRUE)
  
  if (is.null(myname)==TRUE) {
    
    myname <- datestring
    
  }
  
  # set the seed for reproducibility
  set.seed(myseed)
  
  if (myalgorithm == "randomLHS") {
    
    mymatrix <- randomLHS(n = myn, k = myk)
    
  } else if (myalgorithm == "optimumLHS") {
    
    stopifnot(is.null(mymaxsweeps)==FALSE)
    stopifnot(is.null(myeps)==FALSE)
    
    mymatrix <- optimumLHS(n = myn, k = myk, maxSweeps = mymaxsweeps, eps = myeps)

  } else if (myalgorithm == "optimumLHS") {
    
    stopifnot(is.null(mydup)==FALSE)
    
    mymatrix <- maximinLHS(n = myn, k = myk, dup = mydup)
    
  } else if (myalgorithm == "optimumLHS") {
    
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
  
  myfilename <- paste0(myname, ".png")
  
  png(myfilename, width = mygraphsize, height = mygraphsize)
  pairs(mymatrix, pch = mypch, col = mycol, cex = mycex)
  dev.off()
  
  seed <- myseed
  c1 <- as.data.frame(seed)
  
  type <- myalgorithm
  c2 <- as.data.frame(type)
  
  min_dist <- min(dist(mymatrix))
  c3 <- as.data.frame(min_dist)
  
  mean_dist <- mean(dist(mymatrix))
  c4 <- as.data.frame(mean_dist)
  
  max_corr <- c(max(abs(cor(mymatrix)-diag(myk))))
  c5 <- as.data.frame(max_corr)
  
  mydf1 <- as.data.frame(mymatrix) %>%
    mutate(id = row_number()) %>%
    relocate(id)
    
  mydf2 <- cbind(c1, c2, c3, c4, c5)
  
  mylist <- list(mydf1, mydf2)
  
  return(mylist)
 
}