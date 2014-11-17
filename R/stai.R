#' State-trait Anxiety inventory for Adults Scoreing Key (Form Y1)
#'
#' State-trait Anxiety inventory for Adults Scoreing Key (Form Y1)
#' @param Q a matrix or data.frame of 20 columns (corresponing in a ordered
#' way to each, so therefore Q[,1] is the first item of the scale) filled
#' by 1 (Not at all) to 4 (Very much so) values
#' @param NA.treshold compute the score only if # of missing items is less
#' than this value
#' @export 
stai_Y1 <- function( Q = NULL, NA.treshold = 3) {

  if((!(is.data.frame(Q) | is.matrix(Q))) | (ncol(Q)!=20) | (! all(as.matrix(Q) %in% c(NA,1:4))) )
    stop("Q must be a data.frame (or matrix) with 20 columns")
  
  scores <- cbind((5 - Q[, 1]), 
                  (5 - Q[, 2]),
                  (Q[, 3]),
                  (Q[, 4]),
                  (5 - Q[, 5]),
                  (Q[, 6]),
                  (Q[, 7]),
                  (5 - Q[, 8]) ,
                  (Q[, 9])     ,
                  (5 - Q[,10]) ,
                  (5 - Q[,11]) ,
                  (Q[,12])     ,
                  (Q[,13])     ,
                  (Q[,14])     ,
                  (5 - Q[,15]) ,
                  (5 - Q[,16]) ,
                  (Q[,17]) ,
                  (Q[,18]) ,
                  (5 - Q[,19]) ,
                  (5 - Q[,20])) 

  ## base calculation
  stai <- apply(scores, 1, sum)
  ## missing values management: count number of missing items
  nmiss <- apply(scores, 1, function(x) sum(is.na(x)))
  ## missing values management: stai by mean of available items
  NAScore <- unname(apply(scores, 1, function(x) round(mean(x, na.rm = TRUE)*20, 0)))
  ## if all missing values, change score from NaN to NA
  NAScore[ is.nan(NAScore) ] <- NA
  ## missing values management: score fixing
  stai <- ifelse(nmiss %in% 0, stai, ifelse(nmiss < NA.treshold, NAScore , NA))
    
  return(stai)
}


## #' State-trait Anxiety inventory for Adults Scoreing Key (Form Y2)
## #'
## #' State-trait Anxiety inventory for Adults Scoreing Key (Form Y2)
## #' @param Q a matrix or data.frame of 20 columns (corresponing in a ordered
## #' way to each, so therefore Q[,1] is the first item of the scale) filled
## #' by 1 (Almost never) to 4 (Almost always) values
## #' @export 
## stai_Y2 <- function( Q = NULL) {

##   if((!(is.data.frame(Q) | is.matrix(Q))) | (ncol(Q)!=20) | (! all(as.matrix(Q) %in% c(NA,1:4))) )
##     stop("Q must be a data.frame (or matrix) with 20 columns")

##   score <-
##     (5 - Q[, 1]) + 
##     (Q[, 2])     +
##     (5 - Q[, 3]) +
##     (Q[, 4])     + 
##     (Q[, 5])     + 
##     (5 - Q[, 6]) + 
##     (5 - Q[, 7]) + 
##     (Q[, 8])     +
##     (Q[, 9])     +
##     (5 - Q[,10]) +
##     (Q[,11])     +
##     (Q[,12])     +
##     (5 - Q[,13]) +
##     (5 - Q[,14]) +
##     (Q[,15])     +
##     (5 - Q[,16]) +
##     (Q[,17])     +
##     (Q[,18])     +
##     (5 - Q[,19]) +
##     (Q[,20]) 
	
##   return(score)
## }
