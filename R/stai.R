#' State-trait Anxiety inventory for Adults Scoreing Key (Form Y1)
#'
#' State-trait Anxiety inventory for Adults Scoreing Key (Form Y1)
#' @param Q a matrix or data.frame of 20 columns (corresponing in a ordered
#' way to each, so therefore Q[,1] is the first item of the scale) filled
#' by 1 (Not at all) to 4 (Very much so) values
#' @export 
stai_Y1 <- function( Q = NULL) {

  if((!(is.data.frame(Q) | is.matrix(Q))) | (ncol(Q)!=20) | (! all(as.matrix(Q) %in% c(NA,1:4))) )
    stop("Q must be a data.frame (or matrix) with 20 columns")

  score <-
    (5 - Q[, 1]) + 
    (5 - Q[, 2]) +
    (Q[, 3])     +
    (Q[, 4])     + 
    (5 - Q[, 5]) + 
    (Q[, 6])     + 
    (Q[, 7])     + 
    (5 - Q[, 8]) +
    (Q[, 9])     +
    (5 - Q[,10]) +
    (5 - Q[,11]) +
    (Q[,12])     +
    (Q[,13])     +
    (Q[,14])     +
    (5 - Q[,15]) +
    (5 - Q[,16]) +
    (Q[,17]) +
    (Q[,18]) +
    (5 - Q[,19]) +
    (5 - Q[,20]) 
	
  return(score)
}


#' State-trait Anxiety inventory for Adults Scoreing Key (Form Y2)
#'
#' State-trait Anxiety inventory for Adults Scoreing Key (Form Y2)
#' @param Q a matrix or data.frame of 20 columns (corresponing in a ordered
#' way to each, so therefore Q[,1] is the first item of the scale) filled
#' by 1 (Almost never) to 4 (Almost always) values
#' @export 
stai_Y2 <- function( Q = NULL) {

  if((!(is.data.frame(Q) | is.matrix(Q))) | (ncol(Q)!=20) | (! all(as.matrix(Q) %in% c(NA,1:4))) )
    stop("Q must be a data.frame (or matrix) with 20 columns")

  score <-
    (5 - Q[, 1]) + 
    (Q[, 2])     +
    (5 - Q[, 3]) +
    (Q[, 4])     + 
    (Q[, 5])     + 
    (5 - Q[, 6]) + 
    (5 - Q[, 7]) + 
    (Q[, 8])     +
    (Q[, 9])     +
    (5 - Q[,10]) +
    (Q[,11])     +
    (Q[,12])     +
    (5 - Q[,13]) +
    (5 - Q[,14]) +
    (Q[,15])     +
    (5 - Q[,16]) +
    (Q[,17])     +
    (Q[,18])     +
    (5 - Q[,19]) +
    (Q[,20]) 
	
  return(score)
}
