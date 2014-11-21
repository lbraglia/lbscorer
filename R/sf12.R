#' SF12 questionnaire scoring
#'
#' SF12 questionnaire scoring
#' @param X a \code{\link{matrix}} or \code{\link{data.frame}} of 12
#' columns, containing questionnaire items. In order from left to right: gh1, pf02, pf04, rp2,
#' rp3, re2, re3, bp2, mh3, vt2, mh4, sf2.
#' @note
#' This is an R port of SAS algorithm by Apolone and Mosconi found
#' \href{http://crc.marionegri.it/qdv/index.php?page=sf12}{here}.
#' 
#' SF-12  is a registered trademark of medical outcomes trust.
#' @examples
#' ## -------------------------
#' ## Algorithm test/validation
#' ## -------------------------
#' scores <- sf12(sf12sample)
#' ## website data test (printing with many decimals for 10 selected
#' ## questionnaires)
#' web <- c(1,2,4,5,11,27,28,31,37,39)
#' print(scores[web,], digits = 6)
#' ## SF12 Manual checks
#' print(unlist(lapply(scores, mean)), digits = 3)
#' print(unlist(lapply(scores, sd)), digits = 3)
#' print(lapply(scores, range), digits = 3)
#' ## Correlations
#' db <- cbind(sf12sample, scores)
#' var.order <- c(2:5,8,1,10,12,6,7,9,11)
#' cors <- cor(db)[var.order, 13:14]
#' print(cors, digits = 1)
#' ## Fine: reversed item have reverse sign correlation coefficients 
#' @export 
sf12 <- function( X = NULL ) {

  if((!(is.data.frame(X) | is.matrix(X))) | (ncol(X)!=12) )
    stop("X must be a data.frame (or matrix) with 12 columns")

  X <- as.data.frame(lapply(as.data.frame(X), as.integer))
  names(X) <- c("gh1", "pf02", "pf04", "rp2", "rp3", "re2", "re3", "bp2",
                "mh3", "vt2", "mh4", "sf2" )

  ## 
  ## Step 1a - Data cleaning
  ## 
  twopt <- c("rp2", "rp3", "re2", "re3")
  threept <- c("pf02", "pf04")
  fivept <- c("gh1", "bp2", "sf2")
  sixpt <- c("vt2", "mh3", "mh4")
  Recode <- function(x, Max) replace(x, x < 1L | x > Max, NA)
  X[, twopt] <- lapply(X[, twopt], Recode, Max = 2)
  X[, threept] <- lapply(X[, threept], Recode, Max = 3)
  X[, fivept] <- lapply(X[, fivept], Recode, Max = 5)
  X[, sixpt] <- lapply(X[, sixpt], Recode, Max = 6)
  
  ## 
  ## Step 1b - Reverse scoring
  ## 
  X$rbp2  <-  6 - X$bp2
  X$rgh12 <-  6 - X$gh1
  X$rvt2  <-  7 - X$vt2
  X$rmh3  <-  7 - X$mh3  
  
  ## 
  ## Step 2 - Create indicator variables from item response choices
  ##
  X$pf02_1 <- as.numeric(X$pf02 == 1L) 
  X$pf02_2 <- as.numeric(X$pf02 == 2L) 

  X$pf04_1 <- as.numeric(X$pf04 == 1L) 
  X$pf04_2 <- as.numeric(X$pf04 == 2L) 

  X$rp2_1 <- as.numeric(X$rp2 == 1L) 

  X$rp3_1 <- as.numeric(X$rp3 == 1L) 

  X$bp2_1 <- as.numeric(X$rbp2 == 1L) 
  X$bp2_2 <- as.numeric(X$rbp2 == 2L) 
  X$bp2_3 <- as.numeric(X$rbp2 == 3L) 
  X$bp2_4 <- as.numeric(X$rbp2 == 4L) 

  X$gh1_1 <- as.numeric(X$rgh1 == 1L) 
  X$gh1_2 <- as.numeric(X$rgh1 == 2L) 
  X$gh1_3 <- as.numeric(X$rgh1 == 3L) 
  X$gh1_4 <- as.numeric(X$rgh1 == 4L) 

  X$vt2_1 <- as.numeric(X$rvt2 == 1L) 
  X$vt2_2 <- as.numeric(X$rvt2 == 2L) 
  X$vt2_3 <- as.numeric(X$rvt2 == 3L) 
  X$vt2_4 <- as.numeric(X$rvt2 == 4L) 
  X$vt2_5 <- as.numeric(X$rvt2 == 5L) 

  X$sf2_1 <- as.numeric(X$sf2 == 1L) 
  X$sf2_2 <- as.numeric(X$sf2 == 2L) 
  X$sf2_3 <- as.numeric(X$sf2 == 3L) 
  X$sf2_4 <- as.numeric(X$sf2 == 4L) 

  X$re2_1 <- as.numeric(X$re2 == 1L) 

  X$re3_1 <- as.numeric(X$re3 == 1L) 

  X$mh3_1 <- as.numeric(X$rmh3 == 1L) 
  X$mh3_2 <- as.numeric(X$rmh3 == 2L) 
  X$mh3_3 <- as.numeric(X$rmh3 == 3L) 
  X$mh3_4 <- as.numeric(X$rmh3 == 4L) 
  X$mh3_5 <- as.numeric(X$rmh3 == 5L) 

  X$mh4_1 <- as.numeric(X$mh4 == 1L) 
  X$mh4_2 <- as.numeric(X$mh4 == 2L) 
  X$mh4_3 <- as.numeric(X$mh4 == 3L) 
  X$mh4_4 <- as.numeric(X$mh4 == 4L) 
  X$mh4_5 <- as.numeric(X$mh4 == 5L) 
  
  ## 
  ## Step 3 - Weighting and aggregation of indicator variables using
  ##          physical and mental regression weights
  ##

  RAWPCS12 <- with(X,
                   (-7.23216*pf02_1) + (-3.45555*pf02_2) +
                   (-6.24397*pf04_1) + (-2.73557*pf04_2) +
                   (-4.61617*rp2_1) + 
                   (-5.51747*rp3_1) +
                   (-11.25544*bp2_1) + (-8.38063*bp2_2) +
                   (-6.50522*bp2_3) + (-3.80130*bp2_4) + (-8.37399*gh1_1) +
                   (-5.56461*gh1_2) + (-3.02396*gh1_3) + (-1.31872*gh1_4) +
                   (-2.44706*vt2_1) + (-2.02168*vt2_2) + (-1.6185*vt2_3) +
                   (-1.14387*vt2_4) + (-0.42251*vt2_5) + (-0.33682*sf2_1) +
                   (-0.94342*sf2_2) + (-0.18043*sf2_3) + (0.11038*sf2_4) +
                   (3.04365*re2_1) + (2.32091*re3_1) + (3.46638*mh3_1) +
                   (2.90426*mh3_2) + (2.37241*mh3_3) + (1.36689*mh3_4) +
                   (0.66514*mh3_5) + (4.61446*mh4_1) + (3.41593*mh4_2) +
                   (2.34247*mh4_3) + (1.28044*mh4_4) + (0.41188*mh4_5))

  RAWMCS12 <- with(X,
                   (3.93115*pf02_1) + (1.8684*pf02_2) +
                   (2.68282*pf04_1) + (1.43103*pf04_2) + (1.4406*rp2_1) +
                   (1.66968*rp3_1) + (1.48619*bp2_1) + (1.76691*bp2_2) +
                   (1.49384*bp2_3) + (0.90384*bp2_4) + (-1.71175*gh1_1) +
                   (-0.16891*gh1_2) + (0.03482*gh1_3) + (-0.06064*gh1_4) +
                   (-6.02409*vt2_1) + (-4.88962*vt2_2) + (-3.29805*vt2_3) +
                   (-1.65178*vt2_4) + (-0.92057*vt2_5) + (-6.29724*sf2_1) +
                   (-8.26066*sf2_2) + (-5.63286*sf2_3) + (-3.13896*sf2_4) +
                   (-6.82672*re2_1) + (-5.69921*re3_1) + (-10.19085*mh3_1) +
                   (-7.92717*mh3_2) + (-6.31121*mh3_3) + (-4.09842*mh3_4) +
                   (-1.94949*mh3_5) + (-16.15395*mh4_1) + (-10.77911*mh4_2) +
                   (-8.09914*mh4_3) + (-4.59055*mh4_4) + (-1.95934*mh4_5))

  
  ##
  ## Step 4 - Norm-based standardization of scale scores
  ## 
  PCS12 <- RAWPCS12 + 56.57706
  MCS12 <- RAWMCS12 + 60.75781

  return(data.frame(PCS12, MCS12))
  
}


#' SF12 sample dataset
#'
#' SF12 sample of 50 questionnaires \code{sf12} testing purposes.
#'
#' @format A data frame with 50 rows and 13 columns (1 id columns, the
#' other SF12 items)
#' @source SF12 italian manual at
#' \url{http://crc.marionegri.it/qdv/index.php?page=sf12}
#' 
"sf12sample"
