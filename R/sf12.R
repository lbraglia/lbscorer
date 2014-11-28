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
#' (scores <- sf12(sf12sample))
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

  ## *****************************************************************;
  ## ***               STEP 1: DATA CLEANING/REVERSE SCORING       ***;
  ## *****************************************************************;

  ## ARRAY TWOPT RP2 RP3 RE2 RE3;
  ##   DO OVER TWOPT;
  ##   IF TWOPT LT 1 OR TWOPT GT 2 THEN TWOPT = .;
  ## END;
  ## ARRAY THREEPT PF02 PF04;
  ##   DO OVER THREEPT;
  ##   IF THREEPT LT 1 OR THREEPT GT 3 THEN THREEPT = .;
  ## END;
  ## ARRAY FIVEPT GH1 BP2 SF2;
  ##   DO OVER FIVEPT;
  ##   IF FIVEPT LT 1 OR FIVEPT GT 5 THEN FIVEPT = .;
  ## END;
  ## ARRAY SIXPT VT2 MH3 MH4;
  ##   DO OVER SIXPT;
  ##   IF SIXPT LT 1 OR SIXPT GT 6 THEN SIXPT = .;
  ## END;

  twopt <- c("rp2", "rp3", "re2", "re3")
  threept <- c("pf02", "pf04")
  fivept <- c("gh1", "bp2", "sf2")
  sixpt <- c("vt2", "mh3", "mh4")
  ## outRangeNA is defined in utils.R
  X[, twopt] <- lapply(X[, twopt], outRangeNA, Max = 2L)
  X[, threept] <- lapply(X[, threept], outRangeNA, Max = 3L)
  X[, fivept] <- lapply(X[, fivept], outRangeNA, Max = 5L)
  X[, sixpt] <- lapply(X[, sixpt], outRangeNA, Max = 6L)

  ## RBP2=6-BP2;
  ## RGH1=6-GH1;
  ## RVT2=7-VT2;
  ## RMH3=7-MH3;
  
  X$rbp2  <-  6 - X$bp2
  X$rgh1  <-  6 - X$gh1
  X$rvt2  <-  7 - X$vt2
  X$rmh3  <-  7 - X$mh3

  ## *****************************************************************;
  ## *               STEP 2: CREATE INDICATOR VARIABLES FROM         *
  ## *                       ITEM RESPONSE CHOICES                   *
  ## *****************************************************************;
  ## PF02_1 = .;
  ##   if PF02 = . then PF02_1 = .; else
  ##   if PF02 = 1 then PF02_1 = 1; else PF02_1 = 0;
  ## PF02_2 = .;
  ##   if PF02 = . then PF02_2 = .; else
  ##   if PF02 = 2 then PF02_2 = 1; else PF02_2 = 0;

  X$pf02_1 <- as.numeric(X$pf02 == 1L) 
  X$pf02_2 <- as.numeric(X$pf02 == 2L) 
  
  ## PF04_1 = .;
  ##   if PF04 = . then PF04_1 = .; else
  ##   if PF04 = 1 then PF04_1 = 1; else PF04_1 = 0;
  ## PF04_2 = .;
  ##   if PF04 = . then PF04_2 = .; else
  ##   if PF04 = 2 then PF04_2 = 1; else PF04_2 = 0;

  X$pf04_1 <- as.numeric(X$pf04 == 1L) 
  X$pf04_2 <- as.numeric(X$pf04 == 2L) 

  ## RP2_1 = .;
  ##   if RP2 = . then RP2_1 = .; else
  ##   if RP2 = 1 then RP2_1 = 1; else RP2_1 = 0;

  X$rp2_1 <- as.numeric(X$rp2 == 1L) 

  ## RP3_1 = .;
  ##   if RP3 = . then RP3_1 = .; else
  ##   if RP3 = 1 then RP3_1 = 1; else RP3_1 = 0;

  X$rp3_1 <- as.numeric(X$rp3 == 1L) 

  ## BP2_1 = .;
  ##   if RBP2 = . then BP2_1 = .; else
  ##   if RBP2 = 1 then BP2_1 = 1; else BP2_1 = 0;
  ## BP2_2 = .;
  ##   if RBP2 = . then BP2_2 = .; else
  ##   if RBP2 = 2 then BP2_2 = 1; else BP2_2 = 0;
  ## BP2_3 = .;
  ##   if RBP2 = . then BP2_3 = .; else
  ##   if RBP2 = 3 then BP2_3 = 1; else BP2_3 = 0;
  ## BP2_4 = .;
  ##   if RBP2 = . then BP2_4 = .; else
  ##   if RBP2 = 4 then BP2_4 = 1; else BP2_4 = 0;

  X$bp2_1 <- as.numeric(X$rbp2 == 1L) 
  X$bp2_2 <- as.numeric(X$rbp2 == 2L) 
  X$bp2_3 <- as.numeric(X$rbp2 == 3L) 
  X$bp2_4 <- as.numeric(X$rbp2 == 4L) 

  ## GH1_1 = .;
  ##   if RGH1 = . then GH1_1 = .; else
  ##   if RGH1 = 1 then GH1_1 = 1; else GH1_1 = 0;
  ## GH1_2 = .;
  ##   if RGH1 = . then GH1_2 = .; else
  ##   if RGH1 = 2 then GH1_2 = 1; else GH1_2 = 0;
  ## GH1_3 = .;
  ##   if RGH1 = . then GH1_3 = .; else
  ##   if RGH1 = 3 then GH1_3 = 1; else GH1_3 = 0;
  ## GH1_4 = .;
  ##   if RGH1 = . then GH1_4 = .; else
  ##   if RGH1 = 4 then GH1_4 = 1; else GH1_4 = 0;

  X$gh1_1 <- as.numeric(X$rgh1 == 1L) 
  X$gh1_2 <- as.numeric(X$rgh1 == 2L) 
  X$gh1_3 <- as.numeric(X$rgh1 == 3L) 
  X$gh1_4 <- as.numeric(X$rgh1 == 4L) 

  ## VT2_1 = .;
  ##   if RVT2 = . then VT2_1 = .; else
  ##   if RVT2 = 1 then VT2_1 = 1; else VT2_1 = 0;
  ## VT2_2 = .;
  ##   if RVT2 = . then VT2_2 = .; else
  ##   if RVT2 = 2 then VT2_2 = 1; else VT2_2 = 0;
  ## VT2_3 = .;
  ##   if RVT2 = . then VT2_3 = .; else
  ##   if RVT2 = 3 then VT2_3 = 1; else VT2_3 = 0;
  ## VT2_4 = .;
  ##   if RVT2 = . then VT2_4 = .; else
  ##   if RVT2 = 4 then VT2_4 = 1; else VT2_4 = 0;
  ## VT2_5 = .;
  ##   if RVT2 = . then VT2_5 = .; else
  ##   if RVT2 = 5 then VT2_5 = 1; else VT2_5 = 0;
  
  X$vt2_1 <- as.numeric(X$rvt2 == 1L) 
  X$vt2_2 <- as.numeric(X$rvt2 == 2L) 
  X$vt2_3 <- as.numeric(X$rvt2 == 3L) 
  X$vt2_4 <- as.numeric(X$rvt2 == 4L) 
  X$vt2_5 <- as.numeric(X$rvt2 == 5L) 

  ## SF2_1 = .;
  ##   if SF2 = . then SF2_1 = .; else
  ##   if SF2 = 1 then SF2_1 = 1; else SF2_1 = 0;
  ## SF2_2 = .;
  ##   if SF2 = . then SF2_2 = .; else
  ##   if SF2 = 2 then SF2_2 = 1; else SF2_2 = 0;
  ## SF2_3 = .;
  ##   if SF2 = . then SF2_3 = .; else
  ##   if SF2 = 3 then SF2_3 = 1; else SF2_3 = 0;
  ## SF2_4 = .;
  ##   if SF2 = . then SF2_4 = .; else
  ##   if SF2 = 4 then SF2_4 = 1; else SF2_4 = 0;

  X$sf2_1 <- as.numeric(X$sf2 == 1L) 
  X$sf2_2 <- as.numeric(X$sf2 == 2L) 
  X$sf2_3 <- as.numeric(X$sf2 == 3L) 
  X$sf2_4 <- as.numeric(X$sf2 == 4L) 

  ## RE2_1 = .;
  ##   if RE2 = . then RE2_1 = .; else
  ##   if RE2 = 1 then RE2_1 = 1; else RE2_1 = 0;

  X$re2_1 <- as.numeric(X$re2 == 1L) 

  ## RE3_1 = .;
  ##   if RE3 = . then RE3_1 = .; else
  ##   if RE3 = 1 then RE3_1 = 1; else RE3_1 = 0;

  X$re3_1 <- as.numeric(X$re3 == 1L) 

  ## MH3_1 = .;
  ##   if RMH3 = . then MH3_1 = .; else
  ##   if RMH3 = 1 then MH3_1 = 1; else MH3_1 = 0;
  ## MH3_2 = .;
  ##   if RMH3 = . then MH3_2 = .; else
  ##   if RMH3 = 2 then MH3_2 = 1; else MH3_2 = 0;
  ## MH3_3 = .;
  ##   if RMH3 = . then MH3_3 = .; else
  ##   if RMH3 = 3 then MH3_3 = 1; else MH3_3 = 0;
  ## MH3_4 = .;
  ##   if RMH3 = . then MH3_4 = .; else
  ##   if RMH3 = 4 then MH3_4 = 1; else MH3_4 = 0;
  ## MH3_5 = .;
  ##   if RMH3 = . then MH3_5 = .; else
  ##   if RMH3 = 5 then MH3_5 = 1; else MH3_5 = 0;

  X$mh3_1 <- as.numeric(X$rmh3 == 1L) 
  X$mh3_2 <- as.numeric(X$rmh3 == 2L) 
  X$mh3_3 <- as.numeric(X$rmh3 == 3L) 
  X$mh3_4 <- as.numeric(X$rmh3 == 4L) 
  X$mh3_5 <- as.numeric(X$rmh3 == 5L) 

  ## MH4_1 = .;
  ##   if MH4 = . then MH4_1 = .; else
  ##   if MH4 = 1 then MH4_1 = 1; else MH4_1 = 0;
  ## MH4_2 = .;
  ##   if MH4 = . then MH4_2 = .; else
  ##   if MH4 = 2 then MH4_2 = 1; else MH4_2 = 0;
  ## MH4_3 = .;
  ##   if MH4 = . then MH4_3 = .; else
  ##   if MH4 = 3 then MH4_3 = 1; else MH4_3 = 0;
  ## MH4_4 = .;
  ##   if MH4 = . then MH4_4 = .; else
  ##   if MH4 = 4 then MH4_4 = 1; else MH4_4 = 0;
  ## MH4_5 = .;
  ##   if MH4 = . then MH4_5 = .; else
  ##   if MH4 = 5 then MH4_5 = 1; else MH4_5 = 0;

  X$mh4_1 <- as.numeric(X$mh4 == 1L) 
  X$mh4_2 <- as.numeric(X$mh4 == 2L) 
  X$mh4_3 <- as.numeric(X$mh4 == 3L) 
  X$mh4_4 <- as.numeric(X$mh4 == 4L) 
  X$mh4_5 <- as.numeric(X$mh4 == 5L) 
  

  ## *****************************************************************;
  ## *               STEP 3: WEIGHTING AND AGGREGATION OF            *
  ## *                       INDICATOR VARIABLES USING               *
  ## *                       PHYSICAL AND MENTAL REGRESSION WEIGHTS  *
  ## *****************************************************************;
  ## RAWPCS12 = (-7.23216*PF02_1) + (-3.45555*PF02_2) +
  ##   (-6.24397*PF04_1) + (-2.73557*PF04_2) + (-4.61617*RP2_1) +
  ##   (-5.51747*RP3_1) + (-11.25544*BP2_1) + (-8.38063*BP2_2) +
  ##   (-6.50522*BP2_3) + (-3.80130*BP2_4) + (-8.37399*GH1_1) +
  ##   (-5.56461*GH1_2) + (-3.02396*GH1_3) + (-1.31872*GH1_4) +
  ##   (-2.44706*VT2_1) + (-2.02168*VT2_2) + (-1.6185*VT2_3) +
  ##   (-1.14387*VT2_4) + (-0.42251*VT2_5) + (-0.33682*SF2_1) +
  ##   (-0.94342*SF2_2) + (-0.18043*SF2_3) + (0.11038*SF2_4) +
  ##   (3.04365*RE2_1) + (2.32091*RE3_1) + (3.46638*MH3_1) +
  ##   (2.90426*MH3_2) + (2.37241*MH3_3) + (1.36689*MH3_4) +
  ##   (0.66514*MH3_5) + (4.61446*MH4_1) + (3.41593*MH4_2) +
  ##   (2.34247*MH4_3) + (1.28044*MH4_4) + (0.41188*MH4_5);

  ## RAWMCS12 = (3.93115*PF02_1) + (1.8684*PF02_2) +
  ##   (2.68282*PF04_1) + (1.43103*PF04_2) + (1.4406*RP2_1) +
  ##   (1.66968*RP3_1) + (1.48619*BP2_1) + (1.76691*BP2_2) +
  ##   (1.49384*BP2_3) + (0.90384*BP2_4) + (-1.71175*GH1_1) +
  ##   (-0.16891*GH1_2) + (0.03482*GH1_3) + (-0.06064*GH1_4) +
  ##   (-6.02409*VT2_1) + (-4.88962*VT2_2) + (-3.29805*VT2_3) +
  ##   (-1.65178*VT2_4) + (-0.92057*VT2_5) + (-6.29724*SF2_1) +
  ##   (-8.26066*SF2_2) + (-5.63286*SF2_3) + (-3.13896*SF2_4) +
  ##   (-6.82672*RE2_1) + (-5.69921*RE3_1) + (-10.19085*MH3_1) +
  ##   (-7.92717*MH3_2) + (-6.31121*MH3_3) + (-4.09842*MH3_4) +
  ##   (-1.94949*MH3_5) + (-16.15395*MH4_1) + (-10.77911*MH4_2) +
  ##   (-8.09914*MH4_3) + (-4.59055*MH4_4) + (-1.95934*MH4_5);

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


  ## *****************************************************************;
  ## *               STEP 5: NORM-BASED STANDARDIZATION OF           *
  ## *                       SCALE SCORES                            *
  ## *****************************************************************;

  ## PCS12 = RAWPCS12 + 56.57706;
  ## MCS12 = RAWMCS12 + 60.75781;

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
