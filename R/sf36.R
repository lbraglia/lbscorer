#' SF-36 questionnaire scoring
#'
#' SF-36 questionnaire scoring
#' @param X a \code{\link{matrix}} or \code{\link{data.frame}} of 36
#' columns, containing questionnaire items. In order from left to right:
#' GH1, HT, PF01, PF02, PF03, PF04, PF05, PF06, PF07, PF08, PF09, PF10,
#' RP1, RP2, RP3, RP4, RE1, RE2, RE3, SF1, BP1, BP2, VT1, MH1, MH2, MH3,
#' VT2, MH4, VT3, MH5, VT4, SF2, GH2, GH3, GH4, GH5.
#' @note
#' This is an R port of SAS algorithm by Apolone and Mosconi found
#' \href{http://crc.marionegri.it/qdv/index.php?page=sf36}{here}.
#' 
#' SF-36  is a registered trademark of medical outcomes trust.
#' @examples
#' ## -------------------------
#' ## Algorithm test/validation
#' ## -------------------------
#' (scores <- sf36(sf36sample))
#' lapply(scores, comment)
#' @export 
sf36 <- function( X = NULL ) {

  ## **************************
  ## *** STEP 1: INPUT DATA ***
  ## **************************

  if((!(is.data.frame(X) | is.matrix(X))) | (ncol(X)!=36) )
    stop("X must be a data.frame (or matrix) with 36 columns")

  X <- as.data.frame(lapply(as.data.frame(X), as.integer))
  names(X) <- 
    c("GH1", "HT", "PF01", "PF02", "PF03", "PF04", "PF05", "PF06", "PF07",
      "PF08", "PF09", "PF10", "RP1", "RP2", "RP3", "RP4", "RE1", "RE2", "RE3",
      "SF1", "BP1", "BP2", "VT1", "MH1", "MH2", "MH3", "VT2", "MH4", "VT3",
      "MH5", "VT4", "SF2", "GH2", "GH3", "GH4", "GH5")
  
  ## ****************************************
  ## *** STEP 2: SF-36 SCALE CONSTRUCTION ***
  ## ****************************************

  ## Change out-of-range values to missing for each item.  Recode and
  ## recalibrate items as needed. An 'R' prefix means the variable is
  ## recoded

  ## *******************************************************************
  ## *  THE SF-36 PHYSICAL FUNCTIONING INDEX.                          *
  ## *  All items are positively scored -- the higher the item         *
  ## *  value, the better the physical health.                         *
  ## *                                                                 *
  ## *  This scale is positively scored.                               *
  ## *  The higher the score the better the physical functioning.      *
  ## *******************************************************************

  ## ARRAY PFI(10) PF01-PF10;
  pfi <- c("PF01", "PF02", "PF03", "PF04", "PF05", "PF06", "PF07",
           "PF08", "PF09", "PF10") 

  ## DO I = 1 TO 10;
  ##    IF PFI(I) < 1 OR PFI(I) > 3 THEN PFI(I) = .;
  ## END;
  ## for outRangeNA look at utils.R
  X[, pfi] <- lapply(X[, pfi], outRangeNA, Max = 3L)

  ## PFNUM = N(OF PF01-PF10);
  ## (Count non missing values per row)
  X$PFNUM <- apply(X[, pfi], 1, function(x) sum(!is.na(x)))
  
  ## PFMEAN = MEAN(OF PF01-PF10);
  ## (mean per row not considering NA)
  X$PFMEAN <- apply(X[, pfi], 1, mean, na.rm = TRUE)

  ## DO I = 1 TO 10;
  ##   IF PFI(I)= . THEN PFI(I) = PFMEAN;
  ## END;
  X[, pfi] <- lapply(X[, pfi],
                     function(x, y) ifelse(!is.na(x), x, y),
                     y = X$PFMEAN)

  ## IF PFNUM GE 5 THEN RAWPF = SUM(OF PF01-PF10);
  X$RAWPF <- ifelse(X$PFNUM >= 5,
                    apply(X[,pfi], 1, sum, na.rm = TRUE),
                    NA)
  ## PF = ((RAWPF - 10)/(30-10)) * 100;
  X$PF <- with(X, ((RAWPF - 10)/(30-10)) * 100)
  ## LABEL  PF = 'SF-36 PHYSICAL FUNCTIONING (0-100)'
  ##        RAWPF = 'RAW SF-36 PHYSICAL FUNCTIONING';
  comment(X$PF) <- "SF-36 PHYSICAL FUNCTIONING (0-100)" 
  
  ## *******************************************************************
  ## *  THE SF-36 ROLE-PHYSICAL INDEX.                                 *
  ## *  All items are positively scored -- the higher the item value,  *
  ## *  the better the role-physical functioning.                      *
  ## *                                                                 *
  ## *  This scale is positively scored.                               *
  ## *  The higher the score the better the role-physical.             *
  ## *******************************************************************

  ## ARRAY RPA(4) RP1-RP4;
  rpa <- paste0("RP", 1:4)

  ## DO I = 1 TO 4;
  ##    IF RPA(I) < 1 OR RPA(I) > 2 THEN RPA(I) = .;
  ## END;
  X[, rpa] <- lapply(X[, rpa], outRangeNA, Max = 2L)

  ## ROLPNUM = N(OF RP1-RP4);
  X$ROLPNUM <- apply(X[, rpa], 1, function(x) sum(!is.na(x)))
  ## ROLPMEAN = MEAN(OF RP1-RP4);
  X$ROLPMEAN <- apply(X[, rpa], 1, mean, na.rm = TRUE)

  ## DO I = 1 TO 4;
  ##    IF RPA(I) = . THEN RPA(I) = ROLPMEAN;
  ## END;
  X[, rpa] <- lapply(X[, rpa],
                     function(x, y) ifelse(!is.na(x), x, y),
                     y = X$ROLPMEAN)

  ## IF ROLPNUM GE 2 THEN RAWRP = SUM(OF RP1-RP4);
  X$RAWRP <- ifelse(X$ROLPNUM >= 2,
                    apply(X[,rpa], 1, sum, na.rm = TRUE),
                    NA)
  ## RP = ((RAWRP - 4)/(8-4)) * 100;
  X$RP <- with(X, ((RAWRP - 4)/(8-4)) * 100)
  ## LABEL  RP = 'SF-36 ROLE-PHYSICAL (0-100)'
  ##        RAWRP = 'RAW SF-36 ROLE-PHYSICAL';
  comment(X$RP) <- "SF-36 ROLE-PHYSICAL (0-100)"
  
  ## *******************************************************************
  ## *  THE SF-36 PAIN ITEMS.                                          *
  ## *  Item recoding depends on whether both pain1 and pain2          *
  ## *  are answered or whether one of the items has missing data.     *
  ## *  After recoding, all items are positively scored -- the higher  *
  ## *  the score, the less pain (or the more freedom from pain).      *
  ## *                                                                 *
  ## *  This scale is positively scored.  The higher the               *
  ## *  score the less pain or the more freedom from pain.             *
  ## *******************************************************************

  ## IF BP1 < 1 OR BP1 > 6 THEN BP1 = .;
  ## IF BP2 < 1 OR BP2 > 5 THEN BP2 = .;
  X$BP1 <- outRangeNA(X$BP1, Max = 6L)
  X$BP2 <- outRangeNA(X$BP2, Max = 5L)
  
  ## IF BP1 NE . AND BP2 NE . THEN DO;
  ##    IF BP1 = 1 THEN RBP1 = 6;
  ##    IF BP1 = 2 THEN RBP1 = 5.4;
  ##    IF BP1 = 3 THEN RBP1 = 4.2;
  ##    IF BP1 = 4 THEN RBP1 = 3.1;
  ##    IF BP1 = 5 THEN RBP1 = 2.2;
  ##    IF BP1 = 6 THEN RBP1 = 1;
  ##    IF BP2 = 1  AND BP1 = 1 THEN RBP2 = 6;
  ##    IF BP2 = 1  AND 2 LE BP1 LE 6 THEN RBP2 = 5;
  ##    IF BP2 = 2  AND 1 LE BP1 LE 6 THEN RBP2  = 4;
  ##    IF BP2 = 3  AND 1 LE BP1 LE 6 THEN RBP2  = 3;
  ##    IF BP2 = 4  AND 1 LE BP1 LE 6 THEN RBP2  = 2;
  ##    IF BP2 = 5  AND 1 LE BP1 LE 6 THEN RBP2  = 1;
  ## END;
  ## IF BP1 NE . AND BP2 = . THEN DO;
  ##    IF BP1 = 1 THEN RBP1 = 6;
  ##    IF BP1 = 2 THEN RBP1 = 5.4;
  ##    IF BP1 = 3 THEN RBP1 = 4.2;
  ##    IF BP1 = 4 THEN RBP1 = 3.1;
  ##    IF BP1 = 5 THEN RBP1 = 2.2;
  ##    IF BP1 = 6 THEN RBP1 = 1;
  ##    RBP2 = RBP1;
  ## END;
  ## IF BP1 = . AND BP2 NE . THEN DO;
  ##    IF BP2 = 1 THEN RBP2 = 6;
  ##    IF BP2 = 2 THEN RBP2 = 4.75;
  ##    IF BP2 = 3 THEN RBP2 = 3.5;
  ##    IF BP2 = 4 THEN RBP2 = 2.25;
  ##    IF BP2 = 5 THEN RBP2 = 1;
  ##    RBP1 = RBP2;
  ## END;

  ## for sf36recBP look at utils.R
  X <- cbind(X, sf36recBP(bp1 = X$BP1, bp2 = X$BP2))
  
  ## BPNUM = N(BP1,BP2);
  X$BPNUM <- with(X, (!is.na(BP1)) + (!is.na(BP2)))
    
  ## IF BPNUM GE 1 THEN RAWBP = SUM(RBP1,RBP2);
  X$RAWBP <- with(X, ifelse(BPNUM >= 1, RBP1 + RBP2 , NA))

  ## BP = ((RAWBP - 2)/(12-2)) * 100;
  X$BP <- with(X, ((RAWBP - 2)/(12-2)) * 100)

  ## LABEL  BP = 'SF-36 PAIN INDEX (0-100)'
  ##        RAWBP = 'RAW SF-36 PAIN INDEX';
  comment(X$BP) <- "SF-36 PAIN INDEX (0-100)"

  ## *******************************************************************
  ## *  THE SF-36 GENERAL HEALTH PERCEPTIONS INDEX.                    *
  ## *  Reverse two items and recalibrate one item.  After recoding    *
  ## *  and recalibration, all items are positively scored --  the     *
  ## *  higher the score, the better the perceived general health.     *
  ## *                                                                 *
  ## *  This scale is positively scored.                               *
  ## *  The higher the score the better the health perceptions.        *
  ## *******************************************************************

  ## ARRAY GHP(5) GH1-GH5;
  ghp <- paste0("GH", 1:5)
  
  ## DO I= 1 TO 5;
  ##   IF GHP(I) < 1 OR GHP(I) > 5 THEN GHP(I) = .;
  ## END;
  X[, ghp] <- lapply(X[, ghp], outRangeNA, Max = 5L)

  ## IF GH1 = 1 THEN RGH1 =   5;
  ## IF GH1 = 2 THEN RGH1 = 4.4;
  ## IF GH1 = 3 THEN RGH1 = 3.4;
  ## IF GH1 = 4 THEN RGH1 =   2;
  ## IF GH1 = 5 THEN RGH1 =   1;
  X$RGH1 <- c(5, 4.4, 3.4, 2, 1)[X$GH1]
  
  ## RGH3 = 6 - GH3;
  X$RGH3 <- 6 - X$GH3
  
  ## RGH5 = 6 - GH5;
  X$RGH5 <- 6 - X$GH5

  ## GHNUM = N(GH1,GH2,GH3,GH4,GH5);
  X$GHNUM <- apply(X[, ghp], 1, function(x) sum(!is.na(x)))

  ## GHMEAN = MEAN(RGH1,GH2,RGH3,GH4,RGH5);
  rgh <- c("RGH1", "GH2", "RGH3", "GH4", "RGH5")
  X$GHMEAN <- apply(X[, rgh], 1, mean, na.rm = TRUE)

  ## ARRAY RGH(5) RGH1 GH2 RGH3 GH4 RGH5;
  ## DO I= 1 TO 5;
  ##   IF RGH(I) = . THEN RGH(I) = GHMEAN;
  ## END;
  X[, rgh] <- lapply(X[, rgh],
                     function(x, y) ifelse(!is.na(x), x, y),
                     y = X$GHMEAN)

  ## IF GHNUM GE 3  THEN RAWGH = SUM(RGH1,GH2,RGH3,GH4,RGH5);
  X$RAWGH <- ifelse(X$GHNUM >= 3,
                    apply(X[, rgh], 1, sum, na.rm = TRUE),
                    NA)

  ## GH = ((RAWGH - 5)/(25-5)) * 100;
  X$GH <- with(X, ((RAWGH - 5)/(25-5)) * 100)
  
  ## LABEL  GH = 'SF-36 GENERAL HEALTH PERCEPTIONS (0-100)'
  ##        RAWGH = 'RAW SF-36 GENERAL HEALTH PERCEPTIONS';
  comment(X$GH) <- "SF-36 GENERAL HEALTH PERCEPTIONS (0-100)"

  ## *******************************************************************
  ## *  THE SF-36 VITALITY ITEMS.                                      *
  ## *  Reverse two items.  After item reversal, all items are         *
  ## *  positively scored -- the higher the score, the less the        *
  ## *  fatigue and the greater the energy.                            *
  ## *                                                                 *
  ## *  This scale is positively scored.                               *
  ## *  The higher the score the greater the vitality.                 *
  ## *******************************************************************

  ## ARRAY VI(4) VT1-VT4;
  vi <- paste0("VT", 1:4)
    
  ## DO I = 1 TO 4;
  ##   IF VI(I) < 1 OR VI(I) > 6 THEN VI(I) =.;
  ## END;
  X[, vi] <- lapply(X[, vi], outRangeNA, Max = 6L)

  ## RVT1 = 7-VT1;
  ## RVT2 = 7-VT2;
  X$RVT1 <- with(X, 7 - VT1)
  X$RVT2 <- with(X, 7 - VT2)
    
  ## VITNUM = N(VT1,VT2,VT3,VT4);
  X$VITNUM <- apply(X[, vi], 1, function(x) sum(!is.na(x)))

  ## VITMEAN = MEAN(RVT1,RVT2,VT3,VT4);
  rvi <- c("RVT1", "RVT2", "VT3", "VT4")
  X$VITMEAN <- apply(X[, rvi], 1, mean, na.rm = TRUE)
  
  ## ARRAY RVI(4) RVT1 RVT2 VT3 VT4;
  ## DO I = 1 TO 4;
  ##   IF RVI(I) = . THEN RVI(I) = VITMEAN;
  ## END;
  X[, rvi] <- lapply(X[, rvi],
                     function(x, y) ifelse(!is.na(x), x, y),
                     y = X$VITMEAN)
  
  ## IF VITNUM GE 2 THEN RAWVT= SUM(RVT1,RVT2,VT3,VT4);
  X$RAWVT <- ifelse(X$VITNUM >= 2,
                    apply(X[, rvi], 1, sum, na.rm = TRUE),
                    NA)

  ## VT = ((RAWVT-4)/(24-4)) * 100;
  X$VT <- with(X, ((RAWVT-4)/(24-4)) * 100)
    
  ## LABEL  VT = 'SF-36 VITALITY (0-100)'
  ##        RAWVT = 'RAW SF-36 VITALITY';
  comment(X$VT) <- 'SF-36 VITALITY (0-100)'

  ## ******************************************************************
  ## *  THE SF-36 SOCIAL FUNCTIONING INDEX.                           *
  ## *  Reverse one item so that both items are positively scored --  *
  ## *  the higher the score, the better the social functioning.      *
  ## *                                                                *
  ## *  This scale is positively scored.                              *
  ## *  The higher the score the better the social functioning.       *
  ## ******************************************************************

  ## ARRAY SOC(2) SF1-SF2;
  soc <- paste0("SF", 1:2)
  
  ## DO I = 1 TO 2;
  ##    IF SOC(I) < 1 OR SOC(I) > 5 THEN SOC(I) = .;
  ## END;
  X[, soc] <- lapply(X[, soc], outRangeNA, Max = 5L)

  ## RSF1 = 6 - SF1;
  X$RSF1 = with(X, 6 - SF1)
  ## SFNUM = N(SF1,SF2);
  X$SFNUM <- apply(X[, soc], 1, function(x) sum(!is.na(x)))
  ## SFMEAN = MEAN(RSF1,SF2);
  rsf <- c("RSF1","SF2")
  X$SFMEAN <- apply(X[, rsf], 1, mean, na.rm = TRUE)

  ## ARRAY RSF(2) RSF1 SF2;
  ## DO I = 1 TO 2;
  ##   IF RSF(I) = . THEN RSF(I) = SFMEAN;
  ## END;
  X[, rsf] <- lapply(X[, rsf],
                     function(x, y) ifelse(!is.na(x), x, y),
                     y = X$SFMEAN)

  ## IF SFNUM GE 1 THEN RAWSF = SUM(RSF1,SF2);
  X$RAWSF <- ifelse(X$SFNUM >= 1,
                    apply(X[, rsf], 1, sum, na.rm = TRUE),
                    NA)
  ## SF = ((RAWSF - 2)/(10-2)) * 100;
  X$SF <- with(X, ((RAWSF - 2)/(10-2)) * 100)
  
  ## LABEL  SF = 'SF-36 SOCIAL FUNCTIONING (0-100)'
  ##        RAWSF = 'RAW SF-36 SOCIAL FUNCTIONING';
  comment(X$SF) <- 'SF-36 SOCIAL FUNCTIONING (0-100)'

  ## ******************************************************************
  ## *  THE SF-36 ROLE-EMOTIONAL INDEX.                               *
  ## *  All items are positively scored -- the higher the item value, *
  ## *  the better the role-emotional functioning.                    *
  ## *                                                                *
  ## *  This scale is positively scored.                              *
  ## *  The higher the score, the better the role-emotional.          *
  ## ******************************************************************

  ## ARRAY RM(3) RE1-RE3;
  RM <- paste0("RE", 1:3)
    
  ## DO I = 1 TO 3;
  ##    IF RM(I) < 1 OR RM(I) > 2 THEN RM(I) = .;
  ## END;
  X[, RM] <- lapply(X[, RM], outRangeNA, Max = 2L)

  ## ROLMNUM = N(OF RE1-RE3);
  X$ROLMNUM <- apply(X[, RM], 1, function(x) sum(!is.na(x)))
  ## ROLMMEAN = MEAN(OF RE1-RE3);
  X$ROLMMEAN <- apply(X[, RM], 1, mean, na.rm = TRUE)

  ## DO I = 1 TO 3;
  ##    IF RM(I) = . THEN RM(I) = ROLMMEAN;
  ## END;
  X[, RM] <- lapply(X[, RM],
                     function(x, y) ifelse(!is.na(x), x, y),
                     y = X$ROLMMEAN)

  ## IF ROLMNUM GE 2 THEN RAWRE = SUM(OF RE1-RE3);
  X$RAWRE <- ifelse(X$ROLMNUM >= 2,
                    apply(X[,RM], 1, sum, na.rm = TRUE),
                    NA)
  ## RE = ((RAWRE - 3)/(6-3)) * 100;
  X$RE <- with(X, ((RAWRE - 3)/(6-3)) * 100)
  
  ## LABEL  RE = 'SF-36 ROLE-EMOTIONAL (0-100)'
  ##        RAWRE = 'RAW SF-36 ROLE-EMOTIONAL';
  comment(X$RE) <- 'SF-36 ROLE-EMOTIONAL (0-100)'

  ## ******************************************************************
  ## *  THE SF-36 MENTAL HEALTH INDEX.                                *
  ## *  Reverse two items.  After item reversal, all items are        *
  ## *  positively scored -- the higher the score, the better the     *
  ## *  mental health.                                                *
  ## *                                                                *
  ## *  This scale is positively scored.                              *
  ## *  The higher the score the better the mental health.            *
  ## ******************************************************************

  ## ARRAY MHI(5) MH1-MH5;
  mhi <- paste0("MH", 1:5)
  ## DO I = 1 TO 5;
  ##    IF MHI(I) < 1 OR MHI(I) > 6 THEN MHI(I)=.;
  ## END;
  X[, mhi] <- lapply(X[, mhi], outRangeNA, Max = 6L)

  ## RMH3 = 7-MH3;
  ## RMH5 = 7-MH5;
  X$RMH3 = with(X, 7 - MH3)
  X$RMH5 = with(X, 7 - MH5)
  
  ## MHNUM=N(MH1,MH2,MH3,MH4,MH5);
  X$MHNUM <- apply(X[, mhi], 1, function(x) sum(!is.na(x)))

  ## MHMEAN=MEAN(MH1,MH2,RMH3,MH4,RMH5);
  rmh <- c("MH1", "MH2", "RMH3", "MH4", "RMH5")
  X$MHMEAN <- apply(X[, rmh], 1, mean, na.rm = TRUE)

  ## ARRAY RMH(5) MH1 MH2 RMH3 MH4 RMH5;
  ## DO I = 1 TO 5;
  ##    IF RMH(I) = . THEN RMH(I) = MHMEAN;
  ## END;
  X[, rmh] <- lapply(X[, rmh],
                     function(x, y) ifelse(!is.na(x), x, y),
                     y = X$MHMEAN)

  ## IF MHNUM GE 3 THEN RAWMH = SUM(MH1,MH2,RMH3,MH4,RMH5);
  X$RAWMH <- ifelse(X$MHNUM >= 3,
                    apply(X[, rmh], 1, sum, na.rm = TRUE),
                    NA)
  
  ## MH = ((RAWMH-5)/(30-5)) * 100;
  X$MH <- with(X, ((RAWMH-5)/(30-5)) * 100)
  
  ## LABEL  MH = 'SF-36 MENTAL HEALTH INDEX (0-100)'
  ##        RAWMH = 'RAW SF-36 MENTAL HEALTH INDEX';
  comment(X$MH) <- 'SF-36 MENTAL HEALTH INDEX (0-100)'

  ## ******************************************************************
  ## *  THE SF-36 HEALTH TRANSITION ITEM.                             *
  ## *  This item should be analyzed as categorical data.             *
  ## ******************************************************************

  ## IF HT < 1 OR HT > 5 THEN HT = .;
  X$HT <- outRangeNA(X$HT, Max = 5L)
  
  ## LABEL  HT='RAW SF-36 HEALTH TRANSITION ITEM';
  comment(X$HT) <- 'RAW SF-36 HEALTH TRANSITION ITEM'
  
  ## ******************************************************************
  ## ***               STEP 3: SF-36 INDEX CONSTRUCTION             ***
  ## ******************************************************************

  ## ******************************************************************
  ## *  PURPOSE: create physical and mental health index scores       *
  ## *           standardized but not normalized                      *
  ## *           and standard deviations calculated with vardef=wdf   *
  ## ******************************************************************

  ## ******************************************************************
  ## COMPUTE Z SCORES -- OBSERVED VALUES ARE SAMPLE DATA
  ##
  ## MEAN AND SD IS U.S GENERAL POPULATION
  ## FACTOR ANALYTIC SAMPLE
  ## N=2393: HAVE ALL EIGHT SCALES                             
  ## ******************************************************************

  ## PF_Z=(PF-84.52404)/22.89490;
  ## RP_Z=(RP-81.19907)/33.79729;
  ## BP_Z=(BP-75.49196)/23.55879;
  ## GH_Z=(GH-72.21316)/20.16964;
  ## VT_Z=(VT-61.05453)/20.86942;
  ## SF_Z=(SF-83.59753)/22.37642;
  ## RE_Z=(RE-81.29467)/33.02717;
  ## MH_Z=(MH-74.84212)/18.01189;

  X$PF_Z <- with(X, (PF-84.52404)/22.89490 )
  X$RP_Z <- with(X, (RP-81.19907)/33.79729 )
  X$BP_Z <- with(X, (BP-75.49196)/23.55879 )
  X$GH_Z <- with(X, (GH-72.21316)/20.16964 )
  X$VT_Z <- with(X, (VT-61.05453)/20.86942 )
  X$SF_Z <- with(X, (SF-83.59753)/22.37642 )
  X$RE_Z <- with(X, (RE-81.29467)/33.02717 )
  X$MH_Z <- with(X, (MH-74.84212)/18.01189 )
   
  ## ******************************************************************
  ## COMPUTE SAMPLE RAW FACTOR SCORES                           
  ## Z SCORES ARE FROM ABOVE                                    
  ## SCORING COEFFICIENTS ARE FROM U.S. GENERAL POPULATION      
  ## FACTOR ANALYTIC SAMPLE N=2393: HAVE ALL EIGHT SCALES       
  ## ******************************************************************

  ## praw=(PF_Z * .42402)+(RP_Z * .35119)+(BP_Z * .31754)+
  ##      (SF_Z * -.00753)+(MH_Z * -.22069)+(RE_Z * -.19206)+
  ##      (VT_Z * .02877)+(GH_Z * .24954);

  ## mraw=(PF_Z * -.22999)+(RP_Z * -.12329)+(BP_Z * -.09731)+
  ##      (SF_Z * .26876)+(MH_Z * .48581)+(RE_Z * .43407)+
  ##      (VT_Z * .23534)+(GH_Z * -.01571);

  X$praw <- with(X,
                 (PF_Z * .42402)+(RP_Z * .35119)+(BP_Z * .31754) + 
                 (SF_Z * -.00753)+(MH_Z * -.22069)+(RE_Z * -.19206) +
                 (VT_Z * .02877)+(GH_Z * .24954))

  X$mraw <- with(X,
                 (PF_Z * -.22999)+(RP_Z * -.12329)+(BP_Z * -.09731)+
                 (SF_Z * .26876)+(MH_Z * .48581)+(RE_Z * .43407)+
                 (VT_Z * .23534)+(GH_Z * -.01571))
  
  ## ****************************
  ## Compute standardized scores 
  ## ****************************

  X$PCS = (X$praw*10) + 50;
  X$MCS = (X$mraw*10) + 50;

  ## label PCS='STANDARDIZED PHYSICAL COMPONENT SCALE-00'
  ##       MCS='STANDARDIZED MENTAL COMPONENT SCALE-00';

  comment(X$PCS) <- 'STANDARDIZED PHYSICAL COMPONENT SCALE-00'
  comment(X$MCS) <- 'STANDARDIZED MENTAL COMPONENT SCALE-00'
  
  ## *****
  ## Exit 
  ## *****

  ## PROC PRINT;VAR PF RP BP GH VT SF RE MH PCS MCS;ENDSAS;
  vars.returned <- c("PF","RP","BP","GH","VT","SF","RE","MH","PCS","MCS")
  return(X[, names(X) %in% vars.returned, drop = FALSE])
}


#' SF-36 sample dataset
#'
#' SF-36 sample of 10 questionnaires for \code{\link{sf36}} testing purposes.
#'
#' @format A data frame with 10 rows and 36 columns (1 per
#' SF-36 items)
#' @source SF-36 italian SAS algorithm available at
#' \url{http://crc.marionegri.it/qdv/index.php?page=sf36}
#' 
"sf36sample"
