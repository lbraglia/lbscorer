#' SF-36 questionnaire scoring
#'
#' SF-36 questionnaire scoring
#' @param X a \code{\link{matrix}} or \code{\link{data.frame}} of 36
#' columns, containing questionnaire items. In order from left to right:
#' gh1, ht, pf01, pf02, pf03, pf04, pf05, pf06, pf07, pf08, pf09, pf10
#' @note
#' This is an R port of SAS algorithm by Apolone and Mosconi found
#' \href{http://crc.marionegri.it/qdv/index.php?page=sf36}{here}.
#' 
#' SF-36  is a registered trademark of medical outcomes trust.
#' @examples
#' \dontrun{
#' ## -------------------------
#' ## Algorithm test/validation
#' ## -------------------------
#' }
#' scores <- sf36(sf36sample)
#' @export 
sf36 <- function( X = NULL ) {

  if((!(is.data.frame(X) | is.matrix(X))) | (ncol(X)!=36) )
    stop("X must be a data.frame (or matrix) with 36 columns")

  ## ********************************************************************
  ## ***               STEP 1: INPUT DATA                             ***
  ## ********************************************************************;

  X <- as.data.frame(lapply(as.data.frame(X), as.integer))
  names(X) <- c("GH1", "HT", "PF01", "PF02", "PF03", "PF04", "PF05",
                "PF06", "PF07", "PF08", "PF09", "PF10", "RP1", "RP2",
                "RP3", "RP4", "RE1", "RE2", "RE3", "SF1", "BP1", "BP2",
                "VT1", "MH1", "MH2", "MH3", "VT2", "MH4", "VT3", "MH5",
                "VT4", "SF2", "GH2", "GH3", "GH4", "GH5") 

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
  
  
}


## *******************************************************************
## ***               STEP 2: SF-36 SCALE CONSTRUCTION              ***
## *******************************************************************;

## *******************************************************************
## *  Using the SAS dataset created in part 1, change out-of-range   *
## *  values to missing for each item.  Recode and recalibrate items *
## *  as needed. An 'R' prefix means the variable is recoded         *
## *******************************************************************;

## DATA SF36SCAL;
## SET SF36DATA;

## *******************************************************************
## *  THE SF-36 PHYSICAL FUNCTIONING INDEX.                          *
## *  All items are positively scored -- the higher the item         *
## *  value, the better the physical health.                         *
## *                                                                 *
## *  This scale is positively scored.                               *
## *  The higher the score the better the physical functioning.      *
## *******************************************************************;

## ARRAY PFI(10) PF01-PF10;

## DO I = 1 TO 10;
##    IF PFI(I) < 1 OR PFI(I) > 3 THEN PFI(I) = .;
## END;

## PFNUM = N(OF PF01-PF10);
## PFMEAN = MEAN(OF PF01-PF10);

## DO I = 1 TO 10;
##   IF PFI(I)= . THEN PFI(I) = PFMEAN;
## END;

## IF PFNUM GE 5 THEN RAWPF = SUM(OF PF01-PF10);
## PF = ((RAWPF - 10)/(30-10)) * 100;

## LABEL  PF = 'SF-36 PHYSICAL FUNCTIONING (0-100)'
##        RAWPF = 'RAW SF-36 PHYSICAL FUNCTIONING';

## *******************************************************************
## *  THE SF-36 ROLE-PHYSICAL INDEX.                                 *
## *  All items are positively scored -- the higher the item value,  *
## *  the better the role-physical functioning.                      *
## *                                                                 *
## *  This scale is positively scored.                               *
## *  The higher the score the better the role-physical.             *
## *******************************************************************;

## ARRAY RPA(4) RP1-RP4;

## DO I = 1 TO 4;
##    IF RPA(I) < 1 OR RPA(I) > 2 THEN RPA(I) = .;
## END;

## ROLPNUM = N(OF RP1-RP4);
## ROLPMEAN = MEAN(OF RP1-RP4);

## DO I = 1 TO 4;
##    IF RPA(I) = . THEN RPA(I) = ROLPMEAN;
## END;

## IF ROLPNUM GE 2 THEN RAWRP = SUM(OF RP1-RP4);
## RP = ((RAWRP - 4)/(8-4)) * 100;
## LABEL  RP = 'SF-36 ROLE-PHYSICAL (0-100)'
##        RAWRP = 'RAW SF-36 ROLE-PHYSICAL';

## *******************************************************************
## *  THE SF-36 PAIN ITEMS.                                          *
## *  Item recoding depends on whether both pain1 and pain2          *
## *  are answered or whether one of the items has missing data.     *
## *  After recoding, all items are positively scored -- the higher  *
## *  the score, the less pain (or the more freedom from pain).      *
## *                                                                 *
## *  This scale is positively scored.  The higher the               *
## *  score the less pain or the more freedom from pain.             *
## *******************************************************************;

## IF BP1 < 1 OR BP1 > 6 THEN BP1 = .;
## IF BP2 < 1 OR BP2 > 5 THEN BP2 = .;

## *  RECODES IF NEITHER BP1 OR BP2 HAS A MISSING VALUE;

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

## *  RECODES IF BP1 IS NOT MISSING AND BP2 IS MISSING;

## IF BP1 NE . AND BP2 = . THEN DO;
##    IF BP1 = 1 THEN RBP1 = 6;
##    IF BP1 = 2 THEN RBP1 = 5.4;
##    IF BP1 = 3 THEN RBP1 = 4.2;
##    IF BP1 = 4 THEN RBP1 = 3.1;
##    IF BP1 = 5 THEN RBP1 = 2.2;
##    IF BP1 = 6 THEN RBP1 = 1;
##    RBP2 = RBP1;

## END;

## *  RECODES IF BP1 IS MISSING AND BP2 IS NOT MISSING;

## IF BP1 = . AND BP2 NE . THEN DO;

##    IF BP2 = 1 THEN RBP2 = 6;
##    IF BP2 = 2 THEN RBP2 = 4.75;
##    IF BP2 = 3 THEN RBP2 = 3.5;
##    IF BP2 = 4 THEN RBP2 = 2.25;
##    IF BP2 = 5 THEN RBP2 = 1;
##    RBP1 = RBP2;

## END;

## BPNUM = N(BP1,BP2);

## IF BPNUM GE 1 THEN RAWBP = SUM(RBP1,RBP2);
## BP = ((RAWBP - 2)/(12-2)) * 100;

## LABEL  BP = 'SF-36 PAIN INDEX (0-100)'
##        RAWBP = 'RAW SF-36 PAIN INDEX';


## *******************************************************************
## *  THE SF-36 GENERAL HEALTH PERCEPTIONS INDEX.                    *
## *  Reverse two items and recalibrate one item.  After recoding    *
## *  and recalibration, all items are positively scored --  the     *
## *  higher the score, the better the perceived general health.     *
## *                                                                 *
## *  This scale is positively scored.                               *
## *  The higher the score the better the health perceptions.        *
## *******************************************************************;

## ARRAY GHP(5) GH1-GH5;

## DO I= 1 TO 5;
##   IF GHP(I) < 1 OR GHP(I) > 5 THEN GHP(I) = .;
## END;

## IF GH1 = 1 THEN RGH1 =   5;
## IF GH1 = 2 THEN RGH1 = 4.4;
## IF GH1 = 3 THEN RGH1 = 3.4;
## IF GH1 = 4 THEN RGH1 =   2;
## IF GH1 = 5 THEN RGH1 =   1;

## RGH3 = 6 - GH3;
## RGH5 = 6 - GH5;

## GHNUM = N(GH1,GH2,GH3,GH4,GH5);
## GHMEAN = MEAN(RGH1,GH2,RGH3,GH4,RGH5);

## ARRAY RGH(5) RGH1 GH2 RGH3 GH4 RGH5;

## DO I= 1 TO 5;
##   IF RGH(I) = . THEN RGH(I) = GHMEAN;
## END;

## IF GHNUM GE 3  THEN RAWGH = SUM(RGH1,GH2,RGH3,GH4,RGH5);
## GH = ((RAWGH - 5)/(25-5)) * 100;

## LABEL  GH = 'SF-36 GENERAL HEALTH PERCEPTIONS (0-100)'
##        RAWGH = 'RAW SF-36 GENERAL HEALTH PERCEPTIONS';


## *******************************************************************
## *  THE SF-36 VITALITY ITEMS.                                      *
## *  Reverse two items.  After item reversal, all items are         *
## *  positively scored -- the higher the score, the less the        *
## *  fatigue and the greater the energy.                            *
## *                                                                 *
## *  This scale is positively scored.                               *
## *  The higher the score the greater the vitality.                 *
## *******************************************************************;

## ARRAY VI(4) VT1-VT4;

## DO I = 1 TO 4;
##   IF VI(I) < 1 OR VI(I) > 6 THEN VI(I) =.;
## END;

## RVT1 = 7-VT1;
## RVT2 = 7-VT2;

## VITNUM = N(VT1,VT2,VT3,VT4);
## VITMEAN = MEAN(RVT1,RVT2,VT3,VT4);

## ARRAY RVI(4) RVT1 RVT2 VT3 VT4;

## DO I = 1 TO 4;
##   IF RVI(I) = . THEN RVI(I) = VITMEAN;
## END;

## IF VITNUM GE 2 THEN RAWVT= SUM(RVT1,RVT2,VT3,VT4);
## VT = ((RAWVT-4)/(24-4)) * 100;

## LABEL  VT = 'SF-36 VITALITY (0-100)'
##        RAWVT = 'RAW SF-36 VITALITY';


## ******************************************************************
## *  THE SF-36 SOCIAL FUNCTIONING INDEX.                           *
## *  Reverse one item so that both items are positively scored --  *
## *  the higher the score, the better the social functioning.      *
## *                                                                *
## *  This scale is positively scored.                              *
## *  The higher the score the better the social functioning.       *
## ******************************************************************;

## ARRAY SOC(2) SF1-SF2;

## DO I = 1 TO 2;
##    IF SOC(I) < 1 OR SOC(I) > 5 THEN SOC(I) = .;
## END;

## RSF1 = 6 - SF1;
## SFNUM = N(SF1,SF2);
## SFMEAN = MEAN(RSF1,SF2);

## ARRAY RSF(2) RSF1 SF2;

## DO I = 1 TO 2;
##   IF RSF(I) = . THEN RSF(I) = SFMEAN;
## END;

## IF SFNUM GE 1 THEN RAWSF = SUM(RSF1,SF2);
## SF = ((RAWSF - 2)/(10-2)) * 100;

## LABEL  SF = 'SF-36 SOCIAL FUNCTIONING (0-100)'
##        RAWSF = 'RAW SF-36 SOCIAL FUNCTIONING';


## ******************************************************************
## *  THE SF-36 ROLE-EMOTIONAL INDEX.                               *
## *  All items are positively scored -- the higher the item value, *
## *  the better the role-emotional functioning.                    *
## *                                                                *
## *  This scale is positively scored.                              *
## *  The higher the score, the better the role-emotional.          *
## ******************************************************************;

## ARRAY RM(3) RE1-RE3;

## DO I = 1 TO 3;
##    IF RM(I) < 1 OR RM(I) > 2 THEN RM(I) = .;
## END;

## ROLMNUM = N(OF RE1-RE3);
## ROLMMEAN = MEAN(OF RE1-RE3);

## DO I = 1 TO 3;
##    IF RM(I) = . THEN RM(I) = ROLMMEAN;
## END;

## IF ROLMNUM GE 2 THEN RAWRE = SUM(OF RE1-RE3);
## RE = ((RAWRE - 3)/(6-3)) * 100;

## LABEL  RE = 'SF-36 ROLE-EMOTIONAL (0-100)'
##        RAWRE = 'RAW SF-36 ROLE-EMOTIONAL';


## ******************************************************************
## *  THE SF-36 MENTAL HEALTH INDEX.                                *
## *  Reverse two items.  After item reversal, all items are        *
## *  positively scored -- the higher the score, the better the     *
## *  mental health.                                                *
## *                                                                *
## *  This scale is positively scored.                              *
## *  The higher the score the better the mental health.            *
## ******************************************************************;

## ARRAY MHI(5) MH1-MH5;

## DO I = 1 TO 5;
##    IF MHI(I) < 1 OR MHI(I) > 6 THEN MHI(I)=.;
## END;

## RMH3 = 7-MH3;
## RMH5 = 7-MH5;

## MHNUM=N(MH1,MH2,MH3,MH4,MH5);
## MHMEAN=MEAN(MH1,MH2,RMH3,MH4,RMH5);

## ARRAY RMH(5) MH1 MH2 RMH3 MH4 RMH5;

## DO I = 1 TO 5;
##    IF RMH(I) = . THEN RMH(I) = MHMEAN;
## END;

## IF MHNUM GE 3 THEN RAWMH = SUM(MH1,MH2,RMH3,MH4,RMH5);
## MH = ((RAWMH-5)/(30-5)) * 100;

## LABEL  MH = 'SF-36 MENTAL HEALTH INDEX (0-100)'
##        RAWMH = 'RAW SF-36 MENTAL HEALTH INDEX';


## ******************************************************************
## *  THE SF-36 HEALTH TRANSITION ITEM.                             *
## *  This item should be analyzed as categorical data.             *
## ******************************************************************;

## IF HT < 1 OR HT > 5 THEN HT = .;

## LABEL  HT='RAW SF-36 HEALTH TRANSITION ITEM';

## ******************************************************************
## ***               STEP 3: SF-36 INDEX CONSTRUCTION             ***
## ******************************************************************;

## ******************************************************************
## *  PURPOSE: create physical and mental health index scores       *
## *           standardized but not normalized                      *
## *           and standard deviations calculated with vardef=wdf   *
## ******************************************************************;

## ******************************************************************
## *      COMPUTE Z SCORES -- OBSERVED VALUES ARE SAMPLE DATA       *
## *                                                                *
## *      MEAN AND SD IS U.S GENERAL POPULATION                     *
## *      FACTOR ANALYTIC SAMPLE                                    *
## *      N=2393: HAVE ALL EIGHT SCALES                             *
## ******************************************************************;

## PF_Z=(PF-84.52404)/22.89490;
## RP_Z=(RP-81.19907)/33.79729;
## BP_Z=(BP-75.49196)/23.55879;
## GH_Z=(GH-72.21316)/20.16964;
## VT_Z=(VT-61.05453)/20.86942;
## SF_Z=(SF-83.59753)/22.37642;
## RE_Z=(RE-81.29467)/33.02717;
## MH_Z=(MH-74.84212)/18.01189;

## ******************************************************************
## *     COMPUTE SAMPLE RAW FACTOR SCORES                           *
## *     Z SCORES ARE FROM ABOVE                                    *
## *     SCORING COEFFICIENTS ARE FROM U.S. GENERAL POPULATION      *
## *     FACTOR ANALYTIC SAMPLE N=2393: HAVE ALL EIGHT SCALES       *
## ******************************************************************;

## praw=(PF_Z * .42402)+(RP_Z * .35119)+(BP_Z * .31754)+
##      (SF_Z * -.00753)+(MH_Z * -.22069)+(RE_Z * -.19206)+
##      (VT_Z * .02877)+(GH_Z * .24954);

## mraw=(PF_Z * -.22999)+(RP_Z * -.12329)+(BP_Z * -.09731)+
##      (SF_Z * .26876)+(MH_Z * .48581)+(RE_Z * .43407)+
##      (VT_Z * .23534)+(GH_Z * -.01571);

## ******************************************************************
## *     Compute standardized scores                                *
## ******************************************************************;

## PCS = (praw*10) + 50;
## MCS = (mraw*10) + 50;

## label PCS='STANDARDIZED PHYSICAL COMPONENT SCALE-00'
##       MCS='STANDARDIZED MENTAL COMPONENT SCALE-00';




## PROC PRINT;VAR PF RP BP GH VT SF RE MH PCS MCS;ENDSAS;



#' SF-36 sample dataset
#'
#' SF-36 sample of 50 questionnaires \code{sf36} testing purposes.
#'
#' @format A data frame with 12 rows and 36 columns (1 per
#' SF-36 items)
#' @source SF-36 italian SAS algorithm available at
#' \url{http://crc.marionegri.it/qdv/index.php?page=sf36}
#' 
"sf36sample"
