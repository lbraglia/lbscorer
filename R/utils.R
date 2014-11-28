## If a variable has values outside Min or Max, set them to NA
outRangeNA <- function(x, Min = 1L, Max) replace(x, x < Min | x > Max, NA)

## Recoding utility for sf36, BP section
sf36recBP <- function(bp1, bp2) {

  ## -------------------------------------------
  ## The chunk of code to be translated from SAS
  ## -------------------------------------------
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
  
  ## -------------------------------------------
  ## Phase I: not missing cases
  ## -------------------------------------------
  bp1codes <- c(6, 5.4, 4.2, 3.1, 2.2, 1)
  rbp1 <- bp1codes[as.integer(bp1)]
  rbp2 <- as.integer(6L - bp2)
  rbp2 <- ifelse((bp1 %in% 1L) & (rbp2 %in% 5L), 6L, rbp2)

  ## -------------------------------------------
  ## Phase 2: where bp1 is not missing and bp2 is missing, assign rbp1 to
  ##          rbp2 
  ## -------------------------------------------
  phase2cases <- (!is.na(bp1)) & (is.na(bp2))
  rbp2[phase2cases] <- rbp1[phase2cases]

  ## -------------------------------------------
  ## Phase 3: where bp1 is missing and bp2 is not missing, recode rbp2 as
  ##          above then assign rbp2 to rbp1
  ## -------------------------------------------
  phase3cases <- (is.na(bp1)) & (!is.na(bp2))
  bp2codes <- c(6, 4.75, 3.5, 2.25, 1)
  rbp2[phase3cases] <- bp2codes[as.integer(bp2[phase3cases])]
  rbp1[phase3cases] <- rbp2[phase3cases]

  ## -------------------------------------------
  ## Exit
  ## -------------------------------------------
  return(data.frame(RBP1 = rbp1, RBP2 = rbp2))
} # End sf36recBP
