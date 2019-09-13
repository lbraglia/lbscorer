#' Activities Scale for Kids - Performance version (30 items)
#'
#' Activities Scale for Kids - Performance version (30 items)
#'
#' @param Q a matrix or data.frame of 30 columns (corresponing, in a
#'     ordered way to each item respectively, so therefore Q[,1] is
#'     the first item of the scale) filled by 1 (All of the time at
#'     all) to 5 (None of the time) values, with 6 as special case
#'     (Not applicable). Item ARE reversed.
#' @export
ASK_p <- function( Q = NULL) {

    if(! (is.data.frame(Q) || is.matrix(Q)))
        stop("Q must be a data.frame or a matrix")
    if (ncol(Q) != 30)
        stop("Q must have 30 columns")
    if (! all(as.matrix(Q) %in% c(NA, 1:6))) 
        stop("Q must have  columns")

    ## removing not applicable answers
    Q <- apply(Q, 2, function(x) {
        x[x %in% 6] <- NA
        x
    })

    ## reverse scoring
    Q <- apply(Q, 2, function(x) {5 - x})
    
    ## score: mean of complete items * 25, for those who have at least 23
    ## filled items
    score <- apply(Q, 1, function(x) {
        if (sum(!is.na(x)) >= 23L)
            mean(x, na.rm = TRUE) * 25
        else
            NA
    })

    score
}
