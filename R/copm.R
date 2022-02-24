#' Canadian Occupational Performance Measure
#'
#' Canadian Occupational Performance Measure calculation given study variables
#' 
#' @param id patient id: there can be max 5 rows per id, or an error
#'     is thrown
#' @param performance1 performance self assessment at first evaluation
#' @param satisfaction1 satisfaction self assessment at first
#'     evaluation
#' @param performance2 performance self assessment at re-evaluation
#' @param satisfaction2 satisfaction self assessment at re-evaluation
#'
#' @examples
#' set.seed(1)
#' id <- gl(2,5)
#' perf1 <- sample(1:10, replace = TRUE)
#' perf2 <- sample(1:10, replace = TRUE)
#' sat1 <- sample(1:10, replace = TRUE)
#' sat2 <- sample(1:10, replace = TRUE)
#'
#' data.frame(id, perf1, sat1, perf2, sat2)
#' copm(id, perf1, sat1, perf2, sat2)
#' 
#' @export
copm <- function(id, performance1, satisfaction1, performance2, satisfaction2){
    df <- data.frame(id, performance1, satisfaction1, 
                    performance2, satisfaction2)
    if (any(table(id) > 5)) 
        stop("Only the 5 most important problems have to be considered (there are patients with more than 5 rows)")
    df_spl <- split(df, df$id)
    res <- lapply(df_spl, function(x){
        performance_score1 <- mean(x$performance1, na.rm = TRUE)
        performance_score2 <- mean(x$performance2, na.rm = TRUE)
        satisfaction_score1 <- mean(x$satisfaction1, na.rm = TRUE)
        satisfaction_score2 <- mean(x$satisfaction2, na.rm = TRUE)
        res <- data.frame(
            id = x$id[1],
            performance_score1 = performance_score1,
            performance_score2 = performance_score2,
            satisfaction_score1 = satisfaction_score1,
            satisfaction_score2 = satisfaction_score2,
            copm_change_performance = performance_score2 - performance_score1,
            copm_change_satisfaction = satisfaction_score2 - satisfaction_score1
        )
    })
    res <- do.call(rbind, res)
    ## remove NaN and make them NA
    res <- lapply(res, function(x) ifelse(is.nan(x), NA, x))
    data.frame(res)
}
