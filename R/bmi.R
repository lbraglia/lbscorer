#' Calculate Body Mass Index
#'
#' @param weight.kg weight in kg
#' @param height.m height in m
#' @param bmi body mass index already computed
#' @param categ logical indicating whether to return a factor
#' (who classification) 
#' @param labels if \code{categ == TRUE}, logical indicating whether to
#' return factor with labels (who classification)
#' @return a vector of body mass index
#' @examples
#'
#' ## Simple calculation
#' bmi(w = 85, h = 1.85)
#' ## Who classes without or with labels
#' bmi(b = 24, categ = TRUE)
#' bmi(b = 27, categ = TRUE, labels = TRUE)
#' 
#' @export
bmi <- function(weight.kg = NULL,
                height.m = NULL,
                bmi = NULL,
                categ = FALSE,
                labels = FALSE) {
    
    if (! is.null(bmi)) {
        my.bmi <- bmi
    } else if (!any( c(is.null(weight.kg),is.null(height.m)))) {
        my.bmi <- weight.kg/(height.m^2)
    } else {
        stop("Either weight.kg and height.m or bmi, have to be setted")
    }
    
    if (categ) {
        ## Who: http://apps.who.int/bmi/index.jsp?introPage=intro_3.html
        my.bmi <- ifelse(my.bmi<18.5, 1,
                  ifelse(my.bmi<25  , 2,
                  ifelse(my.bmi<30  , 3, 4)))
        lab <- if(labels) {
            c("Underweight","Normal range", "Overweight","Obese")
        } else {
            c("[min-18.5)","[18.5-25)","[25-30)", "[30-max]")
        }
        my.bmi <- factor(my.bmi, levels=1:4, labels=lab)
    }

    return(my.bmi)
}
