outRangeNA <- function(x, Min = 1L, Max) replace(x, x < Min | x > Max, NA)
