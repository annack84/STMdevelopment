#' Calculate weighted average of soil column based on trapezoidal rule
#'
#' The purpose of this function is to calculate the weighted average of a soil
#' property across a soil column over standard depth intervals. This function
#' is designed to work on 4 depth breaks. This uses the trapezoidal rule as
#' described in Hengl et al. (2017, https://doi.org/10.1371/journal.pone.0169748).
#'
#' @param depth1 Numeric. Depth of top soil column interval. Defaults to 0 cm.
#' @param depth2 Numeric. Depth of second from top soil column interval. Defaults to 5 cm.
#' @param depth3 Numeric. Depth of third from top soil column interval. Defaults to 15 cm.
#' @param depth4 Numeric. Depth of bottom soil column interval. Defaults to 30 cm.
#' @param value1 Numeric. Value of the soil property at depth1.
#' @param value2 Numeric. Value of the soil property at depth2.
#' @param value3 Numeric. Value of the soil property at depth3.
#' @param value4 Numeric. Value of the soil property at depth4.
#'
#' @return Numeric. Weighted mean of soil values.
#' @export
#'
calc_soil_trapezoidal <- function(depth1 = 0,
                                  depth2 = 5,
                                  depth3 = 15,
                                  depth4 = 30,
                                  value1,
                                  value2,
                                  value3,
                                  value4
                                  ){
  trapezoidal_mean <- (1/(depth4-depth1)) * (0.5) *
    ((depth2-depth1)*(value2 + value1) +
       (depth3-depth2)*(value3+value2) +
       (depth4-depth3)*(value4+value3))
  return(trapezoidal_mean)
}
