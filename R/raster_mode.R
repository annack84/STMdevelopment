#' Calculate mode for aggregating ESG rasters to coarser resolution
#'
#' @param x Vector of values pulled from a raster. Can be numeric, character, factor, logical.
#' @param na.rm Logical. Omit NA values?
#'
#' @return Modal value of the inputs in same format as inputs. If multiple modes exist, returns the first appearing value of the set of modes.
#' @export
#'

raster_mode <- function(x, na.rm=T) {
  if(na.rm){
    x <- na.omit(x)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
