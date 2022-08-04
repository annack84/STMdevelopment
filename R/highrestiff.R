#' Save figure as a high-resolution TIFF file
#'
#' This function saves a figure as a high resolution TIFF file
#'
#' @param plot_obj Figure to be saved
#' @param file Character. Path and name of the file to save the figure in.
#' @param width_in Numeric. Width of the saved figure in inches
#' @param height_in Numeric. Height of the saved figure in inches
#' @param resolution_dpi Numeric. Image resolution in dots per square inch.
#'
#' @return List. Use this list in other functions to access data from different
#'  file locations
#' @rdname highrestiff
#' @export highrestiff
#'
highrestiff <- function(plot_obj, file, width_in, height_in, resolution_dpi=300) {

  tiff(file, width = width_in,
       height = height_in,
       units = 'in',
       res = resolution_dpi,
       compression="lzw")
  plot0 <- plot_obj
  print(plot0)
  dev.off()
  invisible()

}
