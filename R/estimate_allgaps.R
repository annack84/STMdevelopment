#' Estimate annual & perennial canopy gaps from perennial canopy gaps and LPI
#' cover data with a linear model built by Joe Brehm, to be published in McCord
#' et al. (in prep as of 10/2023). All estimates require values for (1) the
#' perennial gap window that matches the annual & perennial gap window to be
#' estimated (e.g. to estimate annual gaps > 25 cm, use perennial gaps > 25 cm),
#' (2) annual grass cover, (3) perennial grass cover, (4) shrub cover, and (5)
#' forb cover. All cover variables should come from any-hit indicators
#' calculated from LPI data. Structured to be used AFTER \link{plot_data_pull},
#' not with WITHIN \link{plot_data_pull}.
#'
#' @param gap_type Character indicating the gap window to be predicted. One of
#'   "25plus", "50plus", "100plus", or "200plus". Defaults to "25 plus".
#' @param perennial_gaps Numeric vector containing the perennial gap values.
#'   Example indicator column name is "CP_percent_25plus".
#' @param AG Numeric vector containing the any-hit annual grass cover values.
#'   Example indicator column name is "AH_AnnGrassCover".
#' @param PG Numeric vector containing the any-hit perennial grass cover values.
#'   Example indicator column name is "AH_PerenGrassCover".
#' @param SH Numeric vector containing the any-hit shrub cover values. Example
#'   indicator column name is "AH_ShrubCover".
#' @param Forb Numeric vector containing the any-hit forb cover values. Example
#'   indicator column name is "AH_ForbCover".
#' @return Numeric vector of estimated annual and perennial gap values for the
#'   descriptor indicated in \code{gap_type}
#' @export


estimate_allgaps <- function(gap_type = "25plus",
                             perennial_gaps,
                             AG,
                             PG,
                             SH,
                             Forb
                             ){
  if(gap_type == "25plus"){
    all_gaps <- 7.61 + 0.69*perennial_gaps - 0.79*AG +0.05*PG + 2.8*SH - 0.75*Forb
  }
  if(gap_type == "50plus"){
    all_gaps <- 11.65 + 0.63*perennial_gaps - 0.78*AG + 0.02*PG + 0.21*SH - 0.79*Forb
  }
  if(gap_type == "100plus"){
    all_gaps <- 9.12 + 0.63*perennial_gaps - 0.77*AG + 0.12*PG + 0.21*SH - 0.85*Forb
  }
  if(gap_type == "200plus"){
    all_gaps <- 7.64 + 0.67*perennial_gaps - 0.74*AG + 0.23*PG + 0.12*SH - 1.11*Forb
  }
  return(all_gaps)
}
