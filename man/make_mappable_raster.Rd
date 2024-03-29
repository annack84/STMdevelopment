% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_mappable_ESG_rasters.R
\name{make_mappable_raster}
\alias{make_mappable_raster}
\title{Create a binary raster for each ESG}
\usage{
make_mappable_raster(
  target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands",
  user = "Anna"
)
}
\arguments{
\item{target_ESG}{Character. Ecological Site Group to be mapped.
Follow the names used in the ESG raster released with Travis's 2021 manuscript.}

\item{user}{Character. User name to generate input data file paths.See
\code{\link{data_file_paths}} for input options.}
}
\value{
Raster where 1=target ESG and 0=not target ESG
}
\description{
Create a binary raster for each ESG
}
