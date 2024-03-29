% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_data_pull.R
\name{plot_data_pull}
\alias{plot_data_pull}
\title{Pull indicators from desired data sets for target ESG}
\usage{
plot_data_pull(
  user = "Anna",
  target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands",
  data_sources = c("BadgerWash", "CRCMonitoring", "IM_NCPN", "LMF", "NRI", "Parashant",
    "AIM", "VanScoyocThesis"),
  indicators = c("AH_C3PerenGrassCover", "AH_C4PerenGrassCover",
    "AH_IntroducedPerenGrassCover", "AH_NativePerenForbCover",
    "AH_IntroducedPerenForbCover", "AH_NativeAnnGrassCover",
    "AH_IntroducedAnnGrassCover", "AH_NativeAnnForbCover", "AH_IntroducedAnnForbCover",
    "AH_ArtemisiaTridentataCover", "BareSoilCover", "CP_percent_100to200",
    "CP_percent_200plus", "FH_LichenCover", "FH_MossCover"),
  ann_grass_by_spp = FALSE,
  ann_forb_by_spp = FALSE,
  per_grass_by_spp = FALSE,
  per_forb_by_spp = FALSE,
  succulent_by_spp = FALSE,
  shrub_by_spp = TRUE,
  subshrub_by_spp = TRUE,
  tree_by_spp = TRUE,
  opuntia_combined = TRUE,
  impute_gap_type = NULL,
  impute_sources = NULL
)
}
\arguments{
\item{user}{Character. User name to generate input data file paths. See
\code{\link{data_file_paths}} for input options.}

\item{target_ESG}{Character. Ecological Site Group to pull plot data for.
Follow the names used in the ESG raster released with Travis's 2021 manuscript.}

\item{data_sources}{Character or vector of characters indicating which projects
in PlotNet, AIM, and NRI to use data from. Options include any project in PlotNet
but recommended to select from the following: "BadgerWash", "CRCMonitoring",
"IM_NCPN", "LMF", "NRI", "Parashant", "TerrADat", "VanScoyocThesis"}

\item{indicators}{Character vector of indicators to return data from. Can
include anything calculated in PlotNet.}

\item{ann_grass_by_spp}{Logical. Include all annual grasses by species?}

\item{ann_forb_by_spp}{Logical. Include all annual forbs by species?}

\item{per_grass_by_spp}{Logical. Include all perennial grasses by species?}

\item{per_forb_by_spp}{Logical. Include all perennial forbs by species?}

\item{succulent_by_spp}{Logical. Include all succulents by species? If TRUE,
opuntia_combined should be FALSE}

\item{shrub_by_spp}{Logical. Include all shrubs by species?}

\item{subshrub_by_spp}{Logical. Include all sub-shrubs by species?}

\item{tree_by_spp}{Logical. Include all trees by species?}

\item{opuntia_combined}{Logical. Include combined cover of genus Opuntia? If
TRUE, succulent_by_spp should be FALSE}

\item{impute_gap_type}{Character vector. Gap type to predict. Supported
options are "CA_percent_100plus" (\% cover annual & perennial gaps >100 cm)
and "CA_percent_200plus" (\% cover annual & perennial gaps >200 cm). Set to
NULL if no imputation is desired.}

\item{impute_sources}{Character vector. Optional. Only impute missing data for
specific data source(s). Set to NULL if not imputation is desired.}
}
\value{
Wide format data frame containing indicators for the target ESG
}
\description{
Pull indicators from desired data sets for target ESG
}
