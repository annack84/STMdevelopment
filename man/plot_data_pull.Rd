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
    "TerrADat", "VanScoyocThesis"),
  indicators = c("AH_C3PerenGrassCover", "AH_C4PerenGrassCover",
    "AH_IntroducedPerenGrassCover", "AH_NativePerenForbCover",
    "AH_IntroducedPerenForbCover", "AH_NativeAnnGrassCover",
    "AH_IntroducedAnnGrassCover", "AH_NativeAnnForbCover", "AH_IntroducedAnnForbCover",
    "BareSoilCover", "CP_percent_100to200", "CP_percent_200plus", "FH_LichenCover",
    "FH_MossCover"),
  shrub_by_spp = T,
  subshrub_by_spp = T,
  tree_by_spp = T,
  opuntia_combined = T
)
}
\arguments{
\item{user}{Character. User name to generate input data file paths.
Options are "Anna", "Travis", and "VPN".}

\item{target_ESG}{Character. Ecological Site Group to pull plot data for.
Follow the names used in the ESG raster released with Travis's 2021 manuscript.}

\item{data_sources}{Character or vector of characters indicating which projects
in PlotNet, AIM, and NRI to use data from. Options include any project in PlotNet
but recommended to select from the following: "BadgerWash", "CRCMonitoring",
"IM_NCPN", "LMF", "NRI", "Parashant", "TerrADat", "VanScoyocThesis"}

\item{indicators}{Character vector of indicators to return data from. Can
include anything calculated in PlotNet.}

\item{shrub_by_spp}{Logical. Include all shrubs by species?}

\item{subshrub_by_spp}{Logical. Include all sub-shrubs by species?}

\item{tree_by_spp}{Logical. Include all trees by species?}

\item{opuntia_combined}{Logical. Include combined cover of genus Opuntia?}
}
\value{
Wide format data frame containing indicators for the target ESG
}
\description{
Pull indicators from desired data sets for target ESG
}
