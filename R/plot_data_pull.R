#' Pull indicators from desired data sets for target ESG
#'
#' @param file_paths_user Character. User name to generate input data file paths.
#'   Options are "Anna", "Travis", and "VPN".
#' @param target_ESG Character. Ecological Site Group to pull plot data for.
#'   Follow the names used in the ESG raster released with Travis's 2021 manuscript.
#' @param data_sources Character or vector of characters indicating which projects
#'  in PlotNet, AIM, and NRI to use data from. Options include any project in PlotNet
#'  but recommended to select from the following: "BadgerWash", "CRCMonitoring",
#'  "IM_NCPN", "LMF", "NRI", "Parashant", "TerrADat", "VanScoyocThesis"
#' @param indicators Character vector of indicators to return data from. Can
#'   include anything calculated in PlotNet.
#' @param shrub_by_spp Logical. Include all shrubs by species?
#' @param subshrub_by_spp Logical. Include all sub-shrubs by species?
#' @param tree_by_spp Logical. Include all trees by species?
#' @param opuntia_combined Logical. Include combined cover of genus Opuntia?
#'
#' @return
#' @export

plot_data_pull <- function(file_paths_user = "Anna",
                           target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands",
                           data_sources = c(# "ARSLAKE", # probably won't overlap
                             "BadgerWash",
                             #"BSNEs", # Mike says don't use
                             "CRCMonitoring",
                             #"GrandCanyonUranium", # should probably drop this one?
                             # "IM_MOJN", # probably won't overlap
                             "IM_NCPN",
                             "LMF",
                             # "MoabBLMTx",# No
                             # "MilfordFlat", # outside of study area
                             #"MFO", # Need to run
                             "NRI",
                             # "NWERN", # probably too small to be worth using
                             #"PadDust", # no
                             "Parashant",
                             # "SwEDD", # no
                             # "USFWSPartners", # maybe? still in development but only from untreated areas currently
                             "TerrADat", # aka AIM
                             "VanScoyocThesis"
                           ),
                           indicators = c(# Cover types to pull:
                             "AH_C3PerenGrassCover", # C3 native perennial grasses TODO calculate C3 NATIVE
                             "AH_C4PerenGrassCover", # C4 native perennial grasses TODO calculate C4 NATIVE
                             "AH_IntroducedPerenGrassCover", # Non-native perennial grasses
                             "AH_NativePerenForbCover", # Native perennial forbs
                             "AH_IntroducedPerenForbCover", # Non-native perennial forbs
                             "AH_NativeAnnGrassCover", # Native annual grasses
                             "AH_IntroducedAnnGrassCover", # Non-native annual grasses
                             "AH_NativeAnnForbCover", # Native annual forbs
                             "AH_IntroducedAnnForbCover", # Non-native annual forbs
                             "BareSoilCover", # Bare soil
                             "CP_percent_100to200", # Canopy gaps > 100 cm
                             "CP_percent_200plus",
                             "FH_LichenCover", # Lichen + moss combined cover
                             "FH_MossCover"),
                           shrub_by_spp = T, # All shrubs and sub-shrubs by species
                           subshrub_by_spp = T,
                           tree_by_spp = T, # All trees by species
                           opuntia_combined = T # Opuntia spp. (depending on prevalence)) # vector of indicator names
){
  file_paths <- data_file_paths(file_paths_user)
  # 1. extract ESG for each plot location
  plot_locations <- sf::st_read(dsn = file.path(file_paths$plotnet_processed, "NRI/NRI_PlotNet"),
                                layer = "all_plot-years_2021-03-02") # TODO write code to pull the
                                 # most recent version so we don't have to change this file name
                                 # when a new project is added to PlotNet

  ESG_raster <- raster::raster(file_paths$ESG_map)

  plot_ESGs <- sf::st_as_sf(raster::extract(x = ESG_raster,
                               y = plot_locations,
                               sp = T))

  lookup_table <- dplyr::tribble(
    ~ESGs_final, ~ESGs_text,
          1,      "Outcrops",
          2,      "Arid_Warm_Breaks",
          3,      "Arid_Warm_SalineHills",
          4,      "Arid_Warm_Gypsum",
          5,      "Arid_Warm_VeryShallow",
          6,      "Arid_Warm_SalineUplands",
          7,      "Arid_Warm_Shallow",
          8,      "Arid_Warm_DeepRocky",
          9,      "Arid_Warm_SandyUplands_LoamyUplands",
          10,     "Arid_Warm_FinerUplands_ClayUplands",
          11,     "Arid_Warm_SandyBottoms",
          12,     "Arid_Warm_SalineBottoms_Bottoms",
          13,     "Semiarid_Warm_Breaks",
          14,     "Semiarid_Warm_SalineHills",
          15,     "Semiarid_Warm_Gypsum",
          16,     "Semiarid_Warm_VeryShallow",
          17,     "Semiarid_Warm_SalineUplands",
          18,     "Semiarid_Warm_Shallow_DeepRocky",
          19,     "Semiarid_Warm_SandyUplands_LoamyUplands",
          20,     "Semiarid_Warm_FinerUplands",
          21,     "Semiarid_Warm_ClayUplands",
          22,     "Semiarid_Warm_SandyBottoms_Bottoms",
          23,     "Semiarid_Warm_SalineBottoms",
          24,     "Semiarid_Cool_Breaks",
          25,     "Semiarid_Cool_SalineHills",
          26,     "Semiarid_Cool_Gypsum",
          27,     "Semiarid_Cool_VeryShallow",
          28,     "Semiarid_Cool_SalineUplands_SandyUplands_LoamyUplands_FinerUplands",
          29,     "Semiarid_Cool_Shallow",
          30,     "Semiarid_Cool_DeepRocky",
          31,     "Semiarid_Cool_ClayUplands",
          32,     "Semiarid_Cool_SandyBottoms",
          33,     "Semiarid_Cool_SalineBottoms",
          34,     "Semiarid_Cool_Bottoms",
          35,     "Riparian"
  )

  plot_ESGs_join <- dplyr::left_join(plot_ESGs, lookup_table)

  # 2. filter plots to those on target ESG from desired projects
  plot_target_ESG <- dplyr::filter(plot_ESGs_join,
                                   ESGs_text==target_ESG &
                                     grepl(x=PlotCode,
                                           pattern = paste(data_sources, collapse = "|")))


  # 3. pull desired indicators from all plots on target ESG
  # list the indicator files
  plot_files <- list.files(file_paths$plotnet_processed,
                           full.names = T)
  plot_files <- grep(pattern = paste(data_sources, collapse = "|"),
                    x=plot_files, value = T)
  plot_files <- grep(pattern = ".csv", x=plot_files, value = T)

  species_files <- grep(x=plot_files, pattern = "_species_", value = T)
  indicator_files <- plot_files[-which(plot_files %in% species_files)]

  # get species lists
  # TODO update this list's C3/C4 designations based on Travis's lit review list
  species_list <- read.csv("data/SpeciesList_WesternUS_AcceptedSymbols_2020-01-06.csv",
                           stringsAsFactors = F,
                           na.strings = c("NA", "", " "))

  shrub_spp <- dplyr::filter(species_list, GrowthHabitSub=="Shrub")
  subshrub_spp <- dplyr::filter(species_list, GrowthHabitSub=="SubShrub")
  tree_spp <- dplyr::filter(species_list, GrowthHabitSub=="Tree")
  Opunt_spp <- species_list[grep(pattern = "^Opuntia", x=species_list$ScientificName), "SpeciesCode"]

  # compile data and filter to just desired indicators and plots
  indicator_data_all <- read.csv(indicator_files[1])
  indicator_data_all$PlotID <- as.character(indicator_data_all$PlotID)
  indicator_data_all$PlotName <- as.character(indicator_data_all$PlotName)

  for(file in indicator_files[-1]){
    indicator_data_temp <- read.csv(file)
    indicator_data_temp$PlotID <- as.character(indicator_data_temp$PlotID)
    indicator_data_temp$PlotName <- as.character(indicator_data_temp$PlotName)
    indicator_data_all <- dplyr::bind_rows(indicator_data_all, indicator_data_temp)
  }

  indicator_data <- dplyr::filter(indicator_data_all, variable %in% indicators)
  indicator_data$PlotCode <- paste(indicator_data$SourceKey,
                                   indicator_data$SiteName,
                                   indicator_data$PlotName,
                                   sep = "_")
  # NEXT: reduce indicator_data to just the plots in our target ESG,
  # do the requisit minor math canopy gaps and lichen+mosses,
  # then similar data pulls for the species-level data

}



