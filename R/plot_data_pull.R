#' Pull indicators from desired data sets for target ESG
#'
#' @param user Character. User name to generate input data file paths.
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
#' @return Wide format data frame containing indicators for the target ESG
#' @export

plot_data_pull <- function(user = "Anna",
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
                             "CP_percent_100to200", # Canopy gaps > 100 cm TODO do we want annual or perennial gaps?
                             "CP_percent_200plus",
                             "FH_LichenCover", # Lichen + moss combined cover
                             "FH_MossCover"),
                           shrub_by_spp = T, # All shrubs and sub-shrubs by species
                           subshrub_by_spp = T,
                           tree_by_spp = T, # All trees by species
                           opuntia_combined = T # Opuntia spp. (depending on prevalence)) # vector of indicator names
){
  file_paths <- data_file_paths(user)
  # extract ESG for each plot location
  plot_locations <- sf::st_read(dsn = file.path(file_paths$plotnet_processed, "NRI/NRI_PlotNet"),
                                layer = "all_plot-years_2021-09-30") # TODO write code to pull the
                                 # most recent version so we don't have to change this file name
                                 # when a new project is added to PlotNet

  ESG_raster <- raster::raster(file_paths$ESG_map)

  plot_ESGs <- sf::st_as_sf(raster::extract(x = ESG_raster,
                               y = plot_locations,
                               sp = T))

  plot_ESGs_join <- dplyr::left_join(plot_ESGs, ESG_table)

  # filter plots to those on target ESG from desired projects
  plot_target_ESG <- dplyr::filter(plot_ESGs_join,
                                   ESGs_text==target_ESG &
                                     grepl(x=PlotCode,
                                           pattern = paste(data_sources, collapse = "|")))


  # pull desired indicators from all plots on target ESG
  # list the indicator files
  plot_files <- list.files(file_paths$plotnet_processed,
                           full.names = T)
  plot_files <- grep(pattern = paste(data_sources, collapse = "|"),
                    x=plot_files, value = T)
  plot_files <- grep(pattern = ".csv", x=plot_files, value = T)

  species_files <- grep(x=plot_files, pattern = "_species_", value = T)
  indicator_files <- plot_files[-which(plot_files %in% species_files)]

  # compile indicator data
  indicator_data_all <- read.csv(indicator_files[1])
  indicator_data_all$PlotID <- as.character(indicator_data_all$PlotID)
  indicator_data_all$PlotName <- as.character(indicator_data_all$PlotName)

  for(file in indicator_files[-1]){
    indicator_data_temp <- read.csv(file)
    indicator_data_temp$PlotID <- as.character(indicator_data_temp$PlotID)
    indicator_data_temp$PlotName <- as.character(indicator_data_temp$PlotName)
    indicator_data_all <- dplyr::bind_rows(indicator_data_all, indicator_data_temp)
  }

  # filter to just desired indicators
  indicator_data <- dplyr::filter(indicator_data_all, variable %in% indicators)
  indicator_data$PlotCode <- paste(indicator_data$SourceKey,
                                   indicator_data$SiteName,
                                   indicator_data$PlotName,
                                   sep = "_")

  # filter indicators to just target ESG plots
  indicator_data_target <- dplyr::filter(indicator_data, PlotCode %in% plot_target_ESG$PlotCode) %>%
    dplyr::select(-Month, -Day, -Longitude_NAD83, -Latitude_NAD83)

  # calculate canopy gap >100 - doing this one separately because we don't want to autofill with 0s
  # like we do for LPI
  canopy_gaps_100 <- indicator_data_target %>%
    dplyr::filter(variable %in% c("CP_percent_100to200", "CP_percent_200plus")) %>%
    dplyr::group_by(PlotCode, SourceKey, PlotID, SiteName, PlotName, Year) %>%
    dplyr::summarize(variable = "CP_percent_100plus", value = sum(value, na.rm = T),
                     .groups = "drop") %>%
    tidyr::pivot_wider(data = ., names_from = variable, values_from = value) %>%
    dplyr::filter(!is.na(CP_percent_100plus))

  # make wide
  indicator_data_target_wide <- indicator_data_target %>%
    dplyr::filter(variable!="CP_percent_100to200" & variable!="CP_percent_200plus") %>%
    tidyr::pivot_wider(data = ., names_from = variable, values_from = value,
                       values_fill = 0) %>%
    dplyr::left_join(., canopy_gaps_100) %>%
    dplyr::filter(!is.na(CP_percent_100plus)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(FH_LichenMossCover = sum(FH_LichenCover, FH_MossCover, na.rm = T)) %>%
    dplyr::select(-FH_LichenCover, -FH_MossCover)


  # pull species-level cover data for target ESG plots
  # get species lists TODO update this list's C3/C4 designations based on Travis's lit review list
  if(shrub_by_spp|subshrub_by_spp|tree_by_spp|opuntia_combined){
    species_list <- read.csv(file_paths$species_list,
                             stringsAsFactors = F,
                             na.strings = c("NA", "", " "))

    shrub_spp <- dplyr::filter(species_list, GrowthHabitSub=="Shrub")
    subshrub_spp <- dplyr::filter(species_list, GrowthHabitSub=="SubShrub")
    tree_spp <- dplyr::filter(species_list, GrowthHabitSub=="Tree")
    Opunt_spp <- species_list[grep(pattern = "^Opuntia", x=species_list$ScientificName), ]

    # compile species data
    species_data_all <- read.csv(species_files[1])
    species_data_all$PlotID <- as.character(species_data_all$PlotID)
    species_data_all$PlotName <- as.character(species_data_all$PlotName)

    for(file in species_files[-1]){
      species_data_temp <- read.csv(file)
      species_data_temp$PlotID <- as.character(species_data_temp$PlotID)
      species_data_temp$PlotName <- as.character(species_data_temp$PlotName)
      species_data_all <- dplyr::bind_rows(species_data_all, species_data_temp)
    }

    # filter species to just target ESG plots
    species_data_all$PlotCode <- paste(species_data_all$SourceKey,
                                     species_data_all$SiteName,
                                     species_data_all$PlotName,
                                     sep = "_")

    species_data_all_target <- dplyr::filter(species_data_all, PlotCode %in% plot_target_ESG$PlotCode)

    # filter to just desired species
    species_data <- data.frame(SourceKey = character(),
                               PlotID = character(),
                               SiteName = character(),
                               PlotName = character(),
                               Longitude_NAD83 = double(),
                               Latitude_NAD83 = double(),
                               Year = integer(),
                               Month = integer(),
                               Day = integer(),
                               SpeciesCode = character(),
                               percent = double(),
                               PlotCode = character()
                               )
    if(shrub_by_spp){
      species_data_shrub <- dplyr::filter(species_data_all_target, SpeciesCode %in% shrub_spp$SpeciesCode)
      species_data <- dplyr::bind_rows(species_data, species_data_shrub)
    }
    if(subshrub_by_spp){
      species_data_subshrub <- dplyr::filter(species_data_all_target, SpeciesCode %in% subshrub_spp$SpeciesCode)
      species_data <- dplyr::bind_rows(species_data, species_data_subshrub)
    }
    if(tree_by_spp){
      species_data_tree <- dplyr::filter(species_data_all_target, SpeciesCode %in% tree_spp$SpeciesCode)
      species_data <- dplyr::bind_rows(species_data, species_data_tree)
    }
    if(opuntia_combined){
      species_data_opunt <- dplyr::filter(species_data_all_target, SpeciesCode %in% Opunt_spp$SpeciesCode) %>%
        dplyr::group_by(PlotCode, SourceKey, PlotID, SiteName, PlotName, Longitude_NAD83,
                        Latitude_NAD83, Year, Month, Day) %>%
        dplyr::summarize(SpeciesCode = "AH_OpuntiaCover",
                         percent = sum(percent))

      species_data <- dplyr::bind_rows(species_data, species_data_opunt)
    }
    # make wide
    species_data_wide <- tidyr::pivot_wider(data = species_data, names_from = SpeciesCode,
                                            values_from = percent, values_fill = 0)

    # combine with indicator data
    indicator_data_target_wide <- dplyr::left_join(indicator_data_target_wide, species_data_wide)
  }

return(indicator_data_target_wide)
}



