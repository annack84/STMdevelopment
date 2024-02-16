#' Pull indicators from desired data sets for target ESG
#'
#' @param user Character. User name to generate input data file paths. See
#' \code{\link{data_file_paths}} for input options.
#' @param target_ESG Character. Ecological Site Group to pull plot data for.
#'   Follow the names used in the ESG raster released with Travis's 2021 manuscript.
#' @param data_sources Character or vector of characters indicating which projects
#'  in PlotNet, AIM, and NRI to use data from. Options include any project in PlotNet
#'  but recommended to select from the following: "BadgerWash", "CRCMonitoring",
#'  "IM_NCPN", "LMF", "NRI", "Parashant", "TerrADat", "VanScoyocThesis"
#' @param indicators Character vector of indicators to return data from. Can
#' include anything calculated in PlotNet.
#' @param ann_grass_by_spp Logical. Include all annual grasses by species?
#' @param ann_forb_by_spp Logical. Include all annual forbs by species?
#' @param per_grass_by_spp Logical. Include all perennial grasses by species?
#' @param per_forb_by_spp Logical. Include all perennial forbs by species?
#' @param succulent_by_spp Logical. Include all succulents by species? If TRUE,
#'   opuntia_combined should be FALSE
#' @param shrub_by_spp Logical. Include all shrubs by species?
#' @param subshrub_by_spp Logical. Include all sub-shrubs by species?
#' @param tree_by_spp Logical. Include all trees by species?
#' @param opuntia_combined Logical. Include combined cover of genus Opuntia? If
#'   TRUE, succulent_by_spp should be FALSE
#' @param impute_gap_type Character vector. Gap type to predict. Supported
#'   options are "CA_percent_100plus" (% cover annual & perennial gaps >100 cm)
#'   and "CA_percent_200plus" (% cover annual & perennial gaps >200 cm). Set to
#'   NULL if no imputation is desired.
#' @param impute_sources Character vector. Optional. Only impute missing data for
#'   specific data source(s). Set to NULL if not imputation is desired.
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
                             #"TerrADat", # aka AIM
                             "AIM",
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
                             "AH_ArtemisiaTridentataCover", # all big sagebrush subspecies combined
                             "BareSoilCover", # Bare soil
                             "CP_percent_100to200", # Canopy gaps > 100 cm TODO do we want annual or perennial gaps?
                             "CP_percent_200plus",
                             "FH_LichenCover", # Lichen + moss combined cover
                             "FH_MossCover"),
                           ann_grass_by_spp = FALSE,
                           ann_forb_by_spp = FALSE,
                           per_grass_by_spp = FALSE,
                           per_forb_by_spp = FALSE,
                           succulent_by_spp = FALSE,
                           shrub_by_spp = TRUE, # All shrubs and sub-shrubs by species
                           subshrub_by_spp = TRUE,
                           tree_by_spp = TRUE, # All trees by species
                           opuntia_combined = TRUE, # Opuntia spp. (depending on prevalence)) # vector of indicator names
                           impute_gap_type = NULL,
                           impute_sources = NULL
){
  # Check arguments
  if(!is.null(impute_sources)){
    if(!all(impute_sources %in% data_sources)){stop("All impute_sources must also be included in the data_sources argument.")}
    if(is.null(impute_gap_type)){stop("If you supply impute_sources, you must also supply impute_gap_type. Options are c('CA_percent_100plus', 'CA_percent_200plus')")}
  }

  file_paths <- data_file_paths(user)
  # extract ESG for each plot location
  plot_location_file <- last(sort(grep(x=list.files(file.path(file_paths$plotnet_processed, "PlotLocations")), pattern = "all_plot-years_", value = T) %>%
                                    grep(x=., pattern=".shp$", value = T)))
  #plot_location_file <- substr(plot_location_file, 1, nchar(plot_location_file)-4)
  plot_location_file <- gsub(pattern = ".shp$", replacement = "", x=plot_location_file)
  plot_locations <- sf::st_read(dsn = file.path(file_paths$plotnet_processed, "PlotLocations"),
                                layer = plot_location_file,
                                quiet=TRUE) # TODO write code to pull the
  # most recent version so we don't have to change this file name
  # when a new project is added to PlotNet

  if("NRI" %in% data_sources){
    nri_location_file <- last(sort(grep(x=list.files(file_paths$nri), pattern = "NRI_UCRB_plot-years_", value = T)))
    nri_location_file <- substr(nri_location_file, 1, nchar(nri_location_file)-4)
    nri_locations <- sf::st_read(dsn = file_paths$nri,
                                 layer = nri_location_file,
                                 quiet = TRUE)

    plot_locations <- dplyr::filter(plot_locations, !grepl(pattern = "^NRI_",
                                                           x=PlotCode)) %>%
      dplyr::bind_rows(., nri_locations)
  }

  ESG_raster <- raster::raster(file_paths$ESG_map)

  plot_ESGs <- sf::st_as_sf(raster::extract(x = ESG_raster,
                                            y = plot_locations,
                                            sp = T))
  colnames(plot_ESGs)[which(colnames(plot_ESGs)=="ESG")] <- "ESGid"

  plot_ESGs_join <- dplyr::left_join(plot_ESGs, ESG_table)

  # remove "forest" stratified plots from NCPN because they don't record overstory > 2 m
  NCPN_forest_plots <- read.csv(file.path(dirname(file_paths$plotnet_processed),
                                          "ProcessingIntermediates", "InventoryAndMonitoring",
                                          "NCPN", "IM_NCPN_forest_plots.csv")) %>%
    dplyr::mutate(PlotCode = paste("IM_NCPN", Park, Plot, sep = "_"))

  # filter plots to those on target ESG from desired projects
  plot_target_ESG <- dplyr::filter(plot_ESGs_join,
                                   ESGs_text==target_ESG &
                                     grepl(x=PlotCode,
                                           pattern = paste(data_sources, collapse = "|"))) %>%
    dplyr::filter(!(PlotCode %in% NCPN_forest_plots$PlotCode))

  # pull desired indicators from all plots on target ESG
  # list the indicator files
  plot_files <- list.files(file_paths$plotnet_processed,
                           full.names = T)
  if("NRI" %in% data_sources){
    nri_files <- list.files(file_paths$nri,
                            full.names = T)
    plot_files <- c(plot_files, nri_files)
  }
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

  # remove duplicate records if necessary
  indicator_data_all <- distinct(indicator_data_all)

  # filter to just desired indicators
  indicators_expanded <- indicators
  if(!is.null(impute_gap_type)){
    indicators_expanded <- unique(c(indicators_expanded,
                                    stringr::str_replace_all(impute_gap_type, "CA_", "CP_"),
                                    "AH_AnnForbGrassCover"))
  }
  if("CP_percent_100plus" %in% indicators_expanded){
    indicators_expanded <- unique(c(indicators_expanded, "CP_percent_100to200", "CP_percent_200plus"))
  }
  if("CA_percent_100plus" %in% indicators_expanded){
    indicators_expanded <- unique(c(indicators_expanded, "CA_percent_100to200", "CA_percent_200plus"))
  }


  indicator_data <- dplyr::filter(indicator_data_all, variable %in% indicators_expanded)
  indicator_data$PlotCode <- paste(indicator_data$SourceKey,
                                   indicator_data$SiteName,
                                   indicator_data$PlotName,
                                   sep = "_")

  # filter indicators to just target ESG plots
  indicator_data_target <- dplyr::filter(indicator_data, PlotCode %in% plot_target_ESG$PlotCode) #%>%
    #dplyr::select(-Month, -Day, -Longitude_NAD83, -Latitude_NAD83)

  # make wide
  indicator_data_target_wide <- indicator_data_target %>%
    #dplyr::filter(variable!="CP_percent_100to200" & variable!="CP_percent_200plus") %>%
    tidyr::pivot_wider(data = ., names_from = variable, values_from = value,
                       values_fill = NA)

  if("FH_LichenCover" %in% indicators){
    indicator_data_target_wide <- indicator_data_target_wide %>%
      dplyr::rowwise() %>%
      dplyr::mutate(FH_LichenMossCover = sum(FH_LichenCover, FH_MossCover, na.rm = T)) %>%
      dplyr::select(-FH_LichenCover, -FH_MossCover)
  }


  # Missing values for LPI measure mean that functional group wasn't present at
  # a plot. These should be changed to 0. For soil stability and canopy gap,
  # NA means those data weren't collected at the plot, so they should remain NA
  replaceNA <- function(x){
    replace(x, is.na(x), 0)
  }

  indicator_data_target_wide <- indicator_data_target_wide %>%
    mutate(across(.cols = starts_with("AH_"), .fns = replaceNA)) %>%
    mutate(across(.cols = starts_with("FH_"), .fns = replaceNA)) %>%
    mutate(across(.cols = any_of(c("BareSoilCover", "TotalFoliarCover")), .fns = replaceNA))
    #mutate(BareSoilCover = replaceNA(BareSoilCover))

  # calculate canopy gap >100 - doing this one separately because we don't want to autofill with 0s
  # like we do for LPI
  if("CP_percent_100plus" %in% indicators_expanded){
    canopy_gaps_100plus <- indicator_data_target %>%
      dplyr::filter(variable %in% c("CP_percent_100to200", "CP_percent_200plus")) %>%
      dplyr::group_by(PlotCode, SourceKey, PlotID, SiteName, PlotName, Year) %>%
      dplyr::summarize(variable = "CP_percent_100plus", value = sum(value, na.rm = T),
                       .groups = "drop") %>%
      tidyr::pivot_wider(data = ., names_from = variable, values_from = value) %>%
      dplyr::filter(!is.na(CP_percent_100plus))

    indicator_data_target_wide <- indicator_data_target_wide %>%
      dplyr::left_join(., canopy_gaps_100plus) #%>%
    #dplyr::filter(!is.na(CP_percent_100plus))
  }

  if("CA_percent_100plus" %in% indicators_expanded){
    canopy_gaps_100plus <- indicator_data_target %>%
      dplyr::filter(variable %in% c("CA_percent_100to200", "CA_percent_200plus")) %>%
      dplyr::group_by(PlotCode, SourceKey, PlotID, SiteName, PlotName, Year) %>%
      dplyr::summarize(variable = "CA_percent_100plus", value = sum(value, na.rm = T),
                       .groups = "drop") %>%
      tidyr::pivot_wider(data = ., names_from = variable, values_from = value) %>%
      dplyr::filter(!is.na(CA_percent_100plus))

    indicator_data_target_wide <- indicator_data_target_wide %>%
      dplyr::left_join(., canopy_gaps_100plus) #%>%
    #dplyr::filter(!is.na(CA_percent_100plus))
  }

  # impute missing gap values if desired here
  if(!is.null(impute_gap_type)){
    indicator_data_target_wide <- impute_missing_gaps(indicator_data_target_wide = indicator_data_target_wide,
                                                      impute_gap_type = impute_gap_type,
                                                      impute_sources = impute_sources)
  }

  # remove any indicators not requested by the user
  if(!all(indicators_expanded %in% indicators)){
    indicator_data_target_wide <- dplyr::select(indicator_data_target_wide,
                                                -tidyselect::any_of(indicators_expanded[which(!(indicators_expanded %in% indicators))]))
  }

  # pull species-level cover data for target ESG plots
  # get species lists TODO update this list's C3/C4 designations based on Travis's lit review list
  if(ann_grass_by_spp | ann_forb_by_spp | per_grass_by_spp | per_forb_by_spp |
     shrub_by_spp | subshrub_by_spp | tree_by_spp | opuntia_combined){
    species_list <- compiled_species_list

    # shrub_spp <- dplyr::filter(species_list, GrowthHabitSub=="Shrub")
    # subshrub_spp <- dplyr::filter(species_list, GrowthHabitSub=="SubShrub")
    # tree_spp <- dplyr::filter(species_list, GrowthHabitSub=="Tree")
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
    species_data_all_target_fg <- dplyr::left_join(species_data_all_target,
                                                   species_list,
                                                   by = c("SourceKey" = "SpeciesState",
                                                          "SpeciesCode" = "SpeciesCode"))

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
    if(ann_grass_by_spp){
      species_data_anngrass <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="Graminoid" & Duration=="Annual")
      species_data <- dplyr::bind_rows(species_data, species_data_anngrass)
    }
    if(ann_forb_by_spp){
      species_data_annforb <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="Forb" & Duration=="Annual")
      species_data <- dplyr::bind_rows(species_data, species_data_annforb)
    }
    if(per_grass_by_spp){
      species_data_pergrass <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="Graminoid" & Duration=="Perennial")
      species_data <- dplyr::bind_rows(species_data, species_data_pergrass)
    }
    if(per_forb_by_spp){
      species_data_perforb <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="Forb" & Duration=="Perennial")
      species_data <- dplyr::bind_rows(species_data, species_data_perforb)
    }
    if(succulent_by_spp){
      species_data_succulent <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="Succulent")
      species_data <- dplyr::bind_rows(species_data, species_data_succulent)
    }
    if(shrub_by_spp){
      species_data_shrub <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="Shrub")
      species_data <- dplyr::bind_rows(species_data, species_data_shrub)
    }
    if(subshrub_by_spp){
      species_data_subshrub <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="SubShrub")
      species_data <- dplyr::bind_rows(species_data, species_data_subshrub)
    }
    if(tree_by_spp){
      species_data_tree <- dplyr::filter(species_data_all_target_fg, GrowthHabitSub=="Tree")
      species_data <- dplyr::bind_rows(species_data, species_data_tree)
    }
    if(opuntia_combined){
      species_data_opunt <- dplyr::filter(species_data_all_target, SpeciesCode %in% Opunt_spp$SpeciesCode) %>%
        dplyr::group_by(PlotCode, SourceKey, PlotID, SiteName, PlotName, Longitude_NAD83,
                        Latitude_NAD83, Year, Month, Day) %>%
        dplyr::summarize(SpeciesCode = "AH_OpuntiaCover",
                         percent = sum(percent),
                         .groups = "drop")

      species_data <- dplyr::bind_rows(species_data, species_data_opunt)
    }

    # remove duplicates if present
    species_data <- distinct(species_data)

    # make wide
    species_data_wide <- dplyr::select(species_data, any_of(colnames(species_data_all_target))) %>%
      tidyr::pivot_wider(data = .,
                         names_from = SpeciesCode,
                         values_from = percent,
                         values_fill = 0 # fill missing values with 0 instead of NA because missing species had 0% cover
      ) %>%
      dplyr::select(-Month, -Day, -Longitude_NAD83, -Latitude_NAD83) # these can come from the indicator file

    # combine with indicator data
    indicator_data_target_wide <- dplyr::left_join(indicator_data_target_wide, species_data_wide,
                                                   by = join_by(SourceKey, PlotID, SiteName, PlotName, Year, PlotCode))

    # if a plot had none of the species groups specified, fill the missing data with 0
    indicator_data_target_wide <- indicator_data_target_wide %>%
      mutate(across(.cols = -any_of(c("SourceKey", "PlotID", "SiteName",
                                      "PlotName", "Longitude_NAD83",
                                      "Latitude_NAD83", "Year", "Month", "Day",
                                      "PlotCode")),
                    .fns = replaceNA))
  }

  # If using AH_ArtemisiaTridentataCover, remove Artemisia tridentata individual subspecies to avoid double counting them
  if(("AH_ArtemisiaTridentataCover" %in% indicators)&shrub_by_spp){
    indicator_data_target_wide <- select(indicator_data_target_wide, -any_of(c("ARTR2",
                                                                               "ARTRW8",
                                                                               "ARTRT",
                                                                               "ARTRV",
                                                                               "ARTRP4",
                                                                               "ARTRV2",
                                                                               "ARTRW",
                                                                               "ARTRS2",
                                                                               "ARTRX",
                                                                               "ARTRP2")))
  }

  return(indicator_data_target_wide)
}



