# pull and format new plot data for state map validation
# created 11 Dec 2023
# Anna Knight, aknight@usgs.gov

# load packages
library(dplyr)
library(sf)
# use one of the options below to load the STMdevelopment package, depending on
# whether you're working inside the package or not
#library(STMdevelopment)
devtools::load_all()

#target_ESG <- "Semiarid_Warm_SandyUplands_LoamyUplands"
target_ESG <- "Semiarid_Warm_Shallow_DeepRocky"
user <- "Anna"

# pull plot data using the same settings as the target ESG
# by functional group cover
indicators <- c("AH_C3NativePerenGrassCover",
                "AH_C3IntroducedPerenGrassCover",
                "AH_C4NativePerenGrassCover",
                "AH_C4IntroducedPerenGrassCover",
                "AH_NativePerenForbCover", # Native perennial forbs
                "AH_IntroducedPerenForbCover", # Non-native perennial forbs
                "AH_NativeAnnGrassCover", # Native annual grasses
                "AH_IntroducedAnnGrassCover", # Non-native annual grasses
                "AH_NativeAnnForbCover", # Native annual forbs
                "AH_IntroducedAnnForbCover", # Non-native annual forbs
                "AH_ArtemisiaTridentataCover", # All big sagebrush species lumped together
                "BareSoilCover", # Bare soil
                "FH_TotalLitterCover", # Litter
                #"CA_percent_100plus", # Canopy gaps > 100 cm
                #"CA_percent_200plus",
                "FH_LichenCover", # Lichen + moss combined cover
                "FH_MossCover"#,
                #"SoilStab_all" # to represent cyano abundance... I think this is only marginally a community variable. If used in hierarchical clustering and Bray-Curtis NMDS, will need to standardize it to make units more similar to % cover units
)

ann_grass_by_spp <- F
ann_forb_by_spp <- F
per_grass_by_spp <- F
per_forb_by_spp <- F
succulent_by_spp <- F
shrub_by_spp <- T # All shrubs and sub-shrubs by species
subshrub_by_spp <- T
tree_by_spp <- T # All trees by species
opuntia_combined <- T

data_sources <- c(#"BadgerWash",
  #"CRCMonitoring", # drop if not needed for spatial representation
  "IM_NCPN", # exclude until other have permission to use NCPN locations
  #"LMF",
  "NRI"#, # exclude until others have permission to use NRI
  #"Parashant", # drop if not needed for spatial representation
  #"JFSP",
  #"AIM"#,
  #"VanScoyocThesis" # drop if not needed for spatial representation
)

impute_gap_type <- NULL

plot_data <- plot_data_pull(user=user,
                            target_ESG = target_ESG,
                            data_sources = data_sources,
                            indicators = indicators,
                            ann_grass_by_spp = ann_grass_by_spp,
                            ann_forb_by_spp = ann_forb_by_spp,
                            per_grass_by_spp = per_grass_by_spp,
                            per_forb_by_spp = per_forb_by_spp,
                            succulent_by_spp = succulent_by_spp,
                            shrub_by_spp = shrub_by_spp,
                            subshrub_by_spp = subshrub_by_spp,
                            tree_by_spp = tree_by_spp,
                            opuntia_combined = opuntia_combined#,
                            #impute_gap_type = impute_gap_type
) %>%
  filter(!is.na(Month))

# remove incomplete rows - ordination won't work with NAs
plot_data_clean <- na.omit(plot_data) %>%
  # NRI has a separate plot code for each time a plot was sampled, but all other data sets have the same plot
  # code for all sampling times. Make a similar code for NRI.
  mutate(PlotCode_NoYear = ifelse(test = SourceKey == "NRI_UCRB",
                                  yes = paste0(SourceKey, "_",
                                               SiteName, "_",
                                               substr(PlotID, 5, nchar(PlotID))),
                                  no = PlotCode))

# Get only the plot-dates NOT included in the original clustering
if(target_ESG=="Semiarid_Warm_SandyUplands_LoamyUplands"){
  cluster_data <- read.csv("C:/Users/aknight/Documents/Telework_Backups/V_drive/ANNA_KNIGHT/ESG/STM/Data/Plot_data_used_Duniway_et_al_2024/Analysis_ready_data/Semiarid_Warm_SandyUplands_LoamyUplands_FANNYInputData_2023-12-11.csv")

  plot_data_new <- anti_join(x=plot_data_clean,
                             y = select(cluster_data, SourceKey, PlotCode, Year, #Month, Day
                                        ))
}

if(target_ESG=="Semiarid_Warm_Shallow_DeepRocky"){
  cluster_data <- read.csv("C:/Users/aknight/Documents/Telework_Backups/V_drive/ANNA_KNIGHT/ESG/STM/Data/Analysis_ready_data/Semiarid_Warm_Shallow_DeepRocky_FANNYInputData_2023-11-17.csv")

  plot_data_new <- anti_join(x=plot_data_clean,
                             y = select(cluster_data, SourceKey, PlotCode, Year)) %>%
    filter(PlotCode != "AIM_Utah Vernal FO 2019_009") # this one is duplicated in the plot locations with very different coordinates
}

# filter to just the needed plots
#plot_data_new <- filter(plot_data_clean, SourceKey=="NRI_UCRB")
#plot_data_new <- filter(plot_data_new, SourceKey=="JFSP")

# check for duplicate plot-years
plot_data_new %>% group_by(PlotCode) %>% filter(n()>1) %>% ungroup() %>% nrow() # should be 0

# should mostly be recent years, but there might be some older plots that were
# either repeat samples of the same plot or excluded from clustering due to low
# SGU probability
hist(plot_data_new$Year, breaks = 2000:2023)

# add in SGU probability (i.e. certainty of prediction)
file_paths <- data_file_paths(user)
SGU_prob_raster <- raster::raster(file_paths$sgu_probability_raster)

plot_locations <- sf::st_read(dsn = file.path(file_paths$plotnet_processed, "PlotLocations"),
                              layer = "all_plot-years_2024-02-15",
                              quiet=TRUE) %>%
  distinct()

# shouldn't be using NRI since location info must be shared
# if("NRI" %in% data_sources){
#   nri_locations <- sf::st_read(dsn = file_paths$nri,
#                                layer = "NRI_UCRB_plot-years_2022-11-03",
#                                quiet = TRUE)
#
#   plot_locations <- dplyr::filter(plot_locations, !grepl(pattern = "^NRI_",
#                                                          x=PlotCode)) %>%
#     dplyr::bind_rows(., nri_locations) %>%
#     distinct()
# }

plot_SGU_prob <- sf::st_as_sf(raster::extract(x = SGU_prob_raster,
                                              y = plot_locations,
                                              sp = T))

plot_data_new <- left_join(plot_data_new, select(plot_SGU_prob, PlotCode, UCRB_SGUs_ProbMax)) %>%
  distinct()

#certainty_cutoff <- quantile(plot_data_first$UCRB_SGUs_ProbMax, probs = 0.1)

#plot_data_first <- filter(plot_data_first, UCRB_SGUs_ProbMax > certainty_cutoff)

# save the new data
# double check whether to include location info!!
ord.df.share <- select(plot_data_new,
                       any_of(c("Longitude_NAD83", "Latitude_NAD83",
                                colnames(cluster_data))))

# don't need OPUNT data because we have AH_OpuntiaCover
if("OPUNT" %in% colnames(ord.df.share)){
  ord.df.share <- select(ord.df.share, -OPUNT)
}

if("NRI" %in% data_sources){
  output_path <- paste0("C:/Users/aknight/Documents/Telework_Backups/V_drive/ANNA_KNIGHT/ESG/STM/Data/Analysis_ready_data/contains_NRI_do_not_sync/",
         target_ESG, "_NRI_", Sys.Date(), ".csv")
}else{
  output_path <- paste0("C:/Users/aknight/Documents/Telework_Backups/V_drive/ANNA_KNIGHT/ESG/STM/Data/Analysis_ready_data/",
         target_ESG, "_JFSP2023_", Sys.Date(), ".csv")
}

write.csv(ord.df.share,
          file = output_path,
          row.names = F)
