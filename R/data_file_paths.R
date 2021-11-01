#' Set file paths for specific users' working environments
#'
#' The purpose of this function is to make it easy to run the analyses from
#'  different users working environments. Add new list items as needed to access
#'  other data sets.
#'
#' @param user Character. Options are "Anna" and "Travis" for personal hard
#'  drive and OneDrive folders/files, or VPN if doing everything from the
#'  network
#'
#' @return List. Use this list in other functions to access data from different
#'  file locations
#' @export

data_file_paths <- function(user){ # "Anna", "Travis", or "VPN"
  network_files <- list(plotnet_processed = "V:/PROJECTS/MIKE_DUNIWAY/CURRENT/SWDesertRangelandMonitoring/Data/ProcessedData",
                        nri = "V:/PROJECTS/MIKE_DUNIWAY/CURRENT/SWDesertRangelandMonitoring/Data/ProcessedData/NRI",
                        ESG_map = "V:/PROJECTS/TRAVIS_NAUMAN/ESGs/Analysis/RMD_STMs/ESGs_final.tif",
                        target_ESG_map_folder = "V:/PROJECTS/TRAVIS_NAUMAN/ESGs/Analysis/RMD_STMs/leaflet_map_ESG_rasters",
                        apriori_stms = "V:/PROJECTS/TRAVIS_NAUMAN/ESGs/Analysis/RMD_STMs/S5_ESG_STM_summary_forRMD.csv",
                        species_list = "V:/PROJECTS/TRAVIS_NAUMAN/ESGs/Analysis/RMD_STMs/SpeciesList_WesternUS_AcceptedSymbols_2020-01-06.csv", # update to match Anna's file
                        prodfolder = "V:/PROJECTS/TRAVIS_NAUMAN/ESGs/Data_Nonspatial/Duniway_EDIT_qry",
                        climate = "V:/PROJECTS/TRAVIS_NAUMAN/ESGs/Data_Spatial/shps_SSURGO18",
                        ssurgo = "V:/PROJECTS/TRAVIS_NAUMAN/GIS_Archive/gSSURGO18/gSSURGO_CONUS.gdb",
                        ssurgo_result_fldr = "V:/PROJECTS/TRAVIS_NAUMAN/ESGs/Analysis/RMD_STMs/ssurgo_results",
                        plotnet_splist_location = "V:/PROJECTS/MIKE_DUNIWAY/CURRENT/SWDesertRangelandMonitoring/Data",
                        aimlmf_splist = "V:/PROJECTS/MIKE_DUNIWAY/CURRENT/SWDesertRangelandMonitoring/Data/ProcessedData/AIM/Terrestrial AIM Data Spring 2020/PlotNet_style_AIMLMF_calcs/SpeciesList_AIM2020UCRBStates.csv"
                        )

  if(user=="Anna"){
    file_list <- network_files
    # update paths specific to your computer here
    file_list$plotnet_processed <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/SWDesertRangelandMonitoring/Data/ProcessedData"
    file_list$ESG_map <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/ESG/Maps/Eco_Site_Group_Data/ESGs_final.tif"
    file_list$target_ESG_map_folder <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/ESG/Maps/Eco_Site_Group_Data"
    file_list$species_list <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/ESG/Maps/Eco_Site_Group_Data/SpeciesList_PlotNet_compiled_2021-11-01.csv"
    file_list$apriori_stms <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/ESG/Maps/Eco_Site_Group_Data/S5_ESG_STM_summary_formatted.csv"
    file_list$plotnet_splist_location <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/SWDesertRangelandMonitoring/Data"
    file_list$aimlmf_splist <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/SWDesertRangelandMonitoring/Data/ProcessedData/AIM/Terrestrial AIM Data Spring 2020/PlotNet_style_AIMLMF_calcs/SpeciesList_AIM2020UCRBStates.csv"
    file_list$nri <- "C:/Users/aknight/Desktop/Telework_Backups/U_drive/Projects/NRI_US_limitaccess/NRI/NRI_USGS_formats/NRI_UCRB/Indicators"
  }

  if(user=="Travis"){
    file_list <- network_files
    # update paths specific to your computer here
    file_list$ESG_map <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/ESG_UCRB_manuscript/datarelease/ESGs_final.tif"
    file_list$prodfolder <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/EDIT_prod"
    file_list$climate <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/shps_SSURGO18"
    file_list$ssurgo <- "C:/models/gSSURGO18/gSSURGO_CONUS.gdb"
    file_list$ssurgo_result_fldr <- "C:/Users/tnauman/OneDrive - DOI/USGS/BLM_projects/BLM_CO_ESGs/ESGs_UCRB/climateOptimize/ssurgo_results"
  }

  if(user=="VPN"){ # if using VPN to connect to the network, don't change any file paths
    file_list <- network_files
  }
  return(file_list)
}
