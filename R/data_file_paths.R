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
                        ESG_map = NA,
                        apriori_stms = NA)

  if(user=="Anna"){
    file_list <- network_files
    # update paths specific to your computer here
    file_list$plotnet_processed <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/SWDesertRangelandMonitoring/Data/ProcessedData"
    #file_list$nri <- "C:/Users/aknight/Desktop/Telework_Backups/U_drive/Projects/NRI_US_limitaccess"
    file_list$ESG_map <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/ESG/Maps/Eco_Site_Group_Data/ESGs_final.tif"
    file_list$apriori_stms <- "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/ESG/Maps/Eco_Site_Group_Data/S5_ESG_STM_summary_formatted.csv"
  }

  if(user=="Travis"){
    file_list <- network_files
    # update paths specific to your computer here
  }

  if(user=="VPN"){ # if using VPN to connect to the network, don't change any file paths
    file_list <- network_files
  }
  return(file_list)
}
