#' Compile species lists used to calculated grouped indicators
#'
#' @param user Character. User name to generate input data file paths, passed to \link[STMdevelopment]{data_file_paths}
#' @param output_path Character. File path to the folder to saved the compiled species list in.
#'
#' @return Data frame containing the combined species list
#' @export


compile_species_lists <- function(user = "Anna",
                                  output_path = "C:/Users/aknight/Desktop/Telework_Backups/V_drive/PROJECTS/ESG/Maps/Eco_Site_Group_Data"
                                  ){
  # read in the species lists that were used to calculated the grouped indicators
  plotnet_splist <- list.files(data_file_paths(user = user)$plotnet_splist_location, full.names = T) %>%
    grep(pattern = "SpeciesFileMaster_", x = ., value = T) %>%
    read.csv(., na.strings = c("NA", ""))

  aimlmf_splist <- read.csv(data_file_paths(user = user)$aimlmf_splist,
                            na.strings = c("NA", ""))

  # get the same columns for each list
  plotnet_splist_slim <- select(plotnet_splist, any_of(colnames(aimlmf_splist)))

  aimlmf_splist_slim <- select(aimlmf_splist, any_of(colnames(plotnet_splist_slim)))

  # combine the lists, creating separate SpeciesState entries for AIM vs LMF so
  # that they can be matched to the SourceKey column later on

  full_splist <- bind_rows(plotnet_splist_slim,
                           mutate(aimlmf_splist_slim,
                                  SpeciesState = "AIM")) %>%
    bind_rows(.,
              mutate(aimlmf_splist_slim,
                     SpeciesState = "LMF")) %>%
    distinct()

  #save it
  write.csv(full_splist,
            file = paste0(output_path, "/SpeciesList_PlotNet_compiled_", Sys.Date(), ".csv"),
            row.names = F)

  return(full_splist)
}

