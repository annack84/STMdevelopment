## code to prepare `compiled_species_list` dataset goes here

user <- "Anna"

# read in the species lists that were used to calculated the grouped indicators
plotnet_splist <- list.files(data_file_paths(user = user)$plotnet_splist_location, full.names = T) %>%
  grep(pattern = "SpeciesFileMaster_", x = ., value = T) %>%
  read.csv(., na.strings = c("NA", ""))

aimlmf_splist <- read.csv(data_file_paths(user = user)$aimlmf_splist,
                          na.strings = c("NA", ""))

nri_splist <- read.csv(file.path(dirname(data_file_paths(user = user)$nri), "SpeciesList_NRI_UCRB_clean.csv"),
                       na.strings = c("NA", ""))

# get the same columns for each list
plotnet_splist_slim <- dplyr::select(plotnet_splist, dplyr::any_of(colnames(aimlmf_splist)))

aimlmf_splist_slim <- dplyr::select(aimlmf_splist, dplyr::any_of(colnames(plotnet_splist_slim)))

nri_splist_slim <- dplyr::select(nri_splist, dplyr::any_of(colnames(aimlmf_splist_slim)))

# combine the lists, creating separate SpeciesState entries for AIM vs LMF so
# that they can be matched to the SourceKey column later on

compiled_species_list <- dplyr::bind_rows(plotnet_splist_slim,
                                dplyr::mutate(aimlmf_splist_slim,
                                              SpeciesState = "AIM")) %>%
  dplyr::bind_rows(.,
                   dplyr::mutate(aimlmf_splist_slim,
                                 SpeciesState = "LMF")) %>%
  dplyr::bind_rows(.,
                   nri_splist_slim) %>%
  dplyr::distinct()

# Save it!
usethis::use_data(compiled_species_list, overwrite = TRUE)
