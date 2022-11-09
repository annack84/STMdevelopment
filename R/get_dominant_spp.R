#' Get dominant species by functional group for each plant community
#'
#' Get a list of the species in each functional group for each plant community,
#' ranked by mean cover across all plots in the plant community. Species must
#' appear in at least one plot to be returned.
#'
#' @param data Data frame containing all the species to be tallied. Format
#'   should match the outputs of \code{\link{plot_data_pull}}. Filter the input
#'   data frame to the plant community of interest to get dominant species by
#'   community.
#' @param species_cols Character vector of the column names of all the columns
#'   in data containing species cover.
#' @param user Character. User name to generate input data file paths. Options
#'   are "Anna", "Travis", and "VPN". Passes to \code{\link{data_file_paths}}
#'
#' @return Data frame of species recorded for each functional group, arranged
#'   from highest to lowest mean cover. Functional groups are:
#'   C3NativePerenGrass, C3IntroducedPerenGrass, C4NativePerenGrass,
#'   C4IntroducedPerenGrass, NativeAnnGrass, IntroducedAnnGrass,
#'   NativePerenForb, IntroducedPerenForb, NativeAnnForb, IntroducedAnnForb,
#'   Succulent, ShrubSubShrub, Tree
#' @export
#'

get_dominant_spp <- function(data= filter(plot_data_descriptive, PlantCommunity_fuzzy==3),
                             species_cols = colnames(descriptive.df),
                             user = "Anna"
                             ){
  file_paths <- data_file_paths(user = user)

  spp_list_aim <- filter(read.csv(file_paths$species_list), SpeciesState=="AIM")

  species_fg_by_group <- data %>%
    tidyr::pivot_longer(cols = any_of(species_cols),
                        names_to = "SpeciesCode",
                        values_to = "PctCover") %>%
    group_by(SpeciesCode) %>%
    summarise(MeanCover = mean(PctCover, na.rm=T),
              MaxCover = max(PctCover, na.rm = T),
              pct_plots = (sum(PctCover > 0, na.rm = TRUE)/n())*100,
              .groups = "drop") %>%
    left_join(., spp_list_aim, by="SpeciesCode") %>%
    filter(MeanCover > 0) %>%
    arrange(desc(MeanCover))

  dom_C3NativePerenGrass <- species_fg_by_group %>%
    filter(PhotosyntheticPathway=="C3" & Native=="Native" & Duration == "Perennial" & GrowthHabitSub=="Graminoid") %>%
    mutate(Indicator_code = "AH_C3NativePerenGrassCover",
           Indicator = "C3 native perennial grasses")

  dom_C3IntroducedPerenGrass <- species_fg_by_group %>%
    filter(PhotosyntheticPathway=="C3" & Native=="Introduced" & Duration == "Perennial" & GrowthHabitSub=="Graminoid") %>%
    mutate(Indicator_code = "AH_C3IntroducedPerenGrassCover",
           Indicator = "C3 introduced perennial grasses")

  dom_C4NativePerenGrass <- species_fg_by_group %>%
    filter(PhotosyntheticPathway=="C4" & Native=="Native" & Duration == "Perennial" & GrowthHabitSub=="Graminoid") %>%
    mutate(Indicator_code = "AH_C4NativePerenGrassCover",
           Indicator = "C4 native perennial grasses")

  dom_C4IntroducedPerenGrass <- species_fg_by_group %>%
    filter(PhotosyntheticPathway=="C4" & Native=="Introduced" & Duration == "Perennial" & GrowthHabitSub=="Graminoid") %>%
    mutate(Indicator_code = "AH_C4IntroducedPerenGrassCover",
           Indicator = "C4 introduced perennial grasses")

  dom_NativeAnnGrass <- species_fg_by_group %>%
    filter(Native=="Native" & Duration == "Annual" & GrowthHabitSub=="Graminoid") %>%
    mutate(Indicator_code = "AH_NativeAnnGrassCover",
           Indicator = "Native annual grasses")

  dom_IntroducedAnnGrass <- species_fg_by_group %>%
    filter(Native=="Introduced" & Duration == "Annual" & GrowthHabitSub=="Graminoid") %>%
    mutate(Indicator_code = "AH_IntroducedAnnGrassCover",
           Indicator = "Introduced annual grasses")

  dom_NativePerenForb <- species_fg_by_group %>%
    filter(Native=="Native" & Duration == "Perennial" & GrowthHabitSub=="Forb") %>%
    mutate(Indicator_code = "AH_NativePerenForbCover",
           Indicator = "Native perennial forbs")

  dom_IntroducedPerenForb <- species_fg_by_group %>%
    filter(Native=="Introduced" & Duration == "Perennial" & GrowthHabitSub=="Forb") %>%
    mutate(Indicator_code = "AH_IntroducedPerenForbCover",
           Indicator = "Introduced perennial forbs")

  dom_NativeAnnForb <- species_fg_by_group %>%
    filter(Native=="Native" & Duration == "Annual" & GrowthHabitSub=="Forb") %>%
    mutate(Indicator_code = "AH_NativeAnnForbCover",
           Indicator = "Native annual forbs")

  dom_IntroducedAnnForb <- species_fg_by_group %>%
    filter(Native=="Introduced" & Duration == "Annual" & GrowthHabitSub=="Forb") %>%
    mutate(Indicator_code = "AH_IntroducedAnnForbCover",
           Indicator = "Introduced annual forbs")

  dom_Succulent <- species_fg_by_group %>%
    filter(GrowthHabitSub=="Succulent") %>%
    mutate(Indicator_code = "AH_SucculentCover",
           Indicator = "Succulents")

  dom_ShrubSubShrub <- species_fg_by_group %>%
    filter(GrowthHabitSub=="Shrub" | GrowthHabitSub=="SubShrub") %>%
    mutate(Indicator_code = NA,
           Indicator = "Shrubs")

  dom_Tree <- species_fg_by_group %>%
    filter(GrowthHabitSub=="Tree") %>%
    mutate(Indicator_code = "AH_TreeCover",
           Indicator = "Trees")

  dom_spp <- bind_rows(dom_C3NativePerenGrass, dom_C3IntroducedPerenGrass,
                            dom_C4NativePerenGrass, dom_C4IntroducedPerenGrass,
                            dom_NativeAnnGrass, dom_IntroducedAnnGrass,
                            dom_NativePerenForb, dom_IntroducedPerenForb,
                            dom_NativeAnnForb, dom_IntroducedAnnForb,
                            dom_Succulent, dom_ShrubSubShrub, dom_Tree)
  return(dom_spp)
}
