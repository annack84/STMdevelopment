#' Create a table describing the plot indicators
#'
#' @param indicators Character vector. Functional group indicators used to pull plot data
#' @param shrub_by_spp Logical. Include all shrubs by species?
#' @param subshrub_by_spp Logical. Include all sub-shrubs by species?
#' @param tree_by_spp Logical. Include all trees by species?
#' @param opuntia_combined Logical. Include combined cover of genus Opuntia?
#'
#' @return Data frame describing each indicator and set of indicators
#' @export
#'

make_indicator_descriptions <- function(indicators = c("AH_C3PerenGrassCover", # C3 native perennial grasses TODO calculate C3 NATIVE
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
                                        opuntia_combined = T){

  # Create a table of indicator descriptions
  indicator_description_table_options <- dplyr::tribble(
    ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
    "AH_C3PerenGrassCover", "Biotic", "C3 perennial grasses", "Percent foliar cover of the functional group",
    "AH_C4PerenGrassCover", "Biotic", "C4 perennial grasses", "Percent foliar cover of the functional group",
    "AH_IntroducedPerenGrassCover", "Biotic", "Introduced perennial grasses", "Percent foliar cover of the functional group",
    "AH_NativeAnnGrassCover", "Biotic", "Native annual grasses", "Percent foliar cover of the functional group",
    "AH_IntroducedAnnGrassCover", "Biotic", "Introduced annual grasses", "Percent foliar cover of the functional group",
    "AH_NativePerenForbCover", "Biotic", "Native perennial forbs", "Percent foliar cover of the functional group",
    "AH_IntroducedPerenForbCover", "Biotic", "Introduced perennial forbs", "Percent foliar cover of the functional group",
    "AH_NativeAnnForbCover", "Biotic", "Native annual forbs", "Percent foliar cover of the functional group",
    "AH_IntroducedAnnForbCover", "Biotic", "Introduced annual forbs", "Percent foliar cover of the functional group",
    "BareSoilCover", "Ecosystem structure", "Bare soil", "Percent cover of bare soil"
    )

  # filter to only include the indicators used
  indicator_description_table <- dplyr::filter(indicator_description_table_options, Indicator_code %in% indicators)

  # Add on canopy gaps if needed
  if("CP_percent_100to200" %in% indicators & "CP_percent_200plus" %in% indicators){
    canopy_100plus_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "CP_percent_100plus", "Ecosystem structure", "Perennial canopy gaps > 100 cm", "Percent of transect lengths with perennial canopy gaps > 100 cm"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, canopy_100plus_table)
  }

  # Add on lichen and moss cover if needed
  if("FH_LichenCover" %in% indicators & "FH_MossCover" %in% indicators){
    lichenmoss_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "FH_LichenMossCover", "Biotic", "Lichens and mosses", "Percent cover of lichens and mosses with no plant canopy above"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, lichenmoss_table)
  }

  # Add species-level indicators
  if(shrub_by_spp){
    shrub_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "shrub_by_spp", "Biotic", "Shrub species", "Percent foliar cover of each shrub species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, shrub_table)
  }

  if(subshrub_by_spp){
    subshrub_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "subshrub_by_spp", "Biotic", "Sub-shrub species", "Percent foliar cover of each sub-shrub species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, subshrub_table)
  }

  if(tree_by_spp){
    tree_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "tree_by_spp", "Biotic", "Tree species", "Percent foliar cover of each tree species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, tree_table)
  }

  if(opuntia_combined){
    opuntia_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "opuntia_combined", "Biotic", "Opuntia genus", "Percent foliar cover of the genus Opuntia"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, shrub_table)
  }

  return(indicator_description_table)
}
