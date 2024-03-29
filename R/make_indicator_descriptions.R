#' Create a table describing the plot indicators
#'
#' @param indicators Character vector. Functional group indicators used to pull plot data
#' @param ann_grass_by_spp Logical. Include all annual grasses by species?
#' @param ann_forb_by_spp Logical. Include all annual forbs by species?
#' @param per_grass_by_spp Logical. Include all perennial grasses by species?
#' @param per_forb_by_spp Logical. Include all perennial forbs by species?
#' @param succulent_by_spp Logical. Include all succulents by species? If TRUE, opuntia_combined should be FALSE
#' @param shrub_by_spp Logical. Include all shrubs by species?
#' @param subshrub_by_spp Logical. Include all sub-shrubs by species?
#' @param tree_by_spp Logical. Include all trees by species?
#' @param opuntia_combined Logical. Include combined cover of genus Opuntia? If TRUE, succulent_by_spp should be FALSE
#'
#' @return Data frame describing each indicator and set of indicators
#' @export
#'

make_indicator_descriptions <- function(indicators = c("AH_C3NativePerenGrassCover",
                                                       "AH_C3IntroducedPerenGrassCover",
                                                       "AH_C4NativePerenGrassCover",
                                                       "AH_C4IntroducedPerenGrassCover",
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
                                        ann_grass_by_spp = FALSE,
                                        ann_forb_by_spp = FALSE,
                                        per_grass_by_spp = FALSE,
                                        per_forb_by_spp = FALSE,
                                        succulent_by_spp = FALSE,
                                        shrub_by_spp = T, # All shrubs and sub-shrubs by species
                                        subshrub_by_spp = T,
                                        tree_by_spp = T, # All trees by species
                                        opuntia_combined = T){

  # Create a table of indicator descriptions
  indicator_description_table_options <- dplyr::tribble(
    ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
    "AH_C3NativePerenGrassCover", "Biotic", "C3 native perennial grasses", "Percent foliar cover of the functional group",
    "AH_C3IntroducedPerenGrassCover", "Biotic", "C3 introduced perennial grasses", "Percent foliar cover of the functional group",
    "AH_C4NativePerenGrassCover", "Biotic", "C4 native perennial grasses", "Percent foliar cover of the functional group",
    "AH_C4IntroducedPerenGrassCover","Biotic", "C4 introduced perennial grasses", "Percent foliar cover of the functional group",
    "AH_C3PerenGrassCover", "Biotic", "C3 perennial grasses", "Percent foliar cover of the functional group",
    "AH_C4PerenGrassCover", "Biotic", "C4 perennial grasses", "Percent foliar cover of the functional group",
    "AH_IntroducedPerenGrassCover", "Biotic", "Introduced perennial grasses", "Percent foliar cover of the functional group",
    "AH_NativeAnnGrassCover", "Biotic", "Native annual grasses", "Percent foliar cover of the functional group",
    "AH_IntroducedAnnGrassCover", "Biotic", "Introduced annual grasses", "Percent foliar cover of the functional group",
    "AH_NativePerenForbCover", "Biotic", "Native perennial forbs", "Percent foliar cover of the functional group",
    "AH_IntroducedPerenForbCover", "Biotic", "Introduced perennial forbs", "Percent foliar cover of the functional group",
    "AH_NativeAnnForbCover", "Biotic", "Native annual forbs", "Percent foliar cover of the functional group",
    "AH_IntroducedAnnForbCover", "Biotic", "Introduced annual forbs", "Percent foliar cover of the functional group",
    "AH_ArtemisiaTridentataCover", "Biotic", "Artemisia tridentata (all subspecies)", "Percent foliar cover of A. tridentata",
    "BareSoilCover", "Ecosystem structure", "Bare soil", "Percent cover of bare soil",
    "FH_TotalLitterCover", "Ecosystem structure", "Litter", "Percent cover of plant litter",
    "SoilStab_all", "Ecosystem structure", "Soil stability (all cover types)", "Ordinal soil stability rating (1-6), averaged across all plot samples",
    "CP_percent_100plus", "Ecosystem structure", "Perennial canopy gaps > 100 cm", "Percent of transect lengths with perennial canopy gaps > 100 cm, where only perennial plants stop a gap",
    "CP_percent_200plus", "Ecosystem structure", "Perennial canopy gaps > 200 cm", "Percent of transect lengths with perennial canopy gaps > 200 cm, where only perennial plants stop a gap",
    "CA_percent_100plus", "Ecosystem structure", "Ann. & peren. canopy gaps > 100 cm", "Percent of transect lengths with canopy gaps > 100 cm, where annual and perennial plants stop a gap",
    "CA_percent_200plus", "Ecosystem structure", "Ann. & peren. canopy gaps > 200 cm", "Percent of transect lengths with canopy gaps > 200 cm, where annual and perennial plants stop a gap"
    )

  # filter to only include the indicators used
  indicator_description_table <- dplyr::filter(indicator_description_table_options, Indicator_code %in% indicators)

  # Add on canopy gaps if needed
  # if("CP_percent_100plus" %in% indicators){
  #   canopy_100plus_table <- dplyr::tribble(
  #     ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
  #     "CP_percent_100plus", "Ecosystem structure", "Perennial canopy gaps > 100 cm", "Percent of transect lengths with perennial canopy gaps > 100 cm"
  #   )
  #   indicator_description_table <- dplyr::bind_rows(indicator_description_table, canopy_100plus_table)
  # }
  # if("CP_percent_200plus" %in% indicators){
  #   canopy_200plus_table <- dplyr::tribble(
  #     ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
  #     "CP_percent_200plus", "Ecosystem structure", "Perennial canopy gaps > 200 cm", "Percent of transect lengths with perennial canopy gaps > 200 cm"
  #   )
  #   indicator_description_table <- dplyr::bind_rows(indicator_description_table, canopy_200plus_table)
  # }

  # Add on lichen and moss cover if needed
  if("FH_LichenCover" %in% indicators & "FH_MossCover" %in% indicators){
    lichenmoss_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "FH_LichenMossCover", "Biotic", "Lichens and mosses", "Percent cover of lichens and mosses with no plant canopy above"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, lichenmoss_table)
  }

  # Add species-level indicators
  if(ann_grass_by_spp){
    ann_grass_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "ann_grass_by_spp", "Biotic", "Annual grass species", "Percent foliar cover of each annual grass species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, ann_grass_table)
  }

  if(ann_forb_by_spp){
    ann_forb_by_spp <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "ann_forb_by_spp", "Biotic", "Annual forb species", "Percent foliar cover of each annual forb species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, ann_forb_by_spp)
  }

  if(per_grass_by_spp){
    per_grass_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "per_grass_by_spp", "Biotic", "Perennial grass species", "Percent foliar cover of each perennial grass species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, per_grass_table)
  }

  if(per_forb_by_spp){
    per_forb_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "per_forb_by_spp", "Biotic", "Perennial forb species", "Percent foliar cover of each perennial forb species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, per_forb_table)
  }

  if(succulent_by_spp){
    succulent_table <- dplyr::tribble(
      ~Indicator_code, ~Indicator_type, ~Indicator, ~Description,
      "succulent_by_spp", "Biotic", "Succulent species", "Percent foliar cover of each succulent species"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, succulent_table)
  }

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
      "opuntia_combined", "Biotic", "Opuntia spp.", "Percent foliar cover of the genus Opuntia"
    )
    indicator_description_table <- dplyr::bind_rows(indicator_description_table, opuntia_table)
  }

  indicator_description_table <- indicator_description_table[order(indicator_description_table$Indicator_type),]
  colnames(indicator_description_table) <- c("Indicator_code", "Indicator type", "Indicator", "Description")

  return(indicator_description_table)
}
