#' Predictive model for annual and perennial canopy gaps > 200 cm
#'
#' This model can estimate percent cover of annual and perennial canopy gaps >
#' 200 cm using percent cover of perennial canopy gaps > 200 cm and any-hit
#' annual herbaceous cover. It was built using data from all NRI and LMF plots
#' in the Upper Colorado River Basin where both perennial gaps and annual +
#' perennial gaps were measure at the same time. Cases where annual and
#' perennial gap cover was greater than 1 standard deviation above perennial gap
#' cover were excluded because annual and perennial gap cover typically should
#' not exceed perennial gap cover, but occasionally does does due to measurement
#' errors. Adjusted R-squared: 0.8144; P-value < 0.001
#'
#' @format A linear model that predicts CA_percent_200plus (% cover of annual and perennial canopy gaps > 200 cm)
#'  \describe{
#'   \item{CP_percent_200plus}{Independent variable representing % cover of perennial canopy gaps > 200 cm}
#'   \item{AH_AnnForbGrassCover}{Independent variable representing any-hit % cover of annual herbaceous plants from LPI}
#'    }
"gap200_predictive_model"

#' Predictive model for annual and perennial canopy gaps > 100 cm
#'
#' This model can estimate percent cover of annual and perennial canopy gaps >
#' 100 cm using percent cover of perennial canopy gaps > 100 cm and any-hit
#' annual herbaceous cover. It was built using data from all NRI and LMF plots
#' in the Upper Colorado River Basin where both perennial gaps and annual +
#' perennial gaps were measure at the same time.Cases where annual and
#' perennial gap cover was greater than 1 standard deviation above perennial gap
#' cover were excluded because annual and perennial gap cover typically should
#' not exceed perennial gap cover, but occasionally does does due to measurement
#' errors. Adjusted R-squared: 0.831; P-value < 0.001
#'
#' @format A linear model that predicts CA_percent_100plus (% cover of annual and perennial canopy gaps > 100 cm)
#'  \describe{
#'   \item{CP_percent_100plus}{Independent variable representing % cover of perennial canopy gaps > 100 cm}
#'   \item{AH_AnnForbGrassCover}{Independent variable representing any-hit % cover of annual herbaceous plants from LPI}
#'    }
"gap100_predictive_model"

#' Compiled species list from AIM, LMF, NRI, NCPN and USGS internal data
#'
#' This file contains the species codes (typically derived from the USDA Plant
#' database), scientific names, and attributes of each species used for grouping
#' plants into functional types.
#'
#' @format ## `compiled_species_list`
#' A data frame with 15320 rows and 14 columns:
#' \describe{
#'   \item{SpeciesCode}{Character. Abbreviation unique to the species, typically
#'   based on the USDA Plants database (https://plants.usda.gov/home).}
#'   \item{ScientificName}{Character. Scientific name of the plant species.}
#'   \item{CommonName}{Character. Common name of the plant species, typically
#'   based on the USDA Plants database.}
#'    \item{Family}{Character. Taxonomic
#'   family of the plant species.}
#'   \item{Duration}{Character. Life span of the
#'   plant. Options are "Annual" or "Perennial". Plants with biennial growth are
#'   considered "Annual" in this data set.}
#'   \item{GrowthHabit}{Character. Plant
#'   tissue type. Options are "Woody" or "NonWoody".}
#'   \item{GrowthHabitSub}{Character. Life form of the plant. Options are
#'   "Forb", "Graminoid", "Shrub", "SubShrub", "Succulent", or "Tree".}
#'   \item{Native}{Character. Native status of the plant within the contiguous
#'   United States. Options are "Introduced" or "Native".}
#'   \item{UpdatedSpeciesCode}{Character. If the species code represents an old
#'   taxonomy or does not match USDA Plants naming conventions, the current USDA
#'   Plants code is contained here.}
#'   \item{SpeciesState}{Character. The data
#'   collection project (AIM, NRI, etc.) that the species code corresponds to.}
#'   \item{Noxious}{Character. The noxious legal status of the plant based on US
#'   federal and state government lists. Only state government lists from the
#'   western contiguous US were used. Options are "NO" (i.e. not noxious) or
#'   "YES" (i.e. noxious).}
#'   \item{PhotosyntheticPathway}{Character. Carbon
#'   fixation pathway of the plant species. Options are "C3", "C4", or "CAM".}
#'   \item{SG_Group}{Character. Sage grouse group, based on Bureau of Land
#'   Management state lists. Options are "Sagebrush", "PreferredForb",
#'   "ShortStaturePerennialGrass", "NonSagebrushShrub", or
#'   "TallStaturePerennialGrass".}
#'   \item{PinyonJuniper}{Character. Indicates
#'   whether the plant is a species of pinyon or juniper tree.}
#'   }
"compiled_species_list"
