#' Predictive model for annual and perennial canopy gaps > 200 cm
#'
#' This model can estimate percent cover of annual and perennial canopy gaps >
#' 200 cm using percent cover of perennial canopy gaps > 200 cm and any-hit
#' annual herbaceous cover. It was built using data from all NRI and LMF plots
#' in the Upper Colorado River Basin where both perennial gaps and annual +
#' perennial gaps were measure at the same time. Adjusted R-squared:  0.8462
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
#' perennial gaps were measure at the same time. Adjusted R-squared:  0.8604
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
#'   \item{SpeciesCode}{Character. Abbreviation unique to the species, typically based on the USDA Plants database (https://plants.usda.gov/home)}
#'   \item{ScientificName}{ }
#'   \item{CommonName}{ }
#'   \item{Family}{ }
#'   \item{Duration}{ }
#'   \item{GrowthHabit}{ }
#'   \item{GrowthHabitSub}{ }
#'   \item{Native}{ }
#'   \item{UpdatedSpeciesCode}{ }
#'   \item{SpeciesState}{ }
#'   \item{Noxious}{ }
#'   \item{PhotosyntheticPathway}{ }
#'   \item{SG_Group}{ }
#'   \item{PinyonJuniper}{ }
#'   }
"compiled_species_list"
