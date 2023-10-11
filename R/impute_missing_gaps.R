#' Impute missing annual & perennial canopy gaps with a linear model built by
#' Anna based on NRI and LMF data
#'
#' @param indicator_data_target_wide Data frame. Intermediate data product within \link{plot_data_pull}
#' @param impute_gap_type Character vector. Gap type to predict. Supported options are
#'   "CA_percent_100plus" (% cover annual & perennial gaps >100 cm) and
#'   "CA_percent_200plus" (% cover annual & perennial gaps >200 cm)
#' @param impute_sources Character. Optional. Only impute missing data for
#'   specific data source(s). Supported options are any source in
#'   plot_data$SourceKey
#'
#' @return Wide format data frame containing all indicators for the target ESG,
#'   including estimated annual & perennial canopy gaps for plots missing that
#'   data type.


impute_missing_gaps <- function(indicator_data_target_wide,
                                impute_gap_type = c("CA_percent_100plus", "CA_percent_200plus"),
                                impute_sources = NULL){
  # subset data to just the lines that need to be filled in
  if(is.null(impute_sources)){
    if(length(impute_gap_type)>1){
      data_to_fill <- dplyr::filter(indicator_data_target_wide, is.na(.data[[impute_gap_type[1]]])|is.na(.data[[impute_gap_type[2]]]))
    }else{
      data_to_fill <- dplyr::filter(indicator_data_target_wide, is.na(.data[[impute_gap_type]]))
    }
  }

  if(!is.null(impute_sources)){
    data_to_fill <- dplyr::filter(indicator_data_target_wide, SourceKey %in% impute_sources)
  }

  # drop plots that don't have the required predictor variables
  pred_vars <- c("AH_AnnForbGrassCover",
                  stringr::str_replace_all(impute_gap_type, "CA_", "CP_"))

  if(length(impute_gap_type)>1){
    data_to_fill_na.rm <- dplyr::filter(data_to_fill, !is.na(.data[[pred_vars[1]]]) &
                                          !is.na(.data[[pred_vars[2]]]) &
                                          !is.na(.data[[pred_vars[3]]])
                                        )
  }else{
    data_to_fill_na.rm <- dplyr::filter(data_to_fill, !is.na(.data[[pred_vars[1]]]) &
                                          !is.na(.data[[pred_vars[2]]])
                                        )
  }

  data_sources <- unique(data_to_fill_na.rm$SourceKey)

  # make predictions
  data_imputed <- data_to_fill_na.rm
  if("CA_percent_200plus" %in% impute_gap_type){
    data_imputed$CA_percent_200plus <- predict(gap200_predictive_model, data_to_fill_na.rm)
    # negative predicted values get changed to 0 - can't have negative gaps
    data_imputed <- mutate(data_imputed,
                           CA_percent_200plus = ifelse(test = CA_percent_200plus < 0,
                                                       yes = 0,
                                                       no = CA_percent_200plus))
  }
  if("CA_percent_100plus" %in% impute_gap_type){
    data_imputed$CA_percent_100plus <- predict(gap100_predictive_model, data_to_fill_na.rm)
    # negative predicted values get changed to 0 - can't have negative gaps
    data_imputed <- mutate(data_imputed,
                           CA_percent_100plus = ifelse(test = CA_percent_100plus < 0,
                                                       yes = 0,
                                                       no = CA_percent_100plus))
  }

  # replace the values in the input data frame with the predicted values
  keep_original_rows <- dplyr::anti_join(x = indicator_data_target_wide,
                                         y = data_imputed,
                                         by = c("SourceKey", "PlotID", "SiteName",
                                                "PlotName", "Year", "PlotCode"))

  output_data <- dplyr::bind_rows(keep_original_rows, data_imputed)

  message(paste(paste(impute_gap_type, collapse = ", "),
                "imputed from perennial gaps and annual herbaceous cover for",
                nrow(data_imputed),
                "plots from these projects:",
                paste(data_sources, collapse = ", ")))

  return(output_data)
}

