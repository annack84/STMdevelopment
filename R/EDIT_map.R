#' Create leaflet map of the target ESG
#'
#' @param target_ESG Character. Ecological Site Group to be mapped. Follow the
#'   names used in the ESG raster released with Travis's 2021 manuscript.
#' @param user Character. User name to generate input data file paths. Options
#'   are "Anna", "Travis", and "VPN".
#' @param maxZoomStyle Character. Use "public" for maps prepared for EDIT
#'   (zooming limited to match ESG resampled map resolution). Use "developer" to
#'   allow zooming in farther.
#'
#' @return Returns a leaflet map  of the target ESG, formatted for EDIT
#' @export
#'
#'

EDIT_map <- function(target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands",
                     user = "Anna",
                     maxZoomStyle = "public"){
  # read in binary raster for target ESG
  target_ESG_raster <- raster::raster(file.path(data_file_paths(user)$target_ESG_map_folder,
                                             paste0(target_ESG, ".tif")))

  # choose max zoom
  if(maxZoomStyle=="public"){maxZ <- 11}
  if(maxZoomStyle=="developer"){maxZ <- 14}

  # Create leaflet map
  map_figure <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE,
                                   minZoom = 3, maxZoom = maxZ)) %>%
    leaflet::addTiles() %>% # this chunk pulls in base maps
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
    leaflet::addRasterImage(x=target_ESG_raster, opacity=0.5,
                    colors=c("transparent", "dodgerblue"), project = FALSE,
                     maxBytes = 6 * 1024 * 1024, group = "ESG") %>%
    leaflet::fitBounds(lng1 = -114.04, lng2 = -107.02, lat1 =35.85,  lat2 = 42) %>%
    leaflet::addLayersControl(
     baseGroups = c("Topo","ESRI Aerial"),
     overlayGroups = c("ESG"),
     options = leaflet::layersControlOptions(collapsed = T))
  # leaflet::addLegend("bottomright", pal = factpal, values = levels(plot_locs$Agency),
  #             title = "Data source",
  #             #labFormat = labelFormat(prefix = "$"),
  #             opacity = 1
  #   )
  return(map_figure)
}
