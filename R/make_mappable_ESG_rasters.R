#' Create a binary raster for each ESG
#'
#' @param target_ESG Character. Ecological Site Group to be mapped.
#'   Follow the names used in the ESG raster released with Travis's 2021 manuscript.
#' @param user Character. User name to generate input data file paths.
#'   Options are "Anna", "Travis", and "VPN".
#'
#' @return Raster where 1=target ESG and 0=not target ESG
#' @export

make_mappable_raster <- function(target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands",
                                 user = "Anna"){
  all_ESGs <- raster::raster(data_file_paths(user)$ESG_map)

  ESG_number <-ESG_table$ESGid[which(ESG_table$ESGs_text==target_ESG)]

  reclass_matrix <- as.matrix(data.frame(is = c(which(1:36 != ESG_number), ESG_number, 128, NA),
                                         becomes = c(rep(0, length(which(1:36 != ESG_number))), 1, 0, 0)))

  target_ESG_raster <- raster::reclassify(x=all_ESGs,
                                          rcl = reclass_matrix)


  target_ESG_raster <- raster::aggregate(x=target_ESG_raster, fact=3, fun=raster_mode,
                                         na.rm=T)

  target_ESG_raster_proj <- raster::projectRaster(from = target_ESG_raster,
                      #to = raster::projectExtent(target_ESG_raster, crs = sp::CRS("+init=epsg:3857")),
                      crs = sp::CRS("+init=epsg:3857"),
                      method = "ngb",
                      filename = file.path(data_file_paths(user)$target_ESG_map_folder,
                                           paste0(target_ESG, ".tif")),
                      format = "GTiff",
                      datatype = "INT1U",
                      overwrite = T)
  return(target_ESG_raster_proj)
}


