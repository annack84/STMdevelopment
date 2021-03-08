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

  ESG_number <-ESG_table$ESGs_final[which(ESG_table$ESGs_text==target_ESG)]

  target_ESG_raster <- all_ESGs
  target_ESG_raster[target_ESG_raster!=ESG_number] <- 0
  target_ESG_raster[target_ESG_raster==ESG_number] <- 1

  target_ESG_raster <- raster::aggregate(x=target_ESG_raster, fact=2, fun=raster_mode,
                                         na.rm=T)

  raster::writeRaster(x=target_ESG_raster,
                      filename = file.path(dirname(data_file_paths(user)$ESG_map),
                                           paste0(target_ESG, ".tif")),
                      format = "GTiff",
                      datatype = "LOG1S",
                      overwrite = T)
  return(target_ESG_raster)
}


