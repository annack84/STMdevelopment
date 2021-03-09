
#' Compile soil components for ecological site group of interest
#'
#' @param file_paths_user Character. User name to generate input data file paths.
#'   Options are "Anna", "Travis", and "VPN".
#' @param target_ESG The ecological site group to pull data for.
#'
#' @return Soil Profile Collection
#' @export

comp_data_pull <- function(
  file_paths_user = "Travis",
  target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands"
){
  library(aqp)
  file_paths <- data_file_paths(file_paths_user)
  ## Pull ESD, component, and horizon data
  esds <- readRDS(paste(file_paths$ssurgo_result_fldr,"/esd_final.rds",sep=""))
  comps <- readRDS(paste(file_paths$ssurgo_result_fldr,"/compdf_ucrb.rds",sep=""))
  comps_drainage <- readRDS(paste(file_paths$ssurgo_result_fldr,"/comp_drainage.rds",sep=""))
  esd_comps <- readRDS(paste(file_paths$ssurgo_result_fldr,"/UCRB_ESDs_SSURGO18.rds",sep=""))
  comps_surf <- readRDS(paste(file_paths$ssurgo_result_fldr,"/comp_surf.rds",sep=""))
  comps_sub <- readRDS(paste(file_paths$ssurgo_result_fldr,"/comp_sub.rds",sep=""))
  depth_comps <- readRDS(paste(file_paths$ssurgo_result_fldr,"/depth_comps.rds",sep=""))
  horizons <- readRDS(paste(file_paths$ssurgo_result_fldr,"/horizons_ucrb.rds",sep=""))

  ## Now Classify ESDs to ESG and query just ESG of interest
  esds$ESG <- NULL
  esds$ESGid <- NA
  esds$clim <- ifelse(esds$aimean<0.144&esds$maxtempmean>25.02,"Arid_Warm",ifelse(esds$maxtempmean<25.02 & esds$aimean>=0.144,"Semiarid_Cool","Semiarid_Warm"))
  # logic statements to key out esds
  esds$ESGid <- ifelse(esds$sgu=="Outcrops",1,esds$ESGid) # Outcrops
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="Breaks",2,esds$ESGid)  #arid warm breaks
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="SalineHills",3,esds$ESGid)  #arid warm saline hills
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="Gypsum",4,esds$ESGid)  #arid warm gypsum
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="VeryShallow",5,esds$ESGid)  #arid warm very shallow
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="SalineUplands",6,esds$ESGid)  #arid warm Saline uplands
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="Shallow",7,esds$ESGid)  #arid warm shallow
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="DeepRocky",8,esds$ESGid)  #arid warm deep rocky
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&(esds$sgu=="SandyUplands"|esds$sgu=="LoamyUplands"),9,esds$ESGid)  #arid warm sandy uplands and Loamy Uplands
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&(esds$sgu=="FinerUplands"|esds$sgu=="ClayUplands"),10,esds$ESGid) # arid warm finer uplands and clay uplands
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="SandyBottoms",11,esds$ESGid)  #arid warm sandy bottoms
  esds$ESGid <- ifelse(esds$clim=="Arid_Warm"&(esds$sgu=="SalineBottoms"|esds$sgu=="Bottoms"),12,esds$ESGid)  #arid warm saline bottoms and bottoms
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="Breaks",13,esds$ESGid)  # semiarid warm breaks
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="SalineHills",14,esds$ESGid)  # semiarid warm saline hills
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="Gypsum",15,esds$ESGid)  # semiarid warm gypsum
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="VeryShallow",16,esds$ESGid)  # semiarid warm very shallow
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="SalineUplands",17,esds$ESGid)  # semiarid warm saline uplands
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&(esds$sgu=="Shallow"|esds$sgu=="DeepRocky"),18,esds$ESGid)  # semiarid warm shallow and deep rocky
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&(esds$sgu=="SandyUplands"|esds$sgu=="LoamyUplands"),19,esds$ESGid)  # semiarid warm sandy uplands and loamy uplands
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="FinerUplands",20,esds$ESGid)  # semiarid warm finer uplands
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="ClayUplands",21,esds$ESGid)  # semiarid warm clay uplands
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&(esds$sgu=="SandyBottoms"|esds$sgu=="Bottoms"),22,esds$ESGid)  # semiarid warm sandy bottoms and bottoms
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="SalineBottoms",23,esds$ESGid)  # semiarid warm saline bottoms
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="Breaks",24,esds$ESGid)  # semiarid cool breaks ## THIS HAD NO ESDs IN ANALYSIS BUT CAME AS BLANK IN MAP
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="SalineHills",25,esds$ESGid)  # semiarid cool saline hills
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="Gypsum",26,esds$ESGid)  # semiarid cool gypsum ## This had no esds in this esds$climate zone as well in analysis, but had blanks in map
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="VeryShallow",27,esds$ESGid)  # semiarid cool very shallow: ## This had no esds in this esds$climate zone as well in analysis, but had blanks in map
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&(esds$sgu=="SalineUplands"|esds$sgu=="SandyUplands"|esds$sgu=="LoamyUplands"|esds$sgu=="FinerUplands"),28,esds$ESGid)  # semiarid cool saline uplands, sandy uplands, loamy uplands, and finer uplands
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="Shallow",29,esds$ESGid)  # semiarid cool shallow
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="DeepRocky",30,esds$ESGid)  # semiarid cool deep rocky
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="ClayUplands",31,esds$ESGid)  # semiarid cool clay uplands
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="SandyBottoms",32,esds$ESGid)  # semiarid cool sandy bottoms ## This had no esds in this esds$climate zone as well in analysis, but had blanks in map
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="SalineBottoms",33,esds$ESGid)  # semiarid cool saline bottoms
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="Bottoms",34,esds$ESGid)  # semiarid cool bottoms
  esds$ESGid <- ifelse(esds$sgu=="Riparian",35,esds$ESGid) # Riparian

  ## ESG Lookup table
  lookup_table <- dplyr::tribble(
    ~ESGid, ~ESG,
    1,      "Outcrops",
    2,      "Arid_Warm_Breaks",
    3,      "Arid_Warm_SalineHills",
    4,      "Arid_Warm_Gypsum",
    5,      "Arid_Warm_VeryShallow",
    6,      "Arid_Warm_SalineUplands",
    7,      "Arid_Warm_Shallow",
    8,      "Arid_Warm_DeepRocky",
    9,      "Arid_Warm_SandyUplands_LoamyUplands",
    10,     "Arid_Warm_FinerUplands_ClayUplands",
    11,     "Arid_Warm_SandyBottoms",
    12,     "Arid_Warm_SalineBottoms_Bottoms",
    13,     "Semiarid_Warm_Breaks",
    14,     "Semiarid_Warm_SalineHills",
    15,     "Semiarid_Warm_Gypsum",
    16,     "Semiarid_Warm_VeryShallow",
    17,     "Semiarid_Warm_SalineUplands",
    18,     "Semiarid_Warm_Shallow_DeepRocky",
    19,     "Semiarid_Warm_SandyUplands_LoamyUplands",
    20,     "Semiarid_Warm_FinerUplands",
    21,     "Semiarid_Warm_ClayUplands",
    22,     "Semiarid_Warm_SandyBottoms_Bottoms",
    23,     "Semiarid_Warm_SalineBottoms",
    24,     "Semiarid_Cool_Breaks",
    25,     "Semiarid_Cool_SalineHills",
    26,     "Semiarid_Cool_Gypsum",
    27,     "Semiarid_Cool_VeryShallow",
    28,     "Semiarid_Cool_SalineUplands_SandyUplands_LoamyUplands_FinerUplands",
    29,     "Semiarid_Cool_Shallow",
    30,     "Semiarid_Cool_DeepRocky",
    31,     "Semiarid_Cool_ClayUplands",
    32,     "Semiarid_Cool_SandyBottoms",
    33,     "Semiarid_Cool_SalineBottoms",
    34,     "Semiarid_Cool_Bottoms",
    35,     "Riparian"
  )

  ## Join ESG names to esds
  esds <- dplyr::left_join(esds,lookup_table,by="ESGid")

  ## Join esds to comp
  comp_jn <- dplyr::inner_join(esd_comps,esds[,c("ecoclassid","sgu","ESG")],by="ecoclassid")
  comp_jn <- dplyr::left_join(comp_jn,comps,by="cokey")
  comp_jn <- dplyr::left_join(comp_jn,depth_comps[,c("cokey","reskind","resdept_l","resdept_r","resdept_h")],by="cokey")
  comp_jn <- dplyr::left_join(comp_jn,comps_surf,by="cokey")
  comp_jn <- dplyr::left_join(comp_jn,comps_sub, by="cokey")

  ## Create Soil profile collection with selected components
  comp_jn <- comp_jn[comp_jn$ESG==target_ESG,]
  spc <- horizons[horizons$cokey %in% comp_jn$cokey,]
  aqp::depths(spc) <- cokey ~ hzdept_r + hzdepb_r # Create SPC
  spc@site <- dplyr::left_join(spc@site, comp_jn, by ="cokey") # Add component data

  return(spc)

}
