#' ull ecological site data for the ecological site group of interest
#'
#' @param user Character. User name to generate input data file paths. Options
#'   are "Anna", "Travis", and "VPN".
#' @param target_ESG The ecological site group to pull data for.
#'
#' @return Returns a list of attributes for the target ESG based on the
#'   component Ecological Site Descriptions
#' @export
#'
esd_data_pull <- function(
  user = "Travis",
  target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands"
){
  file_paths <- data_file_paths(user)
  ## Pull ESD, component, and horizon data
  esds <- readRDS(paste(file_paths$ssurgo_result_fldr,"/esd_final.rds",sep=""))
  esd_comps <- readRDS(paste(file_paths$ssurgo_result_fldr,"/UCRB_ESDs_SSURGO18.rds",sep=""))

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
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="LoamyUplands",19,esds$ESGid)  # semiarid warm loamy uplands
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
  esds$ESGid <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="SandyUplands",36,esds$ESGid)  # semiarid warm sandy uplands uplands


  ## ESG Lookup table
  lookup_table <- ESG_table

  ## Rename production fields for plotting
  esds <- esds %>% dplyr::rename(
    Total = TOTAL_PROD,
    Big.Sage = bigsage,
    Mtn.Sage = mtnsage,
    Basin.Sage = basinsage,
    Blackbrush = cora,
    Gambels.Oak = quga,
    Pinyon = pinion,
    Juniper = juniper,
    Aspen = aspen,
    Ponderosa = ponderosa,
    Greasewood = greasewd,
    Saltbush = saltbush,
    Shadscale = shadscale,
    Alk.Sacaton = spai,
    C3.Per.Grass = c3PerGr,
    C4.Per.Grass = c4PerGr
  )

  ## Join ESG names to esds
  esds <- dplyr::left_join(esds,lookup_table,by="ESGid")
  esds <- dplyr::left_join(esds,texture_table,by = c("txtcls_sub" = "textabbr"))
  esds$txtnm_sub <- esds$textname
  esds$textname <- NULL
  esds <- dplyr::left_join(esds,texture_table,by = c("txtcls_surf" = "textabbr"))
  esds$txtnm_surf <- esds$textname
  esds$textname <- NULL
  esd_comps <- esd_comps[!duplicated(esd_comps$ecoclassid),]
  esds <- dplyr::left_join(esds,esd_comps[,c("ecoclassid","ecoclassname")],by="ecoclassid")
  esds <- esds[esds$ESGs_text==target_ESG,]

  return(esds)

}
