
comp_data_pull <- function(
  file_paths_user = "Travis",
  target_ESG = "Semiarid_Warm_SandyUplands_LoamyUplands"
){
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
  esds$ESG <- NA
  esds$clim <- ifelse(esds$aimean<0.144&esds$maxtempmean>25.02,"Arid_Warm",ifelse(esds$maxtempmean<25.02 & esds$aimean>=0.144,"Semiarid_Cool","Semiarid_Warm"))
  # logic statements to key out esds
  esds$ESG <- ifelse(esds$sgu=="Outcrops","Outcrops",esds$ESG) # Outcrops
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="Breaks",2,esds$ESG)  #arid warm breaks
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="SalineHills",3,esds$ESG)  #arid warm saline hills
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu=="Gypsum",4,esds$ESG)  #arid warm gypsum
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu==16,5,esds$ESG)  #arid warm very shallow
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu==12,6,esds$ESG)  #arid warm Saline uplands
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu==15,7,esds$ESG)  #arid warm shallow
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu==4,8,esds$ESG)  #arid warm deep rocky
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&(esds$sgu==14|esds$sgu==7),9,esds$ESG)  #arid warm sandy uplands and Loamy Uplands
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&(esds$sgu==5|esds$sgu==3),10,esds$ESG) # arid warm finer uplands and clay uplands
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&esds$sgu==13,11,esds$ESG)  #arid warm sandy bottoms
  esds$ESG <- ifelse(esds$clim=="Arid_Warm"&(esds$sgu==10|esds$sgu==1),12,esds$ESG)  #arid warm saline bottoms and bottoms
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="Breaks",13,esds$ESG)  # semiarid warm breaks
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="SalineHills",14,esds$ESG)  # semiarid warm saline hills
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu=="Gypsum",15,esds$ESG)  # semiarid warm gypsum
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu==16,16,esds$ESG)  # semiarid warm very shallow
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu==12,17,esds$ESG)  # semiarid warm saline uplands
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&(esds$sgu==15|esds$sgu==4),18,esds$ESG)  # semiarid warm shallow and deep rocky
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&(esds$sgu==14|esds$sgu==7),19,esds$ESG)  # semiarid warm sandy uplands and loamy uplands
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu==5,20,esds$ESG)  # semiarid warm finer uplands
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu==3,21,esds$ESG)  # semiarid warm clay uplands
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&(esds$sgu==13|esds$sgu==1),22,esds$ESG)  # semiarid warm sandy bottoms and bottoms
  esds$ESG <- ifelse(esds$clim=="Semiarid_Warm"&esds$sgu==10,23,esds$ESG)  # semiarid warm saline bottoms
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="Breaks",24,esds$ESG)  # semiarid cool breaks ## THIS HAD NO ESDs IN ANALYSIS BUT CAME AS BLANK IN MAP
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="SalineHills",25,esds$ESG)  # semiarid cool saline hills
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu=="Gypsum",26,esds$ESG)  # semiarid cool gypsum ## This had no esds in this esds$climate zone as well in analysis, but had blanks in map
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu==16,27,esds$ESG)  # semiarid cool very shallow: ## This had no esds in this esds$climate zone as well in analysis, but had blanks in map
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&(esds$sgu==12|esds$sgu==14|esds$sgu==7|esds$sgu==5),28,esds$ESG)  # semiarid cool saline uplands, sandy uplands, loamy uplands, and finer uplands
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu==15,29,esds$ESG)  # semiarid cool shallow
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu==4,30,esds$ESG)  # semiarid cool deep rocky
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu==3,31,esds$ESG)  # semiarid cool clay uplands
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu==13,32,esds$ESG)  # semiarid cool sandy bottoms ## This had no esds in this esds$climate zone as well in analysis, but had blanks in map
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu==10,33,esds$ESG)  # semiarid cool saline bottoms
  esds$ESG <- ifelse(esds$clim=="Semiarid_Cool"&esds$sgu==1,34,esds$ESG)  # semiarid cool bottoms
  esds$ESG <- ifelse(esds$sgu=="Riparian","Riparian",esds$ESG) # Riparian
}
