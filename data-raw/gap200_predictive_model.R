## code to prepare `gap200_predictive_model` dataset goes here

# get data
user <- "Anna"
file_paths <- data_file_paths(user)

LMF_AZ <- read.csv(file.path(file_paths$plotnet_processed, "LMFSpring2020AZ_clean_2021-11-23.csv"))
LMF_CO <- read.csv(file.path(file_paths$plotnet_processed, "LMFSpring2020CO_clean_2021-11-24.csv"))
LMF_NM <- read.csv(file.path(file_paths$plotnet_processed, "LMFSpring2020NM_clean_2021-11-24.csv"))
LMF_UT <- read.csv(file.path(file_paths$plotnet_processed, "LMFSpring2020UT_clean_2021-11-24.csv"))
LMF_WY <- read.csv(file.path(file_paths$plotnet_processed, "LMFSpring2020WY_clean_2021-11-24.csv"))

LMF_all <- dplyr::bind_rows(LMF_AZ, LMF_CO, LMF_NM, LMF_UT, LMF_WY)

LMF_all_wide <- tidyr::pivot_wider(dplyr::select(LMF_all, -Longitude_NAD83, -Latitude_NAD83, -Month, -Day),
                                   names_from = variable, values_from = value, values_fill = NA)

LMF_all_canopy <- LMF_all_wide %>%
  dplyr::select(SourceKey,
         PlotID,
         SiteName,
         PlotName,
         #Longitude_NAD83,
         #Latitude_NAD83,
         Year,
         #Month,
         #Day,
         AH_AnnForbGrassCover,
         FH_AnnCover,
         AH_PerenCover,
         CP_percent_25plus,
         CP_percent_100to200,
         CP_percent_200plus,
         CP_percent_25to50,
         CP_percent_50to100,
         CA_percent_25plus,
         CA_percent_100to200,
         CA_percent_200plus,
         CA_percent_25to50,
         CA_percent_50to100) %>%
  dplyr::filter(!is.na(CP_percent_25plus))

# filter out cases where CA is 0 but CP is >0 - they probably only did CP measurements on these plots because
# CA should basically always be less than CP
# LMF_all_canopy_clean <- filter(LMF_all_canopy,
#                                !(CP_percent_25plus + CP_percent_100to200 + CP_percent_200plus + CP_percent_25to50 + CP_percent_50to100>0 &
#                                  CA_percent_25plus + CA_percent_100to200 + CA_percent_200plus + CA_percent_25to50 + CA_percent_50to100==0))
LMF_all_canopy_clean <- dplyr::filter(LMF_all_canopy, CA_percent_200plus <= CP_percent_200plus)

rm(LMF_AZ, LMF_CO, LMF_NM, LMF_UT, LMF_WY)

### Add in NRI
NRI_ucrb <- read.csv(file.path(file_paths$nri, "NRI_UCRB_clean_2021-11-01.csv"))
#sort(unique(NRI_ucrb$variable))

NRI_all_wide <- tidyr::pivot_wider(dplyr::select(NRI_ucrb, -Longitude_NAD83, -Latitude_NAD83, -Month, -Day),
                                   names_from = variable, values_from = value, values_fill = NA)

NRI_all_canopy <- NRI_all_wide %>%
  dplyr::select(SourceKey,
         PlotID,
         SiteName,
         PlotName,
         #Longitude_NAD83,
         #Latitude_NAD83,
         Year,
         #Month,
         #Day,
         AH_AnnForbGrassCover,
         #FH_AnnCover,
         #AH_PerenCover,
         CP_percent_25plus,
         CP_percent_100to200,
         CP_percent_200plus,
         CP_percent_25to50,
         CP_percent_50to100,
         CA_percent_25plus,
         CA_percent_100to200,
         CA_percent_200plus,
         CA_percent_25to50,
         CA_percent_50to100)

NRI_all_canopy_clean <- dplyr::filter(NRI_all_canopy, CA_percent_200plus <= CP_percent_200plus)

# combine data sets
all_canopy_clean <- dplyr::bind_rows(LMF_all_canopy_clean, NRI_all_canopy_clean)

# final model?
lm_predict_ca200plus8 <- lm(CA_percent_200plus~CP_percent_200plus*AH_AnnForbGrassCover, data = all_canopy_clean)
summary(lm_predict_ca200plus8)
#plot(lm_predict_ca200plus8)

plot(y=all_canopy_clean$CA_percent_200plus,
     x=all_canopy_clean$CP_percent_200plus)
abline(lm_predict_ca200plus8, col = "blue", lwd = 2)

plot(y=all_canopy_clean$CA_percent_200plus,
     x=all_canopy_clean$AH_AnnForbGrassCover)

# another one for gaps 100-200
lm_predict_ca100to200 <- lm(CA_percent_100to200~CP_percent_100to200*AH_AnnForbGrassCover, data = all_canopy_clean)
summary(lm_predict_ca100to200)
#plot(lm_predict_ca100to200)

plot(y=all_canopy_clean$CA_percent_100to200,
     x=all_canopy_clean$CP_percent_100to200)
abline(lm_predict_ca100to200, col = "blue", lwd = 2)

plot(y=all_canopy_clean$CA_percent_100to200,
     x=all_canopy_clean$AH_AnnForbGrassCover)

# another one for gaps >100
all_canopy_clean$CA_percent_100plus <- all_canopy_clean$CA_percent_100to200 + all_canopy_clean$CA_percent_200plus
all_canopy_clean$CP_percent_100plus <- all_canopy_clean$CP_percent_100to200 + all_canopy_clean$CP_percent_200plus

lm_predict_ca100plus <- lm(CA_percent_100plus~CP_percent_100plus*AH_AnnForbGrassCover, data = all_canopy_clean)
summary(lm_predict_ca100plus)
#plot(lm_predict_ca100plus)

plot(y=all_canopy_clean$CA_percent_100plus,
     x=all_canopy_clean$CP_percent_100plus)
abline(lm_predict_ca100plus, col = "blue", lwd = 2)

plot(y=all_canopy_clean$CA_percent_100plus,
     x=all_canopy_clean$AH_AnnForbGrassCover)

# save final models
gap200_predictive_model <- lm_predict_ca200plus8
usethis::use_data(gap200_predictive_model, overwrite = TRUE)

gap100_predictive_model <- lm_predict_ca100plus
usethis::use_data(gap100_predictive_model, overwrite = TRUE)
