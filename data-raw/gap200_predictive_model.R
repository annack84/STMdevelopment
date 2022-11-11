## code to prepare `gap200_predictive_model` dataset goes here

# get data
user <- "Anna"
file_paths <- data_file_paths(user)

LMF_AZ <- read.csv(file.path(file_paths$plotnet_processed, "LMF2021AZ_clean_2022-11-07.csv"))
LMF_CO <- read.csv(file.path(file_paths$plotnet_processed, "LMF2021CO_clean_2022-11-07.csv"))
LMF_NM <- read.csv(file.path(file_paths$plotnet_processed, "LMF2021NM_clean_2022-11-07.csv"))
LMF_UT <- read.csv(file.path(file_paths$plotnet_processed, "LMF2021UT_clean_2022-11-07.csv"))
LMF_WY <- read.csv(file.path(file_paths$plotnet_processed, "LMF2021WY_clean_2022-11-07.csv"))

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


LMF_all_canopy_clean <- LMF_all_canopy %>%
  # CA should always be less than or equal to CP
  #dplyr::filter(CA_percent_200plus <= CP_percent_200plus) %>%
  # filter out cases where CA is 0 and AH_AnnForbGrassCover is 0 and CP is >0 because
  # if there is no annual cover, then CA should be very close to CP.
  #dplyr::filter(!(CA_percent_200plus == 0 & CP_percent_200plus > 0 & AH_AnnForbGrassCover==0)) %>%
  tidyr::drop_na() # remove rows with any missing data

rm(LMF_AZ, LMF_CO, LMF_NM, LMF_UT, LMF_WY)

### Add in NRI
NRI_ucrb <- read.csv(file.path(file_paths$nri, "NRI_UCRB_clean_2022-11-03.csv"))
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

NRI_all_canopy_clean <- NRI_all_canopy %>%
  #dplyr::filter(CA_percent_200plus <= CP_percent_200plus) %>%
  #dplyr::filter(!(CA_percent_200plus == 0 & CP_percent_200plus > 0 & AH_AnnForbGrassCover==0)) %>%
  tidyr::drop_na()

# combine data sets
all_canopy_clean <- dplyr::bind_rows(LMF_all_canopy_clean, NRI_all_canopy_clean)

all_canopy_clean$CA_percent_100plus <- all_canopy_clean$CA_percent_100to200 + all_canopy_clean$CA_percent_200plus
all_canopy_clean$CP_percent_100plus <- all_canopy_clean$CP_percent_100to200 + all_canopy_clean$CP_percent_200plus

all_canopy_clean <- all_canopy_clean %>%
  # drop records where CP_percent_25plus is zero and CA_percent_25plus is more than 1 std. dev.
  # because CA theoretically shouldn't be higher than CP ever, but sometimes the
  # transect line shifts around as you're measuring and you can get CA slightly higher
  # than CP
  dplyr::filter(!(CA_percent_25plus >= (sd(CA_percent_25plus) + CP_percent_25plus)))


# final model?
lm_predict_ca200plus8 <- lm(CA_percent_200plus~CP_percent_200plus*AH_AnnForbGrassCover, data = all_canopy_clean)
summary(lm_predict_ca200plus8)
#plot(lm_predict_ca200plus8)

plot(y=all_canopy_clean$CA_percent_200plus,
     x=all_canopy_clean$CP_percent_200plus)
abline(lm_predict_ca200plus8, col = "blue", lwd = 2)

plot(y=all_canopy_clean$CA_percent_200plus,
     x=all_canopy_clean$AH_AnnForbGrassCover)

# another one for gaps >100
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
