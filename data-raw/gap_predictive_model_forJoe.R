## Gap modeling code for Joe

# get data (this is structured for Anna's ESG STM project, not Landscape Data Commons)
user <- "Anna" # this is part of the package I made for this analysis - won't be useful for you
file_paths <- data_file_paths(user) # this is part of the package I made for this analysis - won't be useful for you

# I limited to my study area, but you'll want the whole western US at least - maybe the whole US for NRI???
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
                # CP_... in my code indicates gaps stopped only by perennial plants
                CP_percent_25plus,
                CP_percent_100to200,
                CP_percent_200plus,
                CP_percent_25to50,
                CP_percent_50to100,
                # CA_... in my code indicates gaps stopped by both perennial and annual plants
                CA_percent_25plus,
                CA_percent_100to200,
                CA_percent_200plus,
                CA_percent_25to50,
                CA_percent_50to100) %>%
  dplyr::filter(!is.na(CP_percent_25plus))

# I did some data filtering for logical errors that I thought likely indicated
# bad data - but worth it to double check my logic with the gap methods! I only
# filtered based on open-ended ranges - I don't think this logic necessarily applies
# properly where gaps might have just gotten bumped into a different range. Might
# be better to do on the >25cm range than the >200cm range, though!

# TO DO: need to add in a filter here for plots where the field crew checked the
# checkbox indicating that both types of gap were the same (I believe this info
# is in the GAPS_DIFFERENT_NESW and GAPS_DIFFERENT_NWSE columns of the POINT table
# in the raw LMF and NRI data). Since they didn't actually collect both gap types,
# we can't justify a valid model with the copied data.

LMF_all_canopy_clean <- LMF_all_canopy %>%
  # CA should be less than or equal to CP - but I moved this filter to after I
  # combined the LMF and NRI data sets
  # dplyr::filter(CA_percent_200plus <= CP_percent_200plus) %>%
  # filter out cases where CA is 0 and AH_AnnForbGrassCover is 0 and CP is >0 because
  # if there is no annual cover, then CA should be very close to CP.
  dplyr::filter(!(CA_percent_200plus == 0 & CP_percent_200plus > 0 & AH_AnnForbGrassCover==0)) %>%
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
  # use same filters as LMF data
  #dplyr::filter(CA_percent_200plus <= CP_percent_200plus) %>%
  dplyr::filter(!(CA_percent_200plus == 0 & CP_percent_200plus > 0 & AH_AnnForbGrassCover==0)) %>%
  tidyr::drop_na()

# combine data sets
all_canopy_clean <- dplyr::bind_rows(LMF_all_canopy_clean, NRI_all_canopy_clean)

all_canopy_clean$CA_percent_100plus <- all_canopy_clean$CA_percent_100to200 + all_canopy_clean$CA_percent_200plus
all_canopy_clean$CP_percent_100plus <- all_canopy_clean$CP_percent_100to200 + all_canopy_clean$CP_percent_200plus

all_canopy_clean <- all_canopy_clean %>%
  # drop records where CP_percent_25plus is zero and CA_percent_25plus is more than 1 std. dev.
  # because CA theoretically shouldn't be higher than CP ever, but sometimes the
  # transect line shifts around as you're measuring and you can get CA slightly higher
  # than CP. One standard deviation might not be the most appropriate filter here...
  dplyr::filter(!(CA_percent_25plus >= (sd(CA_percent_25plus) + CP_percent_25plus)))

# Models to predict annual+perennial gaps based on perennial gaps and LPI annual herbaceous cover
# I included an interaction term between the two predictors (the * symbol), but
# I think it might be easier for end users of the model parameters if there was
# no interaction term (replace * with +) - maybe run this question past Sarah, too?
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
#usethis::use_data(gap200_predictive_model, overwrite = TRUE) # this line was to save the model within the package I made

gap100_predictive_model <- lm_predict_ca100plus
#usethis::use_data(gap100_predictive_model, overwrite = TRUE) # this line was to save the model within the package I made

# Sarah is interested in more models than this - I believe she'd like to see
# predictions in the other direction (predict perennial gaps based on
# annual+perennial gaps and LPI annual herb cover), as well as for all the standard
# AIM gap size ranges.

# NOTE: I made these models without doing any assumption tests, which isn't ideal.
# Per the internet (https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R5_Correlation-Regression/R5_Correlation-Regression4.html#:~:text=There%20are%20four%20assumptions%20associated%20with%20a%20linear,fixed%20value%20of%20X%2C%20Y%20is%20normally%20distributed.)
# "There are four assumptions associated with a linear regression model:
#
# "Linearity: The relationship between X and the mean of Y is linear.
# "Homoscedasticity: The variance of residual is the same for any value of X.
# "Independence: Observations are independent of each other.
# "Normality: For any fixed value of X, Y is normally distributed."

# For linearity, if we get significant p-values for the slopes, that tells us
# there is a probably a linear relationship between our predictors and response:
coef(summary(gap200_predictive_model))

# For independence, that's based on our sampling structure - that's why we need
# to drop observations where the field crew only measured one type of gap and then
# checked the checkbox to say the two types were the same.

# Shapiro Wilks test checks for normality in the model residuals. A significant
# p-value indicates that the residuals are NOT normal, breaking a linear model assumption.
# Sometimes transforming the RESPONSE variable can fix models that break this
# assumption. For EDGE, we tried these three transformations, in this order,
# running the Sharpiro Wilks test on each until we go a model with normal residuals:
# 1. square root
# 2. natural log
# 3. squared
# Make sure to report the transformation(s) used in the paper so that people
# using your model parameters know they must back-transform their predictions
# to get actual gap values!
shapiro_pvalue <- shapiro.test(gap200_predictive_model$residuals)$p.value

# Breusch-Pagan test for homoskedasticity (equal variance). P-values less than
# 0.05 for the Breusch-Pagan test indicate heteroscedasticity in the residuals
# (i.e. assumption is violated). This issue is harder to fix, though transforming
# the predictor variable(s) can sometimes help. I'd start with the same transformations
# listed above.
bp_pvalue <- lmtest::bptest(gap200_predictive_model)$p.value

# Obviously you'll also want to check the model fit with r-squared, too. I got
# pretty good r-squareds with the models for the Four Corners states that I made,
# but it's possible things could get squirrely trying to model across more of the
# US. One way to potentially improve that is to remove outliers based on Cook's
# distance (see my EDGE biomass allometry code for how to do that).
# If that doesn't help enough, Sarah suggested that the next step is to look for thresholds
# in the standard monitoring data (maybe bare soil??) to stratify by and see
# if some stratified submodels perform better than a global model.
