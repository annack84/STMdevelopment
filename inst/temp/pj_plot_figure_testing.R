# trying out different ways of plotting the PJ treatment data.
# Run SW_SandyLoamyUplands_EDIT_nogaps.R to line 2089 to put all the required
# objects into your environment.
# Created Feb 3, 2023
# Anna Knight, aknight@usgs.gov

# PJ Treatments
WRI_polygons <- st_read(dsn = "C:/Users/aknight/Documents/Telework_Backups/V_drive/ANNA_KNIGHT/ESG/Maps/FickPJ",
                        layer = "PJ") %>%
  st_transform(crs = st_crs(fuzzy_cluster_memb_sf))

sf_use_s2(FALSE)
pj_plots <- st_join(fuzzy_cluster_memb_sf, WRI_polygons, sparse = T) %>%
  mutate(YearSampled = year(DateSampled),
         TreatedPreSampling = ifelse(test = YearSampled>=yearStart,
                                     yes = TRUE,
                                     no = FALSE)) %>%
  mutate(TreatedPreSampling = ifelse(test = is.na(TreatedPreSampling),
                                     yes = FALSE,
                                     no = TreatedPreSampling))

# Look at the types of treatments present, then categorize into broader types for visual summaries
sort(unique(paste(pj_plots$method, pj_plots$treat, sep = " - ")))
# additional documentation available in G:\Base Layers\Disturbance\TREATMENTS.
# For WRI treatments, see also the database at https://wri.utah.gov/wri/project/search.html

pj_plots_cats <- pj_plots %>%
  filter(!is.na(src)) %>%
  mutate(GeneralTreatmenType = ifelse(test = treat %in% c(# my categories: prescribed fire, brush removal, herbaceous removal, seeding, soil improvements. CHECK aerator, mowing, and weeding for each ESG to make sure they still fit as assigned here!!!
    "aerator",
    "brush removal",
    "mowing",
    "weeding"),
    yes = "Woody removal",
    no = NA),
    TreatmentSubType = ifelse(test = GeneralTreatmenType=="Woody removal" & method %in% c("anchor chain",
                                                                                          "bullhogfull size",
                                                                                          "bullhogskid steer",
                                                                                          "double drum (1-way)",
                                                                                          "lop and scatter",
                                                                                          "mastication",
                                                                                          "mechanical",
                                                                                          "mowing",
                                                                                          "rollerchop"),
                              yes = "Mechanical",
                              no = ifelse(test = GeneralTreatmenType=="Woody removal" & method %in% c("herbicide",
                                                                                                      "herbicide aerial"),
                                          yes = "Chemical",
                                          no = NA
                              )
    ))

n_plots_treated <- length(unique(filter(pj_plots_cats, TreatedPreSampling==T)$PlotCode))
n_plots_tx_twice_or_more <- pj_plots_cats %>%
  filter(TreatedPreSampling==T) %>%
  group_by(PlotCode) %>%
  summarise(n_diff_treatments = n_distinct(yearStart)) %>%
  filter(n_diff_treatments >= 2) %>%
  nrow()

#### plot by count, stacked bars for each group ####
pj_plot_count <- pj_plots_cats %>%
  filter(TreatedPreSampling==T) %>%
  st_drop_geometry() %>%
  group_by(Best_group) %>%
  summarise(n_MechanicalUnseeded = length(which(TreatmentSubType=="Mechanical" & seeded==0)),
            n_MechanicalSeeded = length(which(TreatmentSubType=="Mechanical" & seeded==1)),
            n_ChemicalUnseeded = length(which(TreatmentSubType=="Chemical" & seeded==0)),
            n_ChemicalSeeded = length(which(TreatmentSubType=="Chemical" & seeded==1))
  ) %>%
  tidyr::pivot_longer(cols = c(n_MechanicalUnseeded, n_MechanicalSeeded, n_ChemicalUnseeded, n_ChemicalSeeded),
                      names_to = "TreatmentPair",
                      values_to = "count") %>%
  mutate(Best_group = as.factor(Best_group))

treatments_plot_counts <- ggplot(data = pj_plot_count, aes(x = Best_group, y = count, fill = TreatmentPair)) +
  geom_col() +
  scale_fill_manual(values = c("#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Chemical woody treatment (seeded)",
                               "Chemical woody treatment",
                               "Mechanical woody treatment (seeded)",
                               "Mechanical woody treatment")) +
  scale_x_discrete(name = NULL,
                   labels = c("Open woodland", "Shrubland", "Invaded", "Grassland")) +
  ylab("Number of plots") +
  labs(caption = paste(strwrap(paste("Number of plots within treatment areas documented in Utah's Watershed Restoration Initiative database, the USGS Land Treatment Digital Library, or the NRCS Utah Range treatment database. A total of", n_plots_treated, "out of the", nrow(plot_data_first), "plots in this analysis fell within at least one treatment project. Of these,", n_plots_tx_twice_or_more, "received treatments from two or more projects."), 50), collapse = "\n")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
treatments_plot_counts
#windows(6, 4.5)

#### plot by percent, stacked bars for each group
plots_per_group <- data.frame(Best_group = c(1:4),
                              Best_group_labs = as.factor(c("Open woodland", "Shrubland", "Invaded", "Grassland")),
                              plots_per_group = c(nrow(filter(plot_data_first, PlantCommunity_fuzzy==1)),
                                                  nrow(filter(plot_data_first, PlantCommunity_fuzzy==2)),
                                                  nrow(filter(plot_data_first, PlantCommunity_fuzzy==3)),
                                                  nrow(filter(plot_data_first, PlantCommunity_fuzzy==4))
                                                  ))

pj_plot_pct <- pj_plots_cats %>%
  left_join(., plots_per_group) %>%
  filter(TreatedPreSampling==T) %>%
  st_drop_geometry() %>%
  group_by(PlotCode) %>% # next 3 lines select only the treatment closest to the sampling year
  arrange(desc(yearStart), .by_group = T) %>%
  mutate(Tx_flag = row_number()) %>%
  filter(Tx_flag==1) %>%
  ungroup() %>%
  group_by(Best_group) %>%
  summarise(MechanicalUnseeded = 100*length(which(TreatmentSubType=="Mechanical" & seeded==0))/first(plots_per_group),
            MechanicalSeeded = 100*length(which(TreatmentSubType=="Mechanical" & seeded==1))/first(plots_per_group),
            ChemicalUnseeded = 100*length(which(TreatmentSubType=="Chemical" & seeded==0))/first(plots_per_group),
            ChemicalSeeded = 100*length(which(TreatmentSubType=="Chemical" & seeded==1)/first(plots_per_group)),
            Best_group_labs = first(Best_group_labs)
  ) %>%
  tidyr::pivot_longer(cols = c(MechanicalUnseeded, MechanicalSeeded, ChemicalUnseeded, ChemicalSeeded),
                      names_to = "TreatmentPair",
                      values_to = "pct") %>%
  mutate(Best_group = as.factor(Best_group))

treatments_plot_pcts <- ggplot(data = pj_plot_pct, aes(x = Best_group, y = pct, fill = TreatmentPair)) +
  geom_col() +
  scale_fill_manual(values = c("#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Chemical woody treatment (seeded)",
                               "Chemical woody treatment",
                               "Mechanical woody treatment (seeded)",
                               "Mechanical woody treatment")) +
  scale_x_discrete(name = NULL,
                   labels = c("Open woodland", "Shrubland", "Invaded", "Grassland")) +
  ylab("% of plots treated in each
plant community") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
treatments_plot_pcts

# similar but dodge the bars
ggplot(data = pj_plot_pct, aes(x = Best_group, y = pct, fill = TreatmentPair)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Chemical woody treatment (seeded)",
                               "Chemical woody treatment",
                               "Mechanical woody treatment (seeded)",
                               "Mechanical woody treatment")) +
  scale_x_discrete(name = NULL,
                   labels = c("Open woodland", "Shrubland", "Invaded", "Grassland")) +
  ylab("% of plots treated in each
plant community") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")


#### facet by mechanical vs chemical ####
pj_plot_pct2 <- pj_plot_pct %>%
  mutate(Treatment = ifelse(test=grepl(pattern = "Mechanical", x=TreatmentPair),
                            yes = "Mechanical",
                            no = "Chemical"),
         Seeding = ifelse(test = grepl(pattern = "Unseeded", x=TreatmentPair),
                          yes = "Unseeded",
                          no = "Seeded"))

treatment_plots_pct_facet <- ggplot(data = pj_plot_pct2, aes(x=Best_group, y=pct, fill = Seeding)) +
  geom_col(position = "dodge") +
  facet_wrap(~Treatment, nrow = 1) +
  scale_x_discrete(name = NULL,
                   labels = c("Open woodland", "Shrubland", "Invaded", "Grassland")) +
  ylab("% of plots treated in each
plant community") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
treatment_plots_pct_facet

# dodge treatments but stack seeding
ggplot(data = pj_plot_pct2, aes(x = Treatment, y = pct, fill = Seeding)) +
  geom_col() +
  ylab("% of plots treated in each
plant community") +
  facet_wrap(~Best_group_labs, nrow=1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
