#' @export
ESG_table <- dplyr::tribble(
  ~ESGs_final, ~ESGs_text,
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

#' @export
Climate_group_definitions_replace <- c(
  "SW" = "Semiarid Warm",
  "SC" = "Semiarid Cool",
  "AW" = "Arid Warm"
)

#' @export
SGU_definitions_replace <- c(
  "VSh" = "Very Shallow",
  "Sh" = "Shallow",
  "SaU" = "Saline Uplands",
  "SaH" = "Saline Hills",
  "SBt" = "Sandy Bottoms",
  "Bt" = "Bottoms",
  "SU" = "Sandy Uplands",
  "LU" = "Loamy Uplands",
  "SaBt" = "Saline Bottoms",
  "GY" = "Gypsum",
  "FU" = "Finer Uplands",
  "DR" = "Deep Rocky",
  "CU" = "Clay Uplands",
  "Bks" = "Breaks"
)

