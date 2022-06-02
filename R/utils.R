library(tidyverse)
library(RCSM)

run_cmap <- function(df_ref, df_disease, dname, permuteNum = 1000, ncpu = 6) {
  df_disease_sel <- df_disease %>%
    dplyr::filter(disease_name == dname) %>%
    dplyr::select(c(mirna_baseid, Profile)) %>%
    distinct() %>%
    group_by(mirna_baseid) %>%
    dplyr::filter(n()==1) %>%
    mutate(profile_disease = Profile, .keep = "unused")

  feature_up <- df_disease_sel %>%
    dplyr::filter(profile_disease=="up") %>%
    pull(mirna_baseid)
  feature_down <- df_disease_sel %>%
    dplyr::filter(profile_disease=="down") %>%
    pull(mirna_baseid)

  df_rst <- KSScore(df_ref, feature_up, feature_down, permuteNum = permuteNum, pAdjMethod = "BH", mcCore = ncpu) %>%
    dplyr::filter(Score!=0) %>%
    dplyr::select(-pValue)

  return(df_rst)
}
