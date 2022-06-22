#!/usr/bin/env Rscript
library(argparser)
library(tidyverse)
library(RCSM)

p <- arg_parser("Calculating connectivity-map score")
p <- add_argument(p, "rdata", help="profiles.Rdata")
p <- add_argument(p, "method", help="KSScore, XSumScore, GSEAweight0Score, GSEAweight1Score, GSEAweight2Score, ZhangScore")
p <- add_argument(p, "permuteNum", help="10000")
p <- add_argument(p, "ncpu", help="10")
p <- add_argument(p, "topN", help="500")
p <- add_argument(p, "minN", help="10")
argv <- parse_args(p)

# ======== FUNCTIONS ==========
run_cmap <- function(profile_s2m,
                     profile_d2m,
                     disease,
                     method,
                     permuteNum = 10000,
                     ncpu = 6,
                     topN = 500,
                     minN = 10) {
  # methods: c("KSScore", "XSumScore", "GSEAweight0Score", "GSEAweight1Score", "GSEAweight2Score", "ZhangScore")
  profile_d2m_sub <- profile_d2m %>%
    dplyr::filter(disease_name == disease) %>%
    dplyr::select(c(mirna_baseid, miRNA_reg_profile)) %>%
    distinct() %>%
    group_by(mirna_baseid) %>%
    dplyr::filter(n()==1)    # remove miRNA that are both up and down-regulated in a disease
  print(dim(profile_d2m_sub))

  if (nrow(profile_d2m_sub)>=minN) {

  # up-regulated miRNA
  qry_up <- profile_d2m_sub %>%
    dplyr::filter(miRNA_reg_profile=="up") %>%
    pull(mirna_baseid)

  # down-ragulated miRNA
  qry_down <- profile_d2m_sub %>%
    dplyr::filter(miRNA_reg_profile=="down") %>%
    pull(mirna_baseid)

  # calculate connectivity-map score
  if (method == "KSScore") {
    df <- KSScore(profile_s2m, qry_up, qry_down, permuteNum = permuteNum, pAdjMethod = "BH", mcCore = ncpu)
  } else if (method == "XSumScore") {
    df <- XSumScore(profile_s2m, qry_up, qry_down, topN = topN, permuteNum = permuteNum, pAdjMethod = "BH", mcCore = ncpu)
  } else if (method == "GSEAweight0Score") {
    df <- GSEAweight0Score(profile_s2m, qry_up, qry_down, permuteNum = permuteNum, pAdjMethod = "BH", mcCore = ncpu)
  } else if (method == "GSEAweight1Score") {
    df <- GSEAweight1Score(profile_s2m, qry_up, qry_down, permuteNum = permuteNum, pAdjMethod = "BH", mcCore = ncpu)
  } else if (method == "GSEAweight2Score") {
    df <- GSEAweight2Score(profile_s2m, qry_up, qry_down, permuteNum = permuteNum, pAdjMethod = "BH", mcCore = ncpu)
  } else if (method == "ZhangScore") {
    df <- ZhangScore(profile_s2m, qry_up, qry_down, permuteNum = permuteNum, pAdjMethod = "BH", mcCore = ncpu)
  } else {
    print("Please set a proper method!")
  }

  rst <- df %>%
    dplyr::filter(Score!=0)
  } else {
    rst <- 0
  }
  return(rst)
}


# =========== MAIN =============

load(argv$rdata)
permuteNum <- as.integer(argv$permuteNum)
ncpu <- as.integer(argv$ncpu)
topN <- as.integer(argv$topN)
minN <- as.integer(argv$minN)

# Calculate cmap score for each disease
cmap_score <- list()
for (disease in disease_names) {
    print(disease)
  cmap_score[[disease]] <- run_cmap(profile_s2m,
                                  profile_d2m,
                                  disease,
                                  method= argv$method,
                                  permuteNum = permuteNum,
                                  ncpu = ncpu,
                                  topN = topN,
                                  minN = minN)
}

# Save results
saveRDS(cmap_score, file = paste0(argv$method, ".rds"))

