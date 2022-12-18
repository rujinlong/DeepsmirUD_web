library(here)

prj_path <- here()
dpath <- here(prj_path, "data")
load(here(prj_path, "data/row_col_order.Rdata"))
cmap_g1 <- readRDS(here(dpath, "main/GSEAweight1Score.rds"))
cmap_rst <- cmap_g1

ftitle <- "GSEAweight1"
minN_miRNA <- 5
total_miRNA <- 10
cluster <- T
row_order=F
col_order=F
outdir = "interactive"


df <- data.frame(sm=names(profile_s2m))
for (disease in names(cmap_rst)) {
  profile_d2m_sub <- profile_d2m %>%
    dplyr::filter(disease_name == disease) %>%
    dplyr::select(c(mirna_baseid, miRNA_reg_profile)) %>%
    distinct() %>%
    group_by(mirna_baseid) %>%
    dplyr::filter(n()==1)    # remove miRNA that are both up and down-regulated in a disease

  # up-regulated miRNA
  qry_up <- profile_d2m_sub %>%
    dplyr::filter(miRNA_reg_profile=="up") %>%
    pull(mirna_baseid)

  # down-ragulated miRNA
  qry_down <- profile_d2m_sub %>%
    dplyr::filter(miRNA_reg_profile=="down") %>%
    pull(mirna_baseid)

  # print(disease)
  # print(dim(profile_d2m_sub))
  cmap_one_disease <- cmap_rst[[disease]]
  if (nrow(cmap_one_disease)>0) {
    # If there are no enough miRNA for calculating cmap score, set their cmap score to 0
    if (length(qry_up) < minN_miRNA | length(qry_down) < minN_miRNA) {
      if (nrow(profile_d2m_sub) < total_miRNA) {
        cmap_one_disease["Score"] = 0
        cmap_one_disease["pValue"] = 1
        cmap_one_disease["pAdjValue"] = 1
      }
    }

    # print(disease)
    # return(cmap_one_disease)
    cmap_disease <- cmap_one_disease %>%
      dplyr::filter(pAdjValue < pval) %>%
      data.table::setnames("Score", disease) %>%
      dplyr::select(disease) %>%
      rownames_to_column("sm")
    df <- df %>%
      left_join(cmap_disease, by = "sm")
  }
}

df2 <- df %>%
  mutate(colnames_new = sm, .keep = "unused") %>%
  left_join(df_smname_mapping, by = "colnames_new") %>%
  dplyr::select(-colnames_new) %>%
  column_to_rownames("colnames_old") %>%
  replace(is.na(.), 0) %>%
  t()


df2 <- df2/2
rg <- max(abs(df2))


rst <- heatmaply::heatmaply(df2[row_order, col_order],
                            colors = colorRampPalette(c("navy", "white", "red"))(100),
                            grid_gap = 0,
                            fontsize_row = 7,
                            fontsize_col = 7,
                            width = 3800,
                            height = 1000,
                            Rowv = F,
                            Colv = F,
                            xlab = "Small molecule",
                            ylab = "Cancer",
                            dynamicTicks = T,
                            cellnote_size = 4,
                            margin = c(10,10,30,0),
                            limits = c(-rg, rg))

saveRDS(rst, file = here(prj_path, "data/data4heatmap.rds"))
