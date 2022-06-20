00-create_d2m_s2m_profile
================
Compiled at 2022-06-20 20:02:12

The purpose of this document is …

``` r
library("conflicted")
library(here)
library(readxl)
library(tidyverse)
source("../R/utils.R")

prj_path <- normalizePath("..")
fpath <- here(prj_path, "data/disease.xlsx")

# for sm2miRNA profile normalization
deepsmirud_threshold <- 0.5
```

## Tasks

The first task is …

-   Non-human miRNA were removed from DeepsmirUD predictions. 6841

### Disease-miRNA regulation profile

The disease-miRNA regulation profile is defined as a miRNA-disease
matrix converted from wide to long format, and stored in
`df_miRNA2disease` dataframe. Values in the matrix is either “up” or
“down” indicate the miRNA is up or down-regulated in the disease. The
regulation profile was obtained from
[miRCancer](http://mircancer.ecu.edu/) database.

``` r
df_disease <- read_excel(fpath, sheet = "mircancer")
disease_names <- unique(df_disease$disease_name)

# disease-miRNA profile
profile_d2m <- df_disease %>% 
  mutate(miRNA_reg_profile = Profile, .keep = "unused") %>% 
  dplyr::select(c(disease_name, mirna_baseid, miRNA_reg_profile))
```

There are 1028 miRNAs involved in 131 diseases (connectivity:
6.7425075%).

### SM-miRNA regulation profile

The regulation profile was predicted using DeepsmirUD.

``` r
df_deepsmirud <- read_excel(fpath, sheet = "deepsmirud")

# remove non-human miRNA
df_deepsmirud_human <- df_deepsmirud %>% 
  dplyr::filter(str_detect(mir_name, "^hsa") | str_detect(mir_name, "^mi"))

# remove SM-miRNA pair redundancy by `mirna_baseid`
# TODO: use more proper way to reduce redundancy
df_deepsmirud_human_nr <- df_deepsmirud_human %>% 
  dplyr::select(c(mirna_baseid, sm_name, DeepsmirUD)) %>% 
  distinct(mirna_baseid, sm_name, .keep_all = T)

# SM-miRNA profile in wide format
profile_s2m_raw <- df_deepsmirud_human_nr %>% 
  pivot_wider(names_from = sm_name, values_from = DeepsmirUD) %>% 
  column_to_rownames("mirna_baseid")


# --------- normalize and format SM-miRNA profile matrix -----------
profile_s2m <- profile_s2m_raw - deepsmirud_threshold
profile_s2m[is.na(profile_s2m)] <- 0

# Change SM name so that they are distinct and satisfy DF column name requirement.
colnames_old <- colnames(profile_s2m)
colnames(profile_s2m) <- make.names(colnames(profile_s2m), unique = T)
colnames_new <- colnames(profile_s2m)
df_smname_mapping <- data.frame(colnames_new = colnames_new,
                                colnames_old = colnames_old)
```

|              | Original | Human | Distinct |
|--------------|----------|-------|----------|
| Mirbase      | 1494     | 648   |          |
| mirna_baseid | 1080     | 667   | 667      |

There are 667 miRNAs involved in 1343 small compounds (connectivity:
7.4460164^{-4}%).

## Files written

These files have been written to the target directory,
data/00-create_d2m_s2m_profile:

``` r
save(profile_d2m, profile_s2m, df_smname_mapping, disease_names, file = path_target("profiles.Rdata"))
projthis::proj_dir_info(path_target())
```

    ## # A tibble: 1 × 4
    ##   path           type         size modification_time  
    ##   <fs::path>     <fct> <fs::bytes> <dttm>             
    ## 1 profiles.Rdata file         128K 2022-06-20 18:02:14
