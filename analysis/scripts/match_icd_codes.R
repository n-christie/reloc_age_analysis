library(here)
library(tidyverse)
library(readxl)


code_list <- readRDS(here("analysis/files", "code_list.rds"))
code_list_1 <- readRDS(here("analysis/files", "code_list_1.rds"))

icd_range <- read_excel(here("analysis/files","icd_risk_group_lookup_final.xlsx"))


b <- code_list_1 %>% 
  left_join(icd_range,
            by = c('code' = 'code')) %>% 
  distinct(code, .keep_all = TRUE) %>% 
  arrange(desc(n))

bb <- b %>% 
  filter(source != 'icd_10_se_kap',
         source != 'icd_10_se_kap_imputed',
         source != 'icd_9_kapitel_conversion',
         source != 'icd_10_se_kap',
         code_type != 'kapitel')




saveRDS(c, here("analysis/files","icd_code_lookup.rds"))

## dbparser ----

#dbparser package to parse the codes from the database https://search.r-project.org/CRAN/refmans/dbparser/html/drug_atc_codes.html