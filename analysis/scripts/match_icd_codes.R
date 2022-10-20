library(here)
library(tidyverse)
library(readxl)


code_list <- readRDS(here("analysis/files", "code_list.rds"))
code_list_1 <- readRDS(here("analysis/files", "code_list_1.rds"))

icd_range <- read_excel(here("analysis/files","icd_risk_group_lookup_final.xlsx"))


unique_codes <- code_list_1 %>%
  select(-n) %>% 
  rbind(code_list) %>% 
  distinct(code) %>% 
  left_join(icd_range,
            by = c('code' = 'code')) %>% 
  distinct(code, .keep_all = TRUE) %>% 
  filter(!is.na(code),
         code != "")




no_matched_codes <- code_list_1 %>%
  select(-n) %>% 
  rbind(code_list) %>% 
  distinct(code) %>% 
  anti_join(icd_range,
            by = c('code' = 'code')) %>% 
  distinct(code, .keep_all = TRUE) %>% 
  filter(!is.na(code),
         code != "") %>% 
  arrange(
    
  )



codes_to_link <- unique_codes %>% 
  filter(source != 'icd_10_se_kap',
         source != 'icd_10_se_kap_imputed',
         source != 'icd_9_kapitel_conversion',
         source != 'icd_10_se_kap',
         code_type != 'kapitel')




saveRDS(c, here("analysis/files","icd_code_lookup.rds"))

## dbparser ----

#dbparser package to parse the codes from the database https://search.r-project.org/CRAN/refmans/dbparser/html/drug_atc_codes.html