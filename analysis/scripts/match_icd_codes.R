library(here)
library(tidyverse)
library(readxl)
library(openxlsx)


 code_list <- readRDS(here("analysis/files", "unique_icd.rds"))
# code_list_1 <- readRDS(here("analysis/files", "code_list_1.rds"))

icd_range <- read_excel(here("analysis/files","icd_risk_group_lookup_final.xlsx")) %>% 
  drop_na(code)

# atc_lookup <- read_csv("analysis/files/ATC.csv") %>% 
#   select(code = CUI,
#          "description" = "Preferred Label" ) %>% 
#   mutate(code_type = "atc")


## there are 3297 unique codes from slutvård dataset
## matching on icd description data - 1689 codes remain unmatched
## this apperars to be due to atc codes in the sample. 



no_matched_codes <- code_list %>%
  anti_join(icd_range,
            by = c('value' = 'code')) %>% 
  arrange()

matched_codes <- code_list %>%
  left_join(icd_range,
            by = c('value' = 'code')) %>% 
  distinct(value, .keep_all = TRUE) %>% 
  drop_na(label_swe) %>% 
  arrange()


 


codes_to_link <- unique_codes %>% 
  filter(source != 'icd_10_se_kap',
         source != 'icd_10_se_kap_imputed',
         source != 'icd_9_kapitel_conversion',
         source != 'icd_10_se_kap',
         code_type != 'kapitel')

## Risk groups

risk_groups <- read_excel(here("analysis/files","ICD for RELOC-AGE Registers ICD10 and ICD9 v2.xlsx")) %>% 
  rename(value = "code",
         #code_category = "Categorization",
         #disease_category = "Immunodeficiency* or organ transplant"
  )



matched_risk_group <- matched_codes %>%
  remove_empty('cols') %>% 
  left_join(risk_groups,
            by = c('value' )) %>% 
  distinct(value,.keep_all = TRUE) %>%
  remove_empty('cols')
  select(-"influenza_risk"  ,
         -"stroke_risk.x" ,
         )
  arrange()


no_matched_risk_group <- risk_groups %>%
  anti_join(code_list,
            by = c('value' )) %>% 
  #drop_na(code_category) %>% 
  distinct(value,.keep_all = TRUE) %>% 
  arrange()

write.xlsx(no_matched_risk_group, here("analysis/files/risk_groups","unmatched_group_codes.xlsx") )
write.xlsx(matched_risk_group, here("analysis/files/risk_groups","matched_group_codes.xlsx") )


saveRDS(c, here("analysis/files","icd_code_lookup.rds"))






## dbparser ----

#dbparser package to parse the codes from the database https://search.r-project.org/CRAN/refmans/dbparser/html/drug_atc_codes.html