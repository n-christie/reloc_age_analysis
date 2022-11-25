## there are 3297 unique codes from slutvård dataset
## matching on icd description data - 1689 codes remain unmatched
## this apperars to be due to atc codes in the sample. 
### Update 15/11/22 - after matching atc codes from drugbank we have 988 un matched/ 2309 matched
### unmatched appear to be bad data

## Load packages 

library(here)
library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)



## Load data  ----

code_list <- readRDS(here("analysis/files", "unique_icd.rds")) #list of unique codes in slutvård dataset with frequency

icd_range <- read_excel(here("analysis/files","icd_risk_group_lookup_final.xlsx")) %>% #lookup table for descriptions etc.
  drop_na(code)

risk_groups <- read_excel(here("analysis/files","ICD for RELOC-AGE Registers ICD10 and ICD9 v2.xlsx")) %>% 
  rename(value = "code",
         #code_category = "Categorization",
         #disease_category = "Immunodeficiency* or organ transplant" 
         )

### clean atc codes ----

drug_atc_codes <- read_csv(here("analysis/files/drugbank", "drug_atc_codes.csv")) %>% 
  mutate(code_type = "atc") #atc code descriptions

atc_lev_1 <- drug_atc_codes %>% 
  select("value" = atc_code, "level" = level_1, code_type)

atc_lev_2 <- drug_atc_codes %>% 
  select("value" = code_1, "level" = level_1, code_type)

atc_lev_3 <- drug_atc_codes %>% 
  select("value" = code_2, "level" = level_2, code_type)

atc_lev_4 <- drug_atc_codes %>% 
  select("value" = code_3, "level" = level_3, code_type)

atc_lev_5 <- drug_atc_codes %>% 
  select("value" = code_3, "level" = level_4, code_type)

drug_codes <- rbind(atc_lev_1,atc_lev_2,atc_lev_3,atc_lev_4,atc_lev_5) %>% 
  mutate(source = "drugbank")

rm(atc_lev_1,atc_lev_2,atc_lev_3,atc_lev_4,atc_lev_5,drug_atc_codes)


## Match icd and atc codes ----

matched_codes <- code_list %>%
  left_join(icd_range,
            by = c('value' = 'code')) %>% 
  distinct(value, .keep_all = TRUE) %>% 
  left_join(drug_codes,
            by = c("value" )) %>% 
  mutate(code_type = coalesce(code_type.x, code_type.y),
         source = coalesce(source.x, source.y)) %>% 
  select(-code_type.x, -code_type.y, -source.x, -source.y) %>% 
  distinct(value, .keep_all = TRUE) %>% 
  #filter(!is.na(source)) %>% 
  arrange()


## Match risk groups ----


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


# load updated file with flu risk groups ----

flu_risk <- read_excel(here("analysis/files/risk_groups","flu_risk.xlsx"),
                       sheet = "flu_risk_groups") 








#### DONT RUN!  Already parsed  - - --- dbparser ----

library(dbparser)
library(tidyverse)
library(XML)

read_drugbank_xml_db(here("analysis/files/drugbank","full database.xml"))

## load drugs data
drugs <- drugs()

## load drug groups data
drug_groups <- drug_groups()

## load drug targets actions data
drug_targets_actions <- targets_actions()



