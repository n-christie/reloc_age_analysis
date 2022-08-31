library(here)
library(tidyverse)
library(readxl)


code_list <- readRDS(here("analysis/files", "code_list.rds"))
code_list_1 <- readRDS(here("analysis/files", "code_list_1.rds"))

icd_range <- read_excel(here("analysis/files","icd_range_swe_lookup.xlsx"))

icd_group_2<- read_excel(here("analysis/files","icd_groups_2.xlsx"), sheet = 3) %>% 
  select(code,description) %>% 
  mutate(group_2 = 1)


icd_group <- read_excel(here("analysis/files","icd_10_codes.xlsx")) %>% 
  mutate(group_1 = 1,
         code = str_remove(code, "[.]"))

a <- code_list %>% 
  left_join(icd_range,
            by = c('code' = 'code'))


b <- code_list_1 %>% 
  left_join(icd_range,
            by = c('code' = 'code')) %>% 
  arrange(desc(n))



c <- a %>% 
  left_join(icd_group_2,
            by = "code") %>% 
  left_join(icd_group,
            by = c('code')
  ) %>% 
  filter(!is.na(text))



saveRDS(c, here("analysis/files","icd_code_lookup.rds"))



library(icd)


vignette("introduction", package = "icd")

vignette("ranges", package = "icd")

is_valid(code_list$code, short_code = TRUE)

a <- explain_code(code_list$code, condense = TRUE) # finds 88 out of 2683 codes


remotes::install_github("gforge/comorbidities.icd10")



library(icdcoder)

code_list <-  code_list %>% 
  mutate()

getICDRange("001","002", "icd9")
getICDRange("A00","A03", "icd10")  



library(decoder)

a <- decode(code_list$code, "icd10se" )


