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

a <- code_list %>% 
  left_join(icd_range_swe_lookup,
            by = c('code' = 'icd_10_swe'))


icd_range_swe_lookup <- read_excel("D:/Downloads/icd_range_swe_lookup.xlsx")



