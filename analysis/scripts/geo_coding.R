



library(here)
library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(jsonlite)
library(gganimate)
library(gifski)
library(ggsflabel)

## working ----


## get files and merge with komun data

tmp <- tempfile()
download.file("http://api.thenmap.net/v2/se-7/geo/2020-06-06", destfile = tmp)

mun_name <- fromJSON("http://api.thenmap.net/v2/se-7/data/2020-06-06?language=en&data_props=name|shapeid|is_in") %>% 
  unnest(is_in) %>% 
  rename(county = is_in)

mun <- read_sf(tmp) %>% 
  left_join(mun_name, by = c("id" = "shapeid")) %>% 
  mutate(fill_data = rnorm(nrow(.)))

skåne_mun <- mun %>% 
  filter(county == "Skåne County") %>% 
  mutate(fill_data = rnorm(nrow(.)))

ggplot(mun) +
  geom_sf(aes(fill = fill_data)) +
  scale_fill_viridis_c() +
  theme_void()

mun1 <- mun %>% 
  #filter(str_detect(county, 'County')) %>% 
  distinct(name,.keep_all = TRUE) %>% 
  left_join(komun_code_table,
            by = c('name' = 'municipality', 'county'))

saveRDS(mun1, here('analysis/files', 'komun_geo_table.rds'))

## map data

map_se <- read_rds( here('analysis/files', 'komun_geo_table.rds')) %>% 
  mutate(fill_data = rnorm(nrow(.)))

map_kommun <- count_table_komun %>% 
  left_join(map_se,
            by = c('kommun' = 'code')) %>% 
  select(id = kommun, geometry,  year, name, county , seat,  count = n)


count_table_komun_reloc <- readRDS("~/reloc_age_analysis/output/tables/count_table_komun_reloc.rds")


temp_merge <- count_table_komun_reloc %>% 
  filter(change_housing == 'Multi-dwelling to Special housing',
         #county == "Skåne County",
          year == 2019) 
  
skan_spec <- map_se %>% 
  left_join(temp_merge,
            by = c('code' = 'kommun')) %>% 
  select(id = kommun, geometry,  year, name, county , seat,  change_housing, count = n)




## whole country komun
map_kommun %>% 
  #filter(!is.na(id)) %>% 
ggplot( aes(geometry = geometry)) +
  geom_sf(aes(fill = count)) +
  scale_fill_viridis_c(option = "cividis") +
  theme_void()

## skåne



skåne_map <- skan_spec %>% 
  filter(county == "Skåne County",
         # year == 2019
         ) 

ggplot(skåne_map, aes(geometry = geometry)) +
  geom_sf(aes(
     fill = n
    )) +
  scale_fill_viridis_c(option = "cividis") +
  theme_light()+
  geom_sf_label(aes(label = name),
                size = 3) +
  labs(fill = "DATA") 


ggsave(here('output/figures', 'skane.png'))

## animate  - WORKING!

map_expand <- map_se %>% 
  mutate("1999" = fill_data,
         "2000" = fill_data + rnorm(nrow(.)),
         "2001" = fill_data + rnorm(nrow(.)),
         "2002" = fill_data + rnorm(nrow(.)),
         "2003" = fill_data + rnorm(nrow(.)),
         "2004" = fill_data + rnorm(nrow(.)),
         "2005" = fill_data + rnorm(nrow(.)),
         ) %>% 
  pivot_longer(cols=  c("1999", "2000", "2001", "2002","2003","2004","2005"), 
               names_to = 'year') %>% 
  mutate(year = as.integer(year))

swed <- ggplot(map_expand) +
  geom_sf(aes(fill = value)) +
  #scale_fill_viridis_c() +
  #theme_void() +
  theme_light() +
  transition_states(year) +
  labs(title = 'Year: {closest_state}')

animate(plot = swed,
        fps = 12,
        renderer = gifski_renderer(loop = T))

anim_save(here('output/figures', 'swed.gif'))

skåne_map <- map_expand %>% 
  filter(county == "Skåne County") 


skan_map <- ggplot(skåne_map) +
  geom_sf(aes(fill = value)) +
  #scale_fill_viridis_c() +
  #theme_void() +
  theme_light() +
  transition_states(year) +
  labs(title = 'Year: {closest_state}')

animate(plot = skan_map,
        fps = 12,
        renderer = gifski_renderer(loop = T))


anim_save(here('output/figures', 'skan.gif'))



## archive ----

tmp1 <- tempfile()
tmp2 <- tempfile()

download.file("http://api.thenmap.net/v2/se-4/geo/2020-06-03", destfile = tmp1)
download.file("http://api.thenmap.net/v2/se-7/geo/2020-06-03", destfile = tmp2)

county <- read_sf(tmp1)
mun <- read_sf(tmp2)

plot(mun)
plot(county)

