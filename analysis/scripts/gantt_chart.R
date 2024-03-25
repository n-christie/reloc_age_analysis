# gantt chart
library(here)
library(tidyverse)
library(lubridate)
library(ggthemes)

df_gantt <- data.frame(Register=c("Total Population Register",
                            "Longitudinal Integrated Database for \n Health Insurance and Labour Market Studies",
                            "Real Estate Property Register",
                            "National Patient Register",
                            "National Cause of Death Register",
                            "National Perscribed Drug Register",
                            "Swedish Intensive Care Register",
                            "Swedish Internet-based Surveillance System \n for Communicable Diseases",
                            "National Register of Interventions in Municipal Health Care ",
                            "National Register of Care and Social Services \n for the Elderly and Persons with Impairments",
                            "Apartment Register",
                            "Geographical Database",
                            "Scania Outdoor Environment Database"), 
                 start=as.integer( c("1990", "1990", "1990", "1987", "1990", "2005", "2009", "2008", "2005", "2005", "2008", "1990", "2006")),
                 end=as.integer( c("2020", "2019","2020","2020","2020","2020","2022","2020","2020","2020", "2021", "2020", "2020"))) %>% 
  gather(key = data_type, value = date, - Register)

ggplot() +
geom_line(data=df_gantt, mapping=aes(x=fct_rev(fct_inorder(Register)), y=date), size=5, color = "gray") +
  coord_flip() +
  labs( title = "",
        x = "",
        y = "") +
  scale_y_continuous(breaks = seq(from = 1986, to = 2022, by = 3)) +
  theme_hc() +
  theme(text = element_text(size=15))


ggsave(here("output/figures","gantt.png"), width = 11, height = 4, units = "in")
