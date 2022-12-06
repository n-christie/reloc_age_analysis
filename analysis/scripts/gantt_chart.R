# gantt chart

library(tidyverse)
library(lubridate)

df_gantt <- data.frame(Register=c("Total Population Register",
                            "Longitudinal integrated database \n for health insurance and labour market studies",
                            "Real estate property register",
                            "Apartment Register",
                            "Geographical database",
                            "Scania outdoor environment database",
                            "National Patient Register",
                            "Death cause register",
                            "Drug prescription register",
                            "Swedish intensive care register",
                            "Swedish internet-based surveillance system \n for communicable diseases",
                            "Municipal health care register",
                            "Interventions for elderly and people with disabilities register"), 
                 start=as.integer( c("1990", "1990", "1990", "1990", "1990", "1990", "1987", "1990", "2004", "2004", "2004", "2008", "2008")),
                 end=as.integer( c(rep("2020", 13)))) %>% 
  gather(key = data_type, value = date, - Register)

ggplot() +
geom_line(data=df_gantt, mapping=aes(x=fct_rev(fct_inorder(Register)), y=date), size=10, color = "blue") +
  coord_flip() +
  labs( title = "Register timeline",
        x = "",
        y = "") +
  scale_y_continuous(breaks = seq(from = 1986, to = 2020, by = 2)) +
  theme_light()
