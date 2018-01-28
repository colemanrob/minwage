
library(tidyverse)
library(hrbrthemes)
wages <- read_csv("c:\\users\\rob\\Documents\\projects\\wagedata\\data\\2A71-DAS-Wages2017OpenData--EDSC-All-20171025-V10-MC.csv", locale = locale(encoding = "latin1"))


wages %>% 
  filter(!(str_detect(`ER Code_Code RE`, "^ER\\d{2}$"))) %>% #filter provincial results do not appear
  filter(`ER Code_Code RE` != "ER00") %>% #remove national totals
  filter(!is.na(`Median wage_Salaire Median`)) %>% #remove mising
  filter(`Annual Wage Flag_Salaire annuel` < 1) %>% #remove annual salaries
  filter(`Median wage_Salaire Median` <= 15) %>% 
  filter(PROV == "ON" & `ER Name_Nom RE` == "Northeast" | `ER Name_Nom RE` == "Northwest") %>% 
  ggplot(aes(reorder(`NOC Title`, -`Median wage_Salaire Median`), `Median wage_Salaire Median`, fill=`ER Name_Nom RE`)) +
  geom_col(alpha=0.7, fill="#91AAB4", show.legend = F) +
  coord_flip() +
  facet_wrap(~`ER Name_Nom RE`) +
  labs(title = "Which occupations are affected by changes in minimum wage?",
       x = NULL,
       y = "median hourly wage",
       subtitle = "Northern Ontario Occupations by Median Hourly Wage",
       caption = "Source: Wage data available via Open Data \n http://open.canada.ca/data/en/dataset/adad580f-76b0-4502-bd05-20c125de9116") +
  theme_ipsum_rc(base_family="Roboto", grid = "X")


wages %>%
  filter(!is.na(`Median wage_Salaire Median`)) %>%
  filter(!(str_detect(`ER Code_Code RE`, "^ER\\d{2}$"))) %>% #filter provincial results do not appear
  filter(`ER Code_Code RE` != "ER00") %>% #remove national totals
  filter(`Annual Wage Flag_Salaire annuel` < 1) %>% #remove annual salaries
  filter(PROV == "ON") %>%
  mutate(low_wage = case_when(
    `Median wage_Salaire Median` <= 14 ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(`ER Name_Nom RE`) %>%
  summarise(total_occupations = n_distinct(NOC_CNP),
            total_low_wage = sum(low_wage)) %>%
  mutate(prop_low_wage = total_low_wage / total_occupations) %>%
  ggplot(aes(reorder(`ER Name_Nom RE`, prop_low_wage), prop_low_wage)) +
  geom_col(alpha=0.7, fill="#91AAB4") +
  coord_flip() +
  scale_y_percent() +
  labs(title = "Where does the change in minimum wage affect Ontario communities most?",
       x = NULL,
       y = "% of occupations with median wages < $14/hr",
       subtitle = "Proportion of Occupations with Median Wage < $14/hr by Ontario Community",
       caption = "Source: Wage data available via Open Data \n http://open.canada.ca/data/en/dataset/adad580f-76b0-4502-bd05-20c125de9116") +
  theme_ipsum_rc(base_family="Roboto", grid = "X")

par(oma=c(0,0,2,0))
ggsave("c:\\users\\rob\\Documents\\projects\\wagedata\\img\\minwage2.png", height = 9, width = 16, dpi = 100)