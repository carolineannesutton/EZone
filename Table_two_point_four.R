

## this was remonved b/c of probs with the "grouping variable" need to update this code

## Table 2.4 (needs updating for future surveys to include shot without measures in "OR.R")
```{r table 2.4, echo=FALSE}
#NOTE THIS TABLE MUSt BE REMOVED BEFORE KNIT as IT HAS A GROUPING PROB. OK if just in R

#need to update code to include shot weights for shots where fish were not measured **
# fitler data for 2019
Roughy_2 <- Roughy_1 %>% 
  filter(year==2019)

Roughy_2_sum <- Roughy_2 %>% 
  dplyr::group_by(Ground,Shot,Date,ShotWeight_kg,sex) %>% 
  dplyr::summarise(num_rows = n()) %>% 
  spread(sex,num_rows) %>% 
  mutate(`Total` = F + M) 

# creating ground/ and survey totals for:

#St Helens
TSH <- Roughy_2_sum %>% 
  filter(Ground == "St Helens") %>% 
  dplyr::group_by(Ground) %>% 
  dplyr::summarise(ShotWeight_kg =sum(ShotWeight_kg), F= sum(F), M = sum(M), Total =sum(Total)) %>% 
  mutate(Ground = "St Helens Total")

#St Patricks
TSP <- Roughy_2_sum %>% 
  filter(Ground == "St Patricks") %>% 
  dplyr::group_by(Ground) %>% 
  dplyr::summarise(ShotWeight_kg =sum(ShotWeight_kg, na.rm = TRUE), F= sum(F), M = sum(M), Total =sum(Total)) %>% 
  mutate(Ground = "St Patricks Total")

# totals for SH and SP
TSHSP <- Roughy_2_sum %>% 
  summarise(ShotWeight_kg = sum(ShotWeight_kg, na.rm = TRUE), F= sum(F), M = sum(M), Total =sum(Total)) %>% 
  mutate(Ground = "Survey Total")

#bind the shot and subtotals and total
t2.4 <- bind_rows(Roughy_2_sum, TSH, TSP,TSHSP) %>%   # put all together for totals
  dplyr::rename("Shot Weight (kg)" = ShotWeight_kg, "Total Nos Fish" = Total)


write.csv(t2.4,"Figures/t2.4.csv")

knitr::kable(head(t2.4, n=14), format = "html", caption = "Table 2.4 Summary of shot dates and weights for St Helens and St Patricks, Eastern Zone, 2019") %>% 
  kableExtra::kable_styling("striped") %>%
  kableExtra::scroll_box(width  = "100%")
```