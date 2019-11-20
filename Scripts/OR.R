library(tidyverse)
library(naniar)
library(lubridate)



#READ IN SHOT DATA (1106)
Shot <- read_csv("data/ORshotdata.csv",col_types = list("Date"=col_date("%d/%m/%Y"))) 
Shot

# Fliter shot data for Eastern Zone only
#rename eastern to Eastern (2009 data only)
#filter for Eastern Data (599 records)
#filter for St Patricks and St Helens (226, 253)
#unique(df)

Shot_1 <- Shot %>% #(464)
  filter(Area %in% c("St Patricks", "St Helens")) %>% 
  mutate(zone=case_when(zone =="eastern"~ "Eastern",TRUE~ as.character(zone))) %>% 
  mutate(Survey = case_when(Survey== "EMP2013"~"EP2013", TRUE~as.character(Survey))) %>% 
  mutate(Survey = case_when(Survey== "S02019"~"SO2019", TRUE~as.character(Survey))) %>% 
  #mutate_all(funs(str_replace(., "eastern", "Eastern")))%>%
  mutate(Shot  = as.numeric(Operation)) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  rename(ShotWeight_kg = 'Weight (kg)')%>% 
  glimpse()



Shotcheck <- Shot_1 %>% 
 # filter(Year==1996) %>% 
  group_by(Area,Shot) %>% 
  summarise(n()) %>% 
  glimpse()


#READ IN BIOLOGICAL DATA (nrows = 65008)
Biologicals <- read_csv("Data/ORmeasures.csv",col_types = list("mark_type" = col_character(),"sea_port" = col_character(), "Length (mm)" = col_double(),
                                                              "Op No_shot_tx" = col_character(), "Date"=col_date("%d/%m/%Y")))
Biologicals

Biocheck <-Biologicals %>% 
  group_by(year) %>% 
  summarise(n()) %>% 
  glimpse()


nrow(Biologicals)


# checking out survey names
checksurvey <- Biologicals %>% 
  mutate(SvS = if_else(Survey == Survey2, 1,0)) %>%
  group_by(Area) %>% 
  summarise(n()) %>% 
  glimpse()



# TIDY BIOLOGICAL DATA 
# filter for Eastern zone (44827)
Biologicals_1 <- Biologicals %>% 
  filter(zone == "Eastern")%>% 
  select(zone,year, month, Area, Survey,Shot=Op_shot_no, FishNo = `fish no`, sex, stage, weight_kg =`Weight (kg)`, 
         SL_cm =  `Length (cm)`, SLadj_cm =`Length Adjusted (cm)`,Otoliths, mark= mark_type) %>%
  ##note that "Area" data from 2009 is only found with the df ORShotdata
  mutate(month =ifelse(year == 2016, "Jul",month)) %>% 
  
  # 2006 must exclude 17 adn 31 shot b/c they are not from SH or SP
  mutate(Area = case_when(year==2006 & Shot==17 ~ "Hawk", TRUE ~ Area)) %>% 
  mutate(Area = case_when(year==2006 & Shot==31 ~ "N St Helens", TRUE ~ Area)) %>% 
  
  mutate_all(funs(str_replace(., "St patricks head", "St Patricks"))) %>%
  mutate_all(funs(str_replace(., "W.H.N.St Helens", "St Helens"))) %>%
  mutate_all(funs(str_replace(., "paddys head", "St Patricks"))) %>%
  mutate_all(funs(str_replace(., "Knuckey05", "AP2005"))) %>%
  mutate_all(funs(str_replace(., "Knuckey07", "SP2007"))) %>% 
  mutate_all(funs(str_replace(., "Knuckey08", "SP2008"))) %>%
  mutate_all(funs(str_replace(., "f", "F"))) %>%
  mutate_all(funs(str_replace(., "m", "M"))) %>%
  mutate_all(funs(str_replace(., "j", "J"))) %>%
  mutate_all(funs(str_replace(., "July", "Jul"))) %>%
  mutate(FishNo = as.numeric(FishNo)) %>% 
  mutate(weight_kg = as.numeric(weight_kg)) %>% 
  mutate(stage =as.numeric(stage)) %>% 
  mutate(SLadj_cm = as.numeric(SLadj_cm)) %>%
  mutate(SL_cm = as.numeric(SL_cm)) %>% 
  mutate(Otoliths = case_when(Otoliths == "N"~ 0,
                              Otoliths == "Y" ~ 1,
                              Otoliths == "1"~ 1)) %>% 
  glimpse()

Biologicals_1

nrow(Biologicals_1)
Biocheck1 <- Biologicals_1 %>% 
  #filter(year) %>% 
  group_by(year,Area) %>% 
  summarise(n()) %>% 
  glimpse()

Biologicals %>% 
  ggplot(aes(x='weight (kg)',
           y= ))

#FILTER FOR THE SPAWNING POPULATION (36197 + 24)
# note this is where shot 24 is filtered out from "1996" fixed this above to replace W.H.N.St Helens with St Helens
Biologicals_2 <- Biologicals_1 %>% 
  mutate(Shot = as.numeric(Shot)) %>% 
  filter(Area == "St Helens" |Area =="St Patricks" | Area =="U")%>% 
  filter(is.na(mark)| mark =="AS" | mark  =="AP"| mark =="U")%>% 
  
  # 2004 updated from 3348 to 3019 to exclude presurvey data (Shot >=1)
  filter(Shot>=1) %>% 
  # "AS", "AP" are aggregations, "BS" and "BP" are backscatter part of a field experiment in 1999
  filter(month == "Jul")%>% 
  filter(sex == "F"| sex =="M") %>% 
  glimpse()



Biocheck2 <- Biologicals_2 %>%
  filter(year==2001) %>% 
  group_by(year) %>% 

  summarise(n()) %>% 
  glimpse()


# READ IN BIOLOGICAL DATA FROM 2019
OR2019_Biologicals <- read_csv("Data/sxo_201901_biological_measurements.csv")


# tidy to enable binding of Biologicals and 2019 data
# adding Survey column to enable bind on "Survey" and "Shot" columns
OR2019_Biologicals_1 <- add_column(OR2019_Biologicals,Survey ='SO2019') %>% 
  # check the Area data and replace survey names
  add_column(month = 'Jul') %>% 
  add_column(zone = 'Eastern') %>% 
  add_column(year = '2019') %>% 
  mutate_all(funs(str_replace(., "Paddies", "St Patricks"))) %>% 
  mutate_all(funs(str_replace(., "f", "F"))) %>%
  mutate_all(funs(str_replace(., "m", "M"))) %>%
  mutate(Shot = as.numeric(`Operation No`))%>%
  mutate(weight_g = as.numeric(`Weight (g)`)) %>%
  mutate(weight_kg = weight_g / 1000) %>% 
  mutate(stage = as.numeric(stage)) %>% 
  mutate(SL_cm = as.numeric(`Standard Length (mm)`)/10) %>% 
  mutate(FishNo = as.numeric(`Specimen #`)) %>%
  mutate(SLadj_cm = SL_cm) %>% 
  #no adjustment to length although based on discussion with RD, measured length may have
  # been fork length
  select(zone,Survey,year, month,Shot, Area, FishNo, sex, stage, weight_kg, SL_cm, SLadj_cm) %>% 
  glimpse()

OR2019_Biologicals_1

# create a new column for otoliths. Otoliths were collected from every fish that was weighed.
# new column based on otoliths, then for all weights greater than 0 Otolith is 1
# not sure how to use case when in this situtation so this is the alternative method
OR2019_Biologicals_1$Otoliths <- OR2019_Biologicals_1$weight_kg
OR2019_Biologicals_1$Otoliths[OR2019_Biologicals_1$weight_kg>0] <- 1
glimpse(OR2019_Biologicals_1)

OR2019_Biologicals_1 %>% # (2509)
  select(zone,Survey,year, month,Shot, Area, FishNo, sex, stage, weight_kg, SL_cm, SLadj_cm, Otoliths) %>% 
  glimpse()



#Bind Biological data from 2019 to previous survey Biological data 36197 + 2509 = 38706)

Biologicals_3 <- bind_rows(Biologicals_2, OR2019_Biologicals_1) %>% 
  mutate(Shot = as.numeric(Shot)) %>% 
  mutate(year=as.numeric(year)) %>% 
  glimpse()


36197+2509



Biocheck3 <- Biologicals_3 %>% 
  filter(year==2001) %>% 
  group_by(year) %>% 
  summarise(n()) %>% 
  glimpse()

Biologicals_3
Shot_1


ORMeasuresShot <- left_join(Biologicals_3, Shot_1,by= c("Survey","Shot")) %>% 
  mutate(Area_MS = ifelse(Area.x =="U",Area.y,Area.x)) %>% 
  filter(Area_MS != "U")

ORMeasuresShot

MScheck <- ORMeasuresShot %>%
  #filter(year==2013) %>% 
  group_by(year)%>% 
  summarise(n()) %>% 
  glimpse()
# 1990 must exclude unknown "Area_MS
# 1999 must ensure that only AS and AP are selcted
# 2001 data have been updated with values fish measures increasing from 2644 to 3492
# 2004 updated from 3348 to 3019 to exclude presurvey data (Shot >=1)
# 2006 must exclude 17 adn 31 shot b/c they are not from SH or SP
# 2013 updated from 1034 to 1042



MSCheck_1 <- left_join(Biocheck3,MScheck, by ="Shot")



36197+2509

Biologicals_3
Shot_1

#Tidy data frame (38706)






write.csv(ORMeasuresShot,"Results/OREastern_tidy2019.csv")


test <- ORMeasuresShot %>% 
  filter(year==1990) %>% 
  group_by(Survey,year,Shot,lat1,Date) %>% 
  summarise() %>% 
  glimpse()
  
test1<- Shot_1 %>% 
  filter(Year==1990) %>% 
  group_by(Survey,Year,Shot,lat1,Date) %>% 
  summarise() %>% 
  glimpse()


