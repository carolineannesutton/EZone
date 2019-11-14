library(tidyverse)
library(ggplot2)
library(scales)

#read in data
Roughy <- read_csv("Results/OREastern_tidy2019.csv")

Roughy <- Roughy %>% 
  mutate(Length = SLadj_cm)


Roughycheck <- Roughy %>% 
  filter(year == 2019) %>% 
  group_by(year,Length) %>% 
  summarise(n()) %>% 
glimpse()

# summarise to include #mean SL for Area and sex by year
Roughy1_group <- Roughy %>% 
  drop_na(Area_MS) %>% 
  group_by(year,Area_MS,sex) %>% 
  summarise(SLadj_cm_mean = mean(SLadj_cm),
            sdSL = sd(SLadj_cm, na.rm = TRUE),
            n(),
            seSL = sd(SLadj_cm) / sqrt(n())
            ) %>% 
  rename(Length = SLadj_cm_mean) %>% 
glimpse()



  # summarise plot to have means potted with all data


ggplot(data = Roughy,
       mapping = aes(x = year,
                     y = Length,
                     colour = sex)) +
  geom_jitter(width = .5, size = 1)+
    
    
  geom_line(data=Roughy1_group, size = 1
          )+
  facet_wrap(~Area_MS)
 

ggplot(data = Roughy,
       mapping = aes(x = year,
                     y = Length,
                     colour = Area_MS)) +
  geom_jitter(width = .5, size = 1)+
  
  
  geom_line(data=Roughy1_group, size = 1
  )+
  facet_wrap(~sex)

ggplot(data = Roughy1_group,
       mapping = aes(x = year,
                     y = Length,
                     shape =sex,
                    colour = Area_MS,
                    )) +
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = seq(30,38,1),limits = c(30,38))+
  scale_x_continuous(limits = c(1986,2020), breaks = seq(1986,2020,2))
  
  

RoughyWeight<- Roughy %>% 
  drop_na(weight_kg) %>% 
  filter(weight_kg != 0)

RoughyWeight %>% 
  ggplot(aes(x = weight_kg)) +
  geom_histogram(bins = 10)


#create the LW relationship function for both combined sexes

Lyle_com <- function(x) {(0.037 * (x) ^ 2.942)/1000}
Lyle_com(30) 

Roughy %>% 
  ggplot()
  stat_function(fun = Lyle_com(Length))


ggplot(data = RoughyWeight,
       mapping = aes(x  =SLadj_cm,
                     y = weight_kg, colour = as.factor(year))) +
  geom_point()+
  stat_function(fun = Lyle_com, colour ="black")+
  facet_wrap(~year)

  
  

  #geom_abline(intercept = 1.23, slope = 0.057)

# trying to fit an exponential curve
# 0.037*M2^2.952
#  geom_abline(intercept = 1.23, slope = 0.057)+
>>>>>>> bf737eb87f4d8c6105d7d1973a0dfa0fff7a2046


       