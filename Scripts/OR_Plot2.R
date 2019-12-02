library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)

#read in data
Roughy <- read_csv("Results/OREastern_tidy2019.csv")
Roughy1 <- data.frame(Roughy) %>% 
  select(Area = Area_MS, year,sex, Length= SLadj_cm)

Roughy <- Roughy %>% 
  mutate(Length = SLadj_cm) %>% 
  mutate(Area = Area_MS)

Roughy1 <- data.frame(Roughy) %>% 
  select(Area = Area_MS, year,sex, Length= SLadj_cm)


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



# summarise plot to have means potted with all data by Area
ggplot(data = Roughy,
       mapping = aes(x = year,
                     y = Length,
                     colour = sex)) +
  geom_jitter(width = .5, size = 1)+
  geom_line(data=Roughy1_group, size = 1)+
  facet_wrap(~Area)


#average SL by Area and sex
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
 
# ggboxplot
ggboxplot(Roughy1, x ="year", y= "Length", group = "sex",fill = "sex",
          outlier.colour = NULL)+
  theme(axis.text.x= element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 8))+
  facet_wrap(~Area)


  
#boxplot
ggplot(Roughy,aes(x= as.character(year), y = Length)) +
  geom_boxplot(aes(colour = sex, outlier.colour = sex))+
  theme_bw()+
  theme(axis.text.x= element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 8))+
  
  stat_summary(fun.y = mean, aes(group = sex), geom = "point",
              position = position_dodge(.9),size = .5) +
  #geom_hline(yintercept = 35.6, linetype ="dashed")+
  #geom_hline(yintercept = 33.4, linetype = "dashed")+
  facet_wrap(~Area_MS)
  

Roughy %>% 
group_by(sex) %>% 
  summarise(mean(Length))

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

ggerrorplot(Roughy, x = "year", desc_stat = mean_ci(Length,ci=0.95,
                                                    error.limit = "both"),
                     y = "Length", 
                     shape = "sex",
                     colour = "Area_MS"
       )

  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = seq(30,38,1),limits = c(30,38))+
  scale_x_continuous(limits = c(1986,2020), breaks = seq(1986,2020,2))

       