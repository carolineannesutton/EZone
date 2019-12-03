library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)
library(Rmisc)

#read in the tidy data from results
Roughy <- read_csv("Results/OREastern_tidy2019.csv")

#rename columns
Roughy <- Roughy %>% 
  mutate(Length = SLadj_cm) %>% 
  mutate(Area = Area_MS) %>% 
  drop_na(Area) %>% 
  drop_na(Length) %>% 
  drop_na(sex)

Roughy1 <- (Roughy) %>% 
  select(Area = Area_MS, year,sex, Length= SLadj_cm)


Roughycheck <- Roughy %>% 
  filter(year == 2019)%>% 
  dplyr::group_by(year,Length) %>% 
  dplyr::summarise(n()) %>% 
glimpse()

# summary stats to create confidence intervals
Roughy_sumstats <- Roughy1 %>% 
  dplyr::group_by(year, Area, sex) %>% 
  dplyr::summarise(Avg_Length = mean(Length),
            n= n(),
            sd = sd(Length, na.rm = TRUE),
            se = sd(Length) / sqrt(n()))

#confidence inervals
# ci = avg +- z * se
#z score = qnorm(0.975) b/c 2.5% (0.025) on both tails
Roughy_groupci  <- Roughy_sumstats %>% 
  select(year, Area,sex,Avg_Length,n,sd) %>% 
  mutate(error = qnorm(0.975) * sd/sqrt(n))


#summarise data: using summarySE package Rmisc
#summarySE provides the standard deviation, 
#standard error of the mean, 
#and a (default 95%) confidence interval

roughy2_sum <- summarySE(Roughy1, measurevar = "Length", groupvars = c("year","Area","sex"))

# summarise plot to have means potted with all data by Area
ggplot(data = Roughy,
       mapping = aes(x = year,
                     y = Length,
                     colour = sex)) +
  geom_jitter(width = .5, size = 1)+
  geom_line(data=Roughy1_group, size = 1)+
  facet_wrap(~Area)

roughy2_sum

#average SL by Area and sex
ggplot(roughy2_sum,
       aes(x = year,y = Length, shape =sex,colour = Area)) +
  geom_errorbar(aes(ymin = Length - ci, ymax= Length + ci))+
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

       