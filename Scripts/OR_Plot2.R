library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)
library(Rmisc)
library(dplyr)

#read in the tidy data from results
Roughy <- read_csv("Results/OREastern_tidy2019.csv")

#rename and select relevant columns
Roughy1 <- Roughy %>% 
  mutate(Length = SLadj_cm) %>% 
  mutate(Area = Area_MS) %>% 
  drop_na(Area) %>% 
  drop_na(Length) %>% 
  drop_na(sex) %>% 
  select(Area, year, sex, Length)


Roughycheck <- Roughy %>% 
  filter(year == 1996)%>% 
  dplyr::group_by(Area,sex) %>% 
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


#summarise data: using summarySE package Rmisc (different method to acheive
#ouyvomr for Roughy_sumstats)
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
pd <- position_dodge(0.6) # move position horizontally b/c of overlap
ggplot(roughy2_sum, aes(x = year,y = Length, shape = Area, colour = sex)) +
  geom_errorbar(aes(ymin = Length - ci, ymax= Length + ci),
                width =.7, position = pd) +
  geom_point( position = pd, size = 2)+
  geom_line(position = pd) +
  xlab("Year")+
  ylab("Average Standard Length (cm)")+
  scale_y_continuous(breaks = seq(30,38,1),limits = c(30,38))+
  scale_x_continuous(limits = c(1986,2020), breaks = seq(1986,2020,2))+
  theme_bw()+
  theme(legend.position = c(1,0),legend.justification = c(1,0)) #legend bottom right
 
# ggboxplot
ggboxplot(Roughy1, x ="year", y= "Length", group = "sex",fill = "sex",
          outlier.colour = NULL)+
  theme(axis.text.x= element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 8))+
  facet_wrap(~Area)


  
#boxplot
ggplot(Roughy,aes(x= as.character(year), y = Length)) +
  geom_boxplot(aes(colour = sex, fill = sex, outlier.colour = sex))+
  theme_bw()+
  theme(axis.text.x= element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 8))+
  stat_summary(fun.y = mean, aes(group = sex), geom = "point",
              position = position_dodge(.9),size = 1) +
  geom_hline(yintercept = 36.9, linetype ="dashed",colour= "red")+
  geom_hline(yintercept = 34.7, linetype ="dashed",colour= "red")+
  geom_hline(yintercept = 35.2, linetype ="dashed",colour= "#339999")+
  geom_hline(yintercept = 32.2, linetype = "dashed",colour ="#339999")+
  facet_wrap(~Area_MS)+
  xlab("Year")+
  ylab("Standard Length (cm)")+
  theme(legend.position = c(1,0),legend.justification = c(1,0)) #legend bottom right
 
roughy2_sum %>% 
  dplyr::group_by(sex) %>% 
  dplyr::summarise(max(Length),min(Length))


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

       