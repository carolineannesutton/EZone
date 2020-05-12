#Age data from Eastern zone OR

#load library
library(tidyverse)
library(dplyr)
library(Rmisc)

Age <- read_csv("Data/ORAge87to16.csv")
Age_new <- read_csv("Data/ORAge_2019.csv")


#select fields from OR Age series
Age_1 <- Age %>% 
  filter(month == "Jul") %>% # ensuring spawning population
  select(Zone, Ground = Location1, Year, month, Sex= Sex_no, Length_cm = Length_1, Age,O=`L_Otolith wgt`, Mark =`Acoustic Group/ Aditional comment`) %>% 
  filter(Sex >0) %>%  # only males and females
  filter(Sex <3) %>%  # only males and females
  filter(Age > 1)%>%     # only readable otoliths (-99 represents unreadable otoliths)
  filter(Mark != "BS") %>% 
  filter(Mark != "BP")


#
Age_1 %>% 
  dplyr::group_by(Sex,Year) %>% 
  dplyr::summarise(mean(O,na.rm = TRUE),mean(Age,na.rm = TRUE))

Age_new_1 <- Age_new %>% 
  select(Zone, Ground =`Location / port descriptor`, Year, month, Sex,Length_mm =Length_1, O= `Oto_Wght (g)`) %>% 
  mutate(Length_cm = Length_mm/10) %>% 
  select(-Length_mm) %>% 
  #filter(Age >0) no age data currently

Age_new_1 %>% 
  dplyr::group_by(Sex,Year) %>% 
  dplyr::summarise(mean(O,na.rm = TRUE))

Age_2 <- Age_1 %>% 
  bind_rows(Age_new_1) %>% 
  mutate(Sex= as.factor(Sex)) %>% 
  mutate(Sex = case_when(Sex == "1"~"M", TRUE~as.character(Sex))) %>%
  mutate(Sex = case_when(Sex == "2"~"F", TRUE~as.character(Sex))) 

Age_2 %>% 
  dplyr::group_by(Ground,Sex) %>% 
  dplyr::summarise(mean(O,na.rm = TRUE))

O_Sum <- summarySE(Age_2, measurevar = "O", groupvars = c("Year","Ground","Sex"),na.rm = TRUE) 
Age_Sum <- summarySE(Age_2, measurevar = "Age", groupvars = c("Year","Ground","Sex"),na.rm = TRUE) 

pd <- position_dodge(0.5) # move position horizontally b/c of overlap

f2.7 <- ggplot(O_Sum, aes(x = Year,y = O, shape = Ground, colour = Sex)) +
  geom_errorbar(aes(ymin = O - ci, ymax= O + ci),
                width =.7, position = pd) +
  geom_point( position = pd, size = 2)+
  geom_line(position = pd) +
  xlab("Eastern Zone Spawning Population by year for July")+
  ylab("Average otolith weight (g)")+
  scale_y_continuous(breaks = seq(.05,0.3,.05),limits = c(0,0.28))+
  scale_x_continuous(limits = c(1986,2020), breaks = seq(1986,2020,2))+
  theme_bw()+
  theme(legend.position = c(.25, .05),
        legend.justification = c(1,0),
        legend.text =element_text(size=8),
        legend.title = element_text(size = 9, margin = margin(t=.05)),
        legend.spacing.y =unit(.05,'cm'),
        plot.background = element_rect(linetype =  "solid") )


f2.8 <- ggplot(Age_Sum, aes(x = Year,y = Age, shape = Ground, colour = Sex)) +
  geom_errorbar(aes(ymin = Age - ci, ymax= Age + ci),
                width =.7, position = pd) +
  geom_point( position = pd, size = 2)+
  geom_line(position = pd) +
  xlab("Eastern Zone Spawning Population by year for July")+
  ylab("Average age (years)")+
  scale_y_continuous(breaks = seq(20,70,10),limits = c(20,70))+
  scale_x_continuous(limits = c(1986,2020), breaks = seq(1986,2020,2))+
  theme_bw()+
  theme(legend.position = c(.25, .05),
        legend.justification = c(1,0),
        legend.text =element_text(size=8),
        legend.title = element_text(size = 9, margin = margin(t=.05)),
        legend.spacing.y =unit(.05,'cm'),
        plot.background = element_rect(linetype =  "solid") )

plot(f2.7)
plot(f2.8)
ggsave("Figures/f2.7.emf")
ggsave("Figures/f2.7.jpg")
