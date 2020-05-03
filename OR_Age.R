#Age data from Eastern zone OR

#load library
library(tidyverse)
library(dplyr)

Age <- read_csv("Data/ORAge87to16.csv")
Age_new <- read_csv("Data/ORAge_2019.csv")


Age_1 <- Age %>% 
  filter(month == "Jul") %>% 
  select(Zone, Ground = Location1, Year, month, Sex= Sex_no, Length = Length_1, O=`L_Otolith wgt`) %>% 
  filter(Sex >0) %>% 
  filter(Sex <3)

Age_1 %>% 
  dplyr::group_by(Sex) %>% 
  dplyr::summarise(mean(O,na.rm = TRUE))

Age_new_1 <- Age_new %>% 
  select(Zone, Ground =`Location / port descriptor`, Year, month, Sex,Length_mm =Length_1, O= `Oto_Wght (g)`) %>% 
  mutate(Length_cm = Length_mm/10) %>% 
  select(-Length_mm)

Age_new_1 %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(mean(O,na.rm = TRUE))

Age_2 <- Age_1 %>% 
  bind_rows(Age_new_1) %>% 
  mutate(Sex= as.factor(Sex)) %>% 
  mutate(Sex = case_when(Sex == "1"~"M", TRUE~as.character(Sex))) %>%
  mutate(Sex = case_when(Sex == "2"~"F", TRUE~as.character(Sex))) 

Age_2 %>% 
  dplyr::group_by(Ground,Sex) %>% 
  dplyr::summarise(mean(O,na.rm = TRUE))

Age_Sum <- summarySE(Age_2, measurevar = "O", groupvars = c("Year","Ground","Sex"),na.rm = TRUE)   

pd <- position_dodge(0.5) # move position horizontally b/c of overlap

f2.7 <- ggplot(Age_Sum, aes(x = Year,y = O, shape = Ground, colour = Sex)) +
  geom_errorbar(aes(ymin = O - ci, ymax= O + ci),
                width =.7, position = pd) +
  geom_point( position = pd, size = 2)+
  geom_line(position = pd) +
  xlab("Eastern Zone Spawning Population by year for July")+
  ylab("Average otolith weight (g)")+
  scale_y_continuous(breaks = seq(0,0.3,.04),limits = c(0,0.28))+
  scale_x_continuous(limits = c(1986,2020), breaks = seq(1986,2020,2))+
  theme_bw()+
  theme(legend.position = c(.15, .05),
        legend.justification = c(1,0),
        legend.text =element_text(size=8),
        legend.title = element_text(size = 9, margin = margin(t=.05)),
        legend.spacing.y =unit(.05,'cm'),
        plot.background = element_rect(linetype =  "solid") )


plot(f2.7)
ggsave("Figures/f2.7.emf")
ggsave("Figures/f2.7.jpg")
