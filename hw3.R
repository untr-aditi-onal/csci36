## --------------------------------------------------------------------------------------------------------------------------------------
library("tidyverse")
starwars


## --------------------------------------------------------------------------------------------------------------------------------------
bio <- starwars %>% 
  select (name,height,mass) %>% 
  filter(!is.na(height), !is.na(mass))%>% 
  mutate(ratio=(height/mass)) %>% 
  arrange(ratio)
bio


## --------------------------------------------------------------------------------------------------------------------------------------
eyes<-starwars %>%
  group_by(eye_color) %>%
  summarize(number=n()) %>%
  arrange(desc (number)) 
eyes


## --------------------------------------------------------------------------------------------------------------------------------------
sex_mass <-starwars %>% 
  group_by(sex)%>% 
  filter(!is.na (sex))%>% 
  summarize(min_mass = min(mass, na.rm = TRUE),max_mass = max(mass, na.rm = TRUE), mass_diff=(max_mass-min_mass)) %>% 
  arrange(desc(mass_diff))
sex_mass


## --------------------------------------------------------------------------------------------------------------------------------------
brown <-starwars %>% 
  group_by(homeworld) %>% 
  summarize(all_brown=all(eye_color== "brown"))  
brown


## --------------------------------------------------------------------------------------------------------------------------------------
Human_worlds <- starwars %>% 
  group_by(homeworld) %>%  
  filter(species == "Human") %>%
  summarize(number=sum(species == "Human")) %>%
  filter(number>1)
Human_worlds 


## --------------------------------------------------------------------------------------------------------------------------------------
tallest <- starwars %>% 
  group_by(homeworld) %>% 
  filter(rank(desc(height), ties.method ="first")==1) %>% 
  arrange(desc(height)) %>%   
  select(name,height)
tallest


## --------------------------------------------------------------------------------------------------------------------------------------
tallest2 <- starwars %>% 
  group_by(homeworld) %>% 
  filter(rank(desc(height))==2) %>% 
  arrange(desc(height)) %>%
  select(name)
tallest2


## --------------------------------------------------------------------------------------------------------------------------------------
shortest3_avg<- starwars %>%
  group_by(homeworld) %>%
  filter(!is.na(homeworld)) %>%
  slice(height, n=3) %>%
  summarize(avg=mean(height)) %>%
  arrange(avg) %>%
  select(homeworld, avg)
shortest3_avg

