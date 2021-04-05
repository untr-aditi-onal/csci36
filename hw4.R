## --------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## ---- warning = FALSE, message = FALSE-------------------------------------------------------------------------------------------------
rf1 <- read_csv2('City;January;February;March
  Atlanta, Georgia;11;10;10
  Austin, Texas;7;7;9
  Baltimore, Maryland;10;9;10
  Birmingham, Alabama;11;10;10
  Boston, Massachusetts;11;10;12')

rf1


## --------------------------------------------------------------------------------------------------------------------------------------
tidy_rf1<-rf1 %>% 
  separate(City, into = c("City", "State"))%>% 
  pivot_longer(cols=c('January':'March'), names_to = "Month", values_to = "Rainfall")%>%
  mutate(Rainfall=as.integer(Rainfall))
tidy_rf1 


## --------------------------------------------------------------------------------------------------------------------------------------
City_Averages <-tidy_rf1 %>% 
  group_by(City) %>% 
  summarize(Avg_Rainfall=mean(Rainfall)) 
City_Averages


## --------------------------------------------------------------------------------------------------------------------------------------
tidy_rf1_with_dates<-tidy_rf1 %>%
  mutate(Day=1,Year=2007)%>%
  unite(col=Date,Month,Day,Year,sep="-")%>%
  mutate(Date=parse_date(Date,"%B-%d-%Y"))
tidy_rf1_with_dates


## --------------------------------------------------------------------------------------------------------------------------------------
tidy_rent <- us_rent_income %>%
  select(-GEOID,-moe) %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>%
  mutate (RTI=rent/income) %>%
  arrange(RTI)
tidy_rent 


## --------------------------------------------------------------------------------------------------------------------------------------
race<-read_csv("Name,   50, 100, 150, 200, 250, 300, 350\n Carla,   1.2,   1.8,   2.2,   2.3,   3,     2.5,   1.8\n    Mace,    1.5,   1.1,   1.9,   2,     3.6,   3,     2.5\n    Lea,     1.7,   1.6,   2.3,   2.7,   2.6,   2.2,   2.6\n Karen,   1.3,   1.7,   1.9,   2.2,   3.2,   1.5,   1.9")

race


## --------------------------------------------------------------------------------------------------------------------------------------
tidy_race<-race %>%
  pivot_longer(cols=-Name, names_to= "Time", values_to = "Score") %>%
  mutate(Time=parse_integer(Time))
tidy_race


## --------------------------------------------------------------------------------------------------------------------------------------
results<-read_csv("results.csv")
results


## --------------------------------------------------------------------------------------------------------------------------------------
tidy_results<-results %>%
  mutate(Individ=parse_number(Individ))%>%
  pivot_wider(names_from = "Treatmnt",values_from = "value")
tidy_results  


## --------------------------------------------------------------------------------------------------------------------------------------
grades<-read_csv("grades.csv")
grades


## --------------------------------------------------------------------------------------------------------------------------------------
tidy_grades <- grades%>%
  separate("ID Test", into = c("ID", "Test"))%>% 
  pivot_longer(cols=c('Fall':'Winter'),names_to = "Quarter")%>%
  pivot_wider(names_from=Test,values_from=value)
tidy_grades  


## --------------------------------------------------------------------------------------------------------------------------------------
dates<-read_csv("dates.csv")
dates


## --------------------------------------------------------------------------------------------------------------------------------------
tidy_dates<-dates%>%
  pivot_wider(names_from="TT",values_from=number)%>%
  select(-starts_with('X'))%>%
  unite(col="Date",c("Month","Day","Year"),sep="-")%>%
  mutate(Date=parse_date(Date,"%m-%d-%Y"))
tidy_dates

