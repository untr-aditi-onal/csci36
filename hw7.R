## --------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## --------------------------------------------------------------------------------------------------------------------------------------
gss_cat


## --------------------------------------------------------------------------------------------------------------------------------------
by_religon <- gss_cat$relig%>%
  fct_count(gss_cat$relig)%>%
  rename(relig=f)%>%
  arrange(n)
by_religon


## --------------------------------------------------------------------------------------------------------------------------------------
gss_cat_n<-gss_cat%>%
  full_join(by_religon)
gss_cat_n


## --------------------------------------------------------------------------------------------------------------------------------------
gss_cat_by_religon<-gss_cat_n%>%
  mutate(relig=fct_reorder(relig,desc(n)))%>%
  select(-n)%>%
  arrange(relig)
gss_cat_by_religon


## --------------------------------------------------------------------------------------------------------------------------------------
levels(gss_cat$marital)


## --------------------------------------------------------------------------------------------------------------------------------------
gss_cat2a<-gss_cat%>%
  mutate(marital=fct_collapse(marital,
  "Other" = c("No answer", "Never married"),
  "Previously Married" = c("Divorced", "Separated", "Widowed"),
  "Currently Married" = c("Married")
  ))%>%
  count(marital)
gss_cat2a


## --------------------------------------------------------------------------------------------------------------------------------------
gss_cat2b<-gss_cat2a%>%
  mutate(marital=fct_relevel(marital, "Currently Married", "Previously Married", "Other"))%>%
  arrange(marital)
gss_cat2b


## --------------------------------------------------------------------------------------------------------------------------------------
gss_low_high<-gss_cat%>%
  mutate(low=str_extract(rincome,"(?<=\\$)\\d+(?= )")) %>% 
  mutate(high=str_extract(rincome,"\\d+(?=$)")) %>%
  filter(!is.na(high))%>%
  filter(!is.na(low))
gss_low_high


## --------------------------------------------------------------------------------------------------------------------------------------
gss_mid<-gss_low_high %>% 
  mutate(low=parse_integer(low))%>%
  mutate(high=parse_integer(high))%>%
  mutate(mid=(low+high)/2)
gss_mid


## --------------------------------------------------------------------------------------------------------------------------------------
gss_party_by_income<-gss_mid%>%
  mutate(partyid=fct_reorder(partyid, mid))%>%
  arrange(partyid)%>%
  select(partyid, rincome)
gss_party_by_income

