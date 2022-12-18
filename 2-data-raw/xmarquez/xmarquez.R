
## Setup

source("./setup.R")
library(democracyData)

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- generate_extended_uds() %>% 
  mutate(country=base$country[match(extended_country_name, base$country_altname)])

## Process

data_ans1 <- data_raw1 %>% 
  mutate(
    obsid=paste(country, year, sep=""),
    uds=as.numeric(z1_adj_as_prob),
  ) %>% 
  filter(
    (extended_country_name=="Czechoslovakia"&year>1992)==F,
    (obsid=="GRMN1945"&uds>0.12)==F,
    (obsid=="GRMN1990"&uds<.81)==F,
    (extended_country_name=="Korea, Republic of"&year==1910)==F,
    (substr(obsid,1,4)=="YEMN"&uds<0.03&year<1851)==F,
    (obsid=="YEMN1990"&uds<.18)==F,
  ) %>% 
  select(obsid, uds)

clean1 <- function(object){
  object1 <- object %>% 
    arrange(obsid) %>% 
    pivot_longer(!obsid, names_to = "variable", values_to = "value") %>% 
    drop_na(value) %>% 
    
    mutate(
      country=substr(obsid,1,4),
      country_name=base$country_name[match(country, base$country)],
      year=as.numeric(substr(obsid,5,8)),
      time=specify_decimal(year+.5,3),
      obsid_time=paste(country, time, sep=""),
      unit="na",
      source="xmarquez", 
      dataset="uds",
      type="observation",
      geography="historical",
      permission="public",
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
  
}

data_ans1B <- data_ans1 %>% clean1

data_out <- data_ans1B %>% 
  filter(substr(country,1,3)%notin%c("NA1", "NA2")) #GET RID OF NAs; NEED TO GO BACK AND DEAL WITH THESE

## Write

write_csv(data_out, path(path_raw_out, "/xmarquez.csv"))
