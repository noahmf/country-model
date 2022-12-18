
## Setup

source("./setup.R")

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_csv(path(path_data, "/boe/amomdftuk21sep2021/POPUKA.csv"))

## Process

data_ans1 <- data_raw1 %>% 
  mutate(
    obsid=paste("UKGB", substr(DATE,1,4), sep=""),
    pop=round(as.numeric(POPUKA*1000),0),
  ) %>% 
  mutate(
    pop_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((pop/lag(pop,1))-1)*100,NA),
  ) %>% 
  select(obsid, pop, pop_r)

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
      unit=ifelse(variable=="pop", "capita", ifelse(variable=="pop_r", "perc", NA)),
      source="boe", 
      dataset="amomdftuk21sep2021",
      type="observation",
      geography="modern",
      permission="public",
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

data_ans1B <- data_ans1 %>% clean1

data_out <- data_ans1B

## Write

write_csv(data_out, path(path_raw_out, "/boe.csv"))
