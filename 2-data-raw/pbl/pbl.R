
## Setup

source("./setup.R")

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_csv(path(path_data, "/pbl/hyde3.2/baseline/txt/popc_c-edit.csv"))

## Process

data_ans1 <- data_raw1 %>% 
  filter(region != "Total") %>% 
  rename(`1`=`0`) %>% 
  select(region, `1`:`2017`) %>% 
  pivot_longer(!region, names_to = "year", values_to = "pop") %>% filter(pop>0) %>% 
  
  mutate(region=ifelse(region==736, 729, region)) %>% #FIX SUDAN
  mutate(region=ifelse(region==891, 688, region)) %>% #FIX SERBIA
  mutate(region=ifelse(region==530, 535, region)) %>% #FIX CARIBBEAN NETHERLANDS
  
  mutate(
    obsid=paste(base$country[match(region, base$country_un)], ifelse(as.numeric(year)>999, year, ifelse(as.numeric(year)>99, paste("0", as.numeric(year), sep=""), ifelse(as.numeric(year)>9, paste("00", year, sep=""), paste("000", year, sep="")))), sep=""),
    pop=round(as.numeric(pop),0),
  ) %>% 
  group_by(region) %>% 
  mutate(
    pop_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((pop/lag(pop,1))-1)*100,NA),
  ) %>% 
  ungroup() %>% 
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
      source="pbl", 
      dataset="hyde3.2",
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

write_csv(data_out, path(path_raw_out, "/pbl.csv"))
