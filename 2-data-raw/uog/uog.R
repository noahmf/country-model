
## Setup

source("./setup.R")
library(readxl)

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_excel(path(path_data, "/uog/mpd2020/mpd2020.xlsx"), sheet=3)
data_raw2 <- read_excel(path(path_data, "/uog/pwt10.0/pwt100.xlsx"), sheet=3)

## Process

data_ans1 <- data_raw1 %>% 
  mutate(
    obsid=paste(base$country[match(countrycode, base$country_iso3)], year, sep=""),
    gdp_pc=gdppc*1.205, #src: https://fred.stlouisfed.org/series/CPIAUCSL (11 Jul 2022)
    pop=pop*1000,
  ) %>% 
  group_by(countrycode) %>% 
  mutate(
    gdp=gdp_pc*pop,
    gdp_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((gdp/lag(gdp,1))-1)*100,NA),
    gdp_pc_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((gdp_pc/lag(gdp_pc,1))-1)*100,NA),
    pop_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((pop/lag(pop,1))-1)*100,NA),
  ) %>% 
  ungroup() %>% 
  select(obsid, gdp_pc, pop, pop_r, gdp, gdp_r, gdp_pc_r)

data_ans2 <- data_raw2 %>% 
  rowwise() %>% 
  mutate(
    obsid=paste(base$country[match(countrycode, base$country_iso3)], year, sep=""),
    gdp=as.numeric(mean(rgdpe, rgdpo, cgdpe, cgdpo, na.rm=T))*1000000*1.205, #src: https://fred.stlouisfed.org/series/CPIAUCSL (11 Jul 2022)
    kap_gdp=rnna/rgdpna,
    kap_dep=delta*100,
    labsh=labsh*100,
    emp_pop=emp/pop,
    hrs_emp=avh,
    pwt_hci=hc,
  ) %>% 
  group_by(countrycode) %>% 
  mutate(
    gdp_pc=(gdp/pop)/1000000,
    gdp_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((gdp/lag(gdp,1))-1)*100,NA),
    gdp_pc_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((gdp_pc/lag(gdp_pc,1))-1)*100,NA),
    pop_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((pop/lag(pop,1))-1)*100,NA),
  ) %>% 
  ungroup() %>% 
  select(obsid, gdp, kap_dep, labsh, emp_pop, kap_gdp, gdp_pc, hrs_emp, pwt_hci, gdp_r, gdp_pc_r)

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
      unit=
        ifelse(variable%in%c("gdp", "gdp_pc"), "usd2021ppp",
        ifelse(variable%in%c("pop"), "capita",
        ifelse(variable%in%c("gdp_r", "gdp_pc_r", "pop_r"), "perc",
               NA))),
      source="uog", 
      dataset="mpd2020",
      type="observation",
      geography="modern",
      permission="public",
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

clean2 <- function(object){
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
      unit=
        ifelse(variable%in%c("gdp", "gdp_pc"), "usd2021ppp",
        ifelse(variable%in%c("emp_pop"), "capita",
        ifelse(variable%in%c("hrs_emp"), "hrs",
        ifelse(variable%in%c("pwt_hci", "kap_gdp"), "na",
        ifelse(variable%in%c("gdp_r", "gdp_pc_r", "pop_r", "kap_dep", "labsh"), "perc",
               NA))))),
      source="uog", 
      dataset="pwt10.0",
      type="observation",
      geography="modern",
      permission="public",
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

data_ans1B <- data_ans1 %>% clean1
data_ans2B <- data_ans2 %>% clean2

data_out <- bind_rows(data_ans1B, data_ans2B) %>% 
  filter(substr(country,1,3)%notin%c("NA1", "NA2")) #GET RID OF NAs; NEED TO GO BACK AND DEAL WITH THESE

## Write

write_csv(data_out, path(path_raw_out, "/uog.csv"))
