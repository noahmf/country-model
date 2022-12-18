
## Setup

source("./setup.R")
library(readxl)

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_csv(path(path_data, "/gapminder/gd001-v27/GM-GDP per capita - Dataset - v27 - data-for-countries-etc-by-year.csv"))
#data_raw2 <- read_excel(path(path_data, "/gapminder/gd003-v6/_GM-Population - Dataset - v6.xlsx"), sheet=4)
data_raw3 <- read_csv(path(path_data, "/gapminder/gd004-v11/GM-Life Expectancy- Dataset - v11 - data-for-countries-etc-by-year.csv"))
data_raw4 <- read_csv(path(path_data, "/gapminder/gd005-v11/GM-Child Mortality - Dataset - v11 - data-for-countries-etc-by-year.csv"))
data_raw5 <- read_excel(path(path_data, "/gapminder/gd008-v12/tfr-by-gapminder.xlsx"), sheet=2)
#data_raw6 <- read_excel(path(path_data, "/gapminder/gini-v3/_Gini Data - v3 - by Gapminder.xlsx"), sheet=4)
data_raw7 <- read_excel(path(path_data, "/gapminder/incmountains-v2/Detailed Data - Income Mountains-by-Gapminder-v2-20180504-edit.xlsx"), sheet=4, skip=1)

## Process

data_ans1 <- data_raw1 %>% 
  mutate(
    obsid=paste(base$country[match(name, base$country_altname)], time, sep=""),
    gdp=`GDP total`*1.105, gdp_pc=`Income per person`*1.105, #src: https://fred.stlouisfed.org/series/CPIAUCSL (11 Jul 2022)
  ) %>% #, gdp_pc_r=`GDP per capita growth (%)`
  group_by(name) %>% 
  mutate(
    gdp_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((gdp/lag(gdp,1))-1)*100,NA),
    gdp_pc_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((gdp_pc/lag(gdp_pc,1))-1)*100,NA),
  ) %>% 
  ungroup() %>% 
  select(obsid, gdp, gdp_pc, gdp_pc_r, gdp_r)

#data_ans2 <- data_raw2 %>% 
  #select(name:Population) %>% 
  #mutate(
    #obsid=paste(base$country[match(name, base$country_altname)], time, sep=""),
    #pop=round(as.numeric(Population),0),
  #) %>% 
  #group_by(name) %>% 
  #mutate(
    #pop_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((pop/lag(pop,1))-1)*100,NA),
  #) %>% 
  #ungroup() %>% 
  #select(obsid, pop, pop_r)

data_ans3 <- data_raw3 %>% 
  mutate(
    obsid=paste(base$country[match(name, base$country_altname)], time, sep=""),
    lexb=`Life expectancy`,
  ) %>% 
  select(obsid, lexb)

data_ans4 <- data_raw4 %>% 
  mutate(
    obsid=paste(base$country[match(name, base$country_altname)], time, sep=""),
    cmr=`Child mortality`,
  ) %>% 
  select(obsid, cmr)

data_ans5 <- data_raw5 %>% 
  select(geo.name, `1800`:`2100`) %>% 
  pivot_longer(!geo.name, names_to = "year", values_to = "tfr") %>% 
  mutate(
    obsid=paste(base$country[match(geo.name, base$country_altname)], year, sep=""),
  ) %>% 
  select(obsid, tfr)

#data_ans6 <- data_raw6 %>% 
  #select(name, time, Gini) %>% 
  #mutate(
    #obsid=paste(base$country[match(name, base$country_altname)], time, sep=""),
    #gini=as.numeric(Gini),
  #) %>% 
  #select(obsid, gini)

data_ans7 <- data_raw7 %>% 
  select(name, year, is_0_2:is_200_plus) %>% 
  mutate(
    obsid=paste(base$country[match(name, base$country_altname)], year, sep=""),
  ) %>% 
  select(obsid, is_0_2:is_200_plus)

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
        ifelse(variable%in%c("pop", "tfr"), "capita",
        ifelse(variable%in%c("gdp", "gdp_pc"), "usd2021ppp",
        ifelse(variable%in%c("gdp_pc_r", "gdp_r", "pop_r"), "perc",
        ifelse(variable%in%c("lexb"), "yrs",
        ifelse(variable%in%c("cmr"), "perm",
        ifelse(variable%in%c("gini"), "na",
        ifelse(variable%in%c("is_0_2", "is_2_6", "is_6_20", "is_20_60", "is_60_200", "is_200_plus"), "perc",
               NA))))))),
      source="gapminder", 
      dataset=
        ifelse(variable%in%c("gdp", "gdp_pc", "gdp_pc_r", "gdp_r"), "gd001-v27",
        ifelse(variable%in%c("pop", "pop_r"), "gd003-v6",
        ifelse(variable%in%c("lexb"), "gd004-v11",
        ifelse(variable%in%c("cmr"), "gd005-v11",
        ifelse(variable%in%c("tfr"), "gd008-v12",
        ifelse(variable%in%c("gini"), "gini-v3",
        ifelse(variable%in%c("is_0_2", "is_2_6", "is_6_20", "is_20_60", "is_60_200", "is_200_plus"), "incmountains-v2",
               NA))))))),
      type=ifelse(year<=2019, "observation", "forecast"),
      geography="modern",
      permission="public",
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
  
}

data_ans1B <- data_ans1 %>% clean1()
#data_ans2B <- data_ans2 %>% clean1()
data_ans3B <- data_ans3 %>% clean1()
data_ans4B <- data_ans4 %>% clean1()
data_ans5B <- data_ans5 %>% clean1()
#data_ans6B <- data_ans6 %>% clean1()
data_ans7B <- data_ans7 %>% clean1()

data_out <- bind_rows(data_ans1B, data_ans3B, data_ans4B, data_ans5B, data_ans7B)

## Write

write_csv(data_out, path(path_raw_out, "/gapminder.csv"))
