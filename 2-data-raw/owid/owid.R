
## Setup

source("./setup.R")

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_csv(path(path_data, "/owid/annual-co2-emissions-per-country.csv"))
data_raw2 <- read_csv(path(path_data, "/owid/owid-energy-data.csv"))
#data_raw3 <- read_csv(path(path_data, "/owid/military-expenditure-as-a-share-of-gdp.csv"))
data_raw4 <- read_csv(path(path_data, "/owid/number-of-homicide-deaths.csv"))
data_raw5 <- read_csv(path(path_data, "/owid/population-breakdown-by-highest-level-of-education-achieved-for-those-aged-15-in.csv"))

## Process

data_ans1 <- data_raw1 %>% 
  filter(Code!="OWID_WRL") %>% 
  drop_na(Code) %>% 
  mutate(
    obsid=paste(base$country[match(Entity, base$country_altname)], Year, sep=""),
    co2=`Annual CO2 emissions`
  ) %>% 
  select(obsid, co2) %>% 
  drop_na(co2)

data_ans2 <- data_raw2 %>% 
  mutate(
    obsid=paste(base$country[match(country, base$country_altname)], year, sep=""),
    energy_pc=energy_per_capita*0.0036, #KwH to GJ
  ) %>% 
  select(obsid, energy_pc) %>% 
  drop_na(energy_pc) %>% filter(substr(obsid,1,3)%notin%c("NA1", "NA2"))

#data_ans3 <- data_raw3 %>% 
  #mutate(
    #obsid=paste(base$country[match(Entity, base$country_altname)], Year, sep=""),
    #milex_perc=as.numeric(`Military expenditure as a share of GDP (OWID calculated based on NMC, COW & SIPRI (2017))`),
  #) %>% 
  #select(obsid, milex_perc) %>% 
  #drop_na(milex_perc)

data_ans4 <- data_raw4 %>% 
  mutate(
    obsid=paste(base$country[match(Entity, base$country_altname)], Year, sep=""),
    homicide=as.numeric(`Deaths - Interpersonal violence - Sex: Both - Age: All Ages (Number)`),
  ) %>% 
  select(obsid, homicide) %>% 
  drop_na(homicide)

data_ans5 <- data_raw5 %>% 
  mutate(
    obsid=paste(base$country[match(Entity, base$country_altname)], Year, sep=""),
    edu_tertiary_share=(`Post-secondary education, 1970-2050 (IIASA (2015))`/(`Post-secondary education, 1970-2050 (IIASA (2015))`+`Secondary education, 1970-2050 (IIASA (2015))`+`Primary education, 1970-2050 (IIASA (2015))`+`No education, 1970-2050 (IIASA (2015))`))*100,
  ) %>% 
  select(obsid, edu_tertiary_share) %>% 
  drop_na(edu_tertiary_share)

cleanX <- function(object, unit1, source1, dataset1, type1, geography1, permission1){
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
      unit=unit1,
      source=source1, 
      dataset=dataset1,
      type=type1,
      geography=geography1,
      permission=permission1,
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

clean2 <- function(object, unit1, source1, dataset1, type1, geography1, permission1){
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
      unit="perc",
      source="owid", 
      dataset="iiasa",
      type=ifelse(year<=2015, "observation", "forecast"),
      geography="modern",
      permission="public",
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

data_ans1B <- data_ans1 %>% cleanX(unit1="tonne", source1="owid", dataset1="owid", type1="observation", geography1="modern", permission1="public")
data_ans2B <- data_ans2 %>% cleanX(unit1="gj", source1="owid", dataset1="owid", type1="observation", geography1="historical", permission1="public")
#data_ans3B <- data_ans3 %>% cleanX(unit1="perc", source1="owid", dataset1="owid", type1="observation", geography1="historical", permission1="public") %>% 
  #filter(substr(country,1,3)%notin%c("NA1", "NA2")) #GET RID OF NAs; NEED TO GO BACK AND DEAL WITH THESE
data_ans4B <- data_ans4 %>% 
  cleanX(unit1="capita", source1="owid", dataset1="ihme", type1="observation", geography1="modern", permission1="public") %>% 
  filter(substr(country,1,3)%notin%c("NA1", "NA2")) #GET RID OF NAs
data_ans5B <- data_ans5 %>% clean2

data_out <- bind_rows(data_ans1B, data_ans2B, data_ans3B, data_ans4B, data_ans5B)

## Write

write_csv(data_out, path(path_raw_out, "/owid.csv"))
