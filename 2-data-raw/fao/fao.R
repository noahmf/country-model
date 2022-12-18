
## Setup

source("./setup.R")

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_csv(path(path_data, "/fao/FAOSTAT_data_7-13-2022.csv"))

## Process

data_ans1 <- data_raw1 %>% 
  filter(Area!="China") %>% 
  rename(variable=Item) %>% 
  select(Area, variable, Value) %>% 
  mutate(
    country=paste(base$country[match(Area, base$country_altname)], sep=""),
    value=Value*10, #hectares to km2
  ) %>% 
  select(country, variable, value)

clean1 <- function(object){
  object1 <- object %>% 
    arrange(country) %>% 
    drop_na(value) %>% 
    
    mutate(
      country_name=base$country_name[match(country, base$country)],
      variable=ifelse(variable=="Land area", "land", ifelse(variable=="Cropland", "cropland", ifelse(variable=="Forest land", "forest_land", NA))),
      unit="km2",
      source="fao", 
      dataset="fao",
      type="observation",
      geography="modern",
      permission="public",
    ) %>% 
    select(country, country_name, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

data_ans1B <- data_ans1 %>% clean1

data_out <- data_ans1B

## Write

write_csv(data_out, path(path_raw_out, "/fao-static.csv"))
