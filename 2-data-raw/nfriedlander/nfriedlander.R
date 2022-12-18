
## Setup

source("./setup.R")
library(sf)

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
map_raw1 <- st_read(path(path_data, "/nfriedlander/gis-country.csv")) %>% mutate(geometry=st_as_sfc(geometry, crs=4326), center=st_as_sfc(center, crs=4326))
data_raw1 <- read_csv(path(path_data, "/nfriedlander/wbg/1453a9ed-f83c-4b39-a437-5202ca72442f_Data edit/Sheet 2-1453a9ed-f83c-4b39-a437-5202ca72442f_Data edit.csv"))

## Process

map_ans1 <- map_raw1 %>% 
  mutate(
    country=paste(base$country[match(country_iso3, base$country_iso3)], sep=""),
  ) %>% 
  select(country, center, geometry) %>% 
  arrange(country)


data_ans1 <- data_raw1 %>% 
  select(`Country Code`, `1950`:`2021`) %>% 
  pivot_longer(!`Country Code`, names_to = "year", values_to = "gnp_gdp") %>% 
  mutate(
    obsid=paste(base$country[match(`Country Code`, base$country_iso3)], year, sep="")
  ) %>% 
  select(obsid, gnp_gdp)

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
      unit=ifelse(variable=="gnp_gdp", "ratio", NA),
      source="nfriedlander", 
      dataset="wbg",
      type="observation",
      geography="modern",
      permission="public",
    ) %>% 
    select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

data_ans1B <- data_ans1 %>% clean1()

## Write

write_csv({map_ans1 %>% rowwise() %>% mutate(geometry=st_as_text(geometry), center=st_as_text(center))}, path(path_out, "/map.csv"))
write_csv(data_ans1B, path(path_raw_out, "/nfriedlander.csv"))
