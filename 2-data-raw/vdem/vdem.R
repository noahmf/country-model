
## Setup

source("./setup.R")

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_csv(path(path_data, "/vdem/Country_Year_V-Dem_Full+others_CSV_v12/V-Dem-CY-Full+Others-v12.csv"))

## Process

data_ans1 <- data_raw1 %>% 
  mutate(
    obsid=paste(base$country[match(country_name, base$country_altname)], year, sep=""),
    vdem=(v2x_polyarchy+v2x_libdem+v2x_partipdem+v2x_delibdem+v2x_egaldem)/5,
    vdem_libdem=as.numeric(v2x_libdem),
    vdem_liberal=as.numeric(v2x_liberal),
    vdem_suffrage=as.numeric(v2x_suffr),
    edu_mnyr_15pl=as.numeric(e_peaveduc),
  ) %>% 
  filter((obsid=="ISRL1948"&country_name=="Palestine/British Mandate")==F) %>% 
  select(obsid, vdem, vdem_libdem, vdem_liberal, vdem_suffrage, edu_mnyr_15pl)

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
      unit=ifelse(variable%in%c("vdem", "vdem_libdem", "vdem_liberal", "vdem_suffrage"), "na", ifelse(variable=="edu_mnyr_15pl", "yrs", NA)),
      source="vdem", 
      dataset="vdem",
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

write_csv(data_out, path(path_raw_out, "/vdem.csv"))
