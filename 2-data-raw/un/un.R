
## Setup

source("./setup.R")

## Read

base <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv"))
data_raw1 <- read_csv(path(path_data, "/un/wpp2022/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT-edit-sheet1.csv"))
data_raw2 <- read_csv(path(path_data, "/un/wpp2022/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT-edit-sheet2.csv"))
data_raw3 <- read_csv(path(path_data, "/un/wpp2022/WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES-edit-sheet1.csv"))
data_raw4 <- read_csv(path(path_data, "/un/wpp2022/WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES-edit-sheet2.csv"))

## Process

data_ans1 <- data_raw1 %>% 
  filter(`Location code`<900) %>% 
  rename(country=`Region, subregion, country or area *`, year=Year, pop=`Total Population, as of 1 July (thousands)`, age_mdn=`Median Age, as of 1 July (years)`, births=`Births (thousands)`, tfr=`Total Fertility Rate (live births per woman)`, age_mean_chldbear=`Mean Age Childbearing (years)`, deaths=`Total Deaths (thousands)`, lexb=`Life Expectancy at Birth, both sexes (years)`, imr=`Infant Mortality Rate (infant deaths per 1,000 live births)`, cmr=`Under-Five Mortality (deaths under age 5 per 1,000 live births)`, netmigs=`Net Number of Migrants (thousands)`) %>% 
  dplyr::select(country, year, pop, age_mdn, births, tfr, age_mean_chldbear, deaths, lexb, imr, cmr, netmigs) %>% 
  mutate(
    obsid=paste(base$country[match(country, base$country_altname)], year, sep=""),
    pop=pop*1000, births=births*1000, deaths=deaths*1000, netmigs=netmigs*1000,
  ) %>% 
  group_by(country) %>% 
  mutate(
    pop_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((pop/lag(pop,1))-1)*100,NA),
  ) %>% 
  ungroup() %>% 
  dplyr::select(obsid, pop, pop_r, age_mdn, births, tfr, age_mean_chldbear, deaths, lexb, imr, cmr, netmigs)

data_ans2 <- data_raw2 %>% 
  filter(`Location code`<900) %>% 
  rename(country=`Region, subregion, country or area *`, year=Year, pop=`Total Population, as of 1 July (thousands)`, age_mdn=`Median Age, as of 1 July (years)`, births=`Births (thousands)`, tfr=`Total Fertility Rate (live births per woman)`, age_mean_chldbear=`Mean Age Childbearing (years)`, deaths=`Total Deaths (thousands)`, lexb=`Life Expectancy at Birth, both sexes (years)`, imr=`Infant Mortality Rate (infant deaths per 1,000 live births)`, cmr=`Under-Five Mortality (deaths under age 5 per 1,000 live births)`, netmigs=`Net Number of Migrants (thousands)`) %>% 
  dplyr::select(country, year, pop, age_mdn, births, tfr, age_mean_chldbear, deaths, lexb, imr, cmr, netmigs) %>% 
  mutate(
    obsid=paste(base$country[match(country, base$country_altname)], year, sep=""),
    pop=pop*1000, births=births*1000, deaths=deaths*1000, netmigs=netmigs*1000,
  ) %>% 
  group_by(country) %>% 
  mutate(
    pop_r=ifelse(as.numeric(lag(substr(obsid,5,8),1))==as.numeric(substr(obsid,5,8))-1,((pop/lag(pop,1))-1)*100,NA),
  ) %>% 
  ungroup() %>% 
  dplyr::select(obsid, pop, pop_r, age_mdn, births, tfr, age_mean_chldbear, deaths, lexb, imr, cmr, netmigs)

data_ans3 <- data_raw3 %>% 
  filter(`Location code`<900) %>% 
  rename(country=`Region, subregion, country or area *`, year=Year) %>% 
  dplyr::select(country, year, `0-4`:`100`) %>% 
  mutate(
    obsid=paste(base$country[match(country, base$country_altname)], year, sep=""),
    pop0_4=(`0-4`)*1000,
    pop5_14=(`5-9`+`10-14`)*1000,
    pop15_24=(`15-19`+`20-24`)*1000,
    pop25_39=(`25-29`+`30-34`+`35-39`)*1000,
    pop40_64=(`40-44`+`45-49`+`50-54`+`55-59`+`60-64`)*1000,
    pop65_74=(`65-69`+`70-74`)*1000,
    pop75_plus=(`75-79`+`80-84`+`85-89`+`90-94`+`95-99`+`100`)*1000,
  ) %>% 
  dplyr::select(obsid, pop0_4:pop75_plus)

data_ans4 <- data_raw4 %>% 
  filter(`Location code`<900) %>% 
  rename(country=`Region, subregion, country or area *`, year=Year) %>% 
  dplyr::select(country, year, `0-4`:`100`) %>% 
  mutate(
    obsid=paste(base$country[match(country, base$country_altname)], year, sep=""),
    pop0_4=(`0-4`)*1000,
    pop5_14=(`5-9`+`10-14`)*1000,
    pop15_24=(`15-19`+`20-24`)*1000,
    pop25_39=(`25-29`+`30-34`+`35-39`)*1000,
    pop40_64=(`40-44`+`45-49`+`50-54`+`55-59`+`60-64`)*1000,
    pop65_74=(`65-69`+`70-74`)*1000,
    pop75_plus=(`75-79`+`80-84`+`85-89`+`90-94`+`95-99`+`100`)*1000,
  ) %>% 
  dplyr::select(obsid, pop0_4:pop75_plus)

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
        ifelse(variable%in%c("pop", "births", "deaths", "netmigs", "pop0_4", "pop5_14", "pop15_24", "pop25_39", "pop40_64", "pop65_74", "pop75_plus"), "capita",
        ifelse(variable%in%c("age_mdn", "age_mean_chldbear", "lexb"), "yrs",
        ifelse(variable%in%c("imr", "cmr"), "perm",
        ifelse(variable%in%c("pop_r"), "perc",
        ifelse(variable%in%c("tfr"), "capita",
               NA))))),
      source="un", 
      dataset="wpp2022",
      type=ifelse(year<=2022, "observation", "forecast"),
      geography="modern",
      permission="public",
    ) %>% 
    dplyr::select(obsid, country, country_name, year, time, obsid_time, variable, value, unit, source, dataset, type, geography, permission)
  
  return(object1)
}

data_ans1B <- data_ans1 %>% clean1()
data_ans2B <- data_ans2 %>% clean1()
data_ans3B <- data_ans3 %>% clean1()
data_ans4B <- data_ans4 %>% clean1()

data_out <- bind_rows(data_ans1B, data_ans2B, data_ans3B, data_ans4B)

## Write

write_csv(data_out, path(path_raw_out, "/un.csv"))
