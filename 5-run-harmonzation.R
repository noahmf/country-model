
## Setup ---------------------------------------------------------------
  
source("./setup.R")

## Base ---------------------------------------------------------------

base_country_alt <- read_csv(path(path_base, "/nmf-base-country-cross-alt.csv")) %>% 
  mutate(
    country_un=ifelse(as.numeric(country_un)<10, paste("00", country_un, sep=""), ifelse(as.numeric(country_un)<100, paste("0", country_un, sep=""), paste(country_un)))
  )
data_stat <- read_csv(path(path_int_out, "/data-static.csv")) %>% 
  dplyr::select(country, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value")

base_country <- base_country_alt %>% dplyr::select(-country_altname) %>% distinct() %>% arrange(country_name) %>% 
  left_join(data_stat, by="country")

base_sovereignty <- base_country %>% dplyr::select(sovereignty_name) %>% distinct() %>% drop_na(sovereignty_name) %>% 
  mutate(
    country_iso3=base_country$country_iso3[match(sovereignty_name, base_country$country_name)],
    country_iso2=base_country$country_iso2[match(sovereignty_name, base_country$country_name)],
    country_un=base_country$country_un[match(sovereignty_name, base_country$country_name)],
  ) %>% arrange(sovereignty_name)

yrs1 <- paste("obs", paste("", c("0001", "0050", paste("0", seq(100,950,50), sep=""), seq(1000,1150,50), seq(1200,1745,5), seq(1750,2100,1), seq(2110,2300,10)), sep=""), sep="")

data_country0 <- data.frame(year=yrs1, data=NA) %>% 
  pivot_wider(names_from = year, values_from = data)

rm(yrs1)

data_countryA <- bind_cols(base_country, data_country0) %>%
  pivot_longer(cols=starts_with("obs"), names_to="year", names_prefix="obs", values_to="data") %>% 
  dplyr::select(-data, -country_iso2, -country_un) %>% 
  mutate(
    sovereignty=base_country$country[match(sovereignty_name, base_country$country_name)],
    continent=base_country$continent[match(country_name, base_country$country_name)],
    sub_region=base_country$sub_region[match(country_name, base_country$country_name)],
    greater_region=base_country$greater_region[match(country_name, base_country$country_name)],
    analysis_region=base_country$analysis_region[match(country_name, base_country$country_name)],
    economic_region=base_country$economic_region[match(country_name, base_country$country_name)],
    eco_region=base_country$eco_region[match(country_name, base_country$country_name)],
  ) %>% 
  mutate(
    country_obsid=paste(country, year, sep=""),
    sovereignty_obsid=paste(sovereignty, year, sep=""),
    year=as.numeric(year)
    
  ) %>% 
  dplyr::select(country_obsid, country, year, country_name, country_iso3, sovereignty_obsid, sovereignty, sovereignty_name, supra_sovereignty, continent, sub_region, greater_region, analysis_region, economic_region, eco_region, economic_bloc, security_bloc, currency_area, land)

rm(data_country0)


## Read ---------------------------------------------------------------

data0 <- read_csv(path(path_int_out, "/data.csv"))


## Harmonize ---------------------------------------------------------------

### 1. Population ---------------------------------------------------------------

data_pop1 <- data0 %>% # take UN data for 1950-2100
  filter(variable=="pop", source=="un") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_pop2 <- data0 %>% # fixed PBL data for Ireland to represent Republic territory
  filter(year<1930, variable=="pop", source=="pbl", country=="IRLN", year!=1700) %>% 
  mutate(value=round(value*2926998/4361002, 0)) %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_pop2b <- data0 %>% # take BOE data for UK; PBL data seems odd early 19th century
  filter(year<=1900, variable=="pop", source=="boe", country=="UKGB", year%in%seq(1710,1900,10)) %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_pop3 <- data0 %>% # otherwise, take PBL data before 1950
  filter(year<1950, variable=="pop", source=="pbl", obsid%notin%data_pop2$obsid, obsid%notin%data_pop2b$obsid, year!=1700) %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_pop <- bind_rows(data_pop1, data_pop2, data_pop2b, data_pop3)


data_countryB <- data_countryA %>% 
  left_join({data_pop %>% rename(country_obsid=obsid)}, by="country_obsid")


#### 1.2 Manual fixes ---------------------------------------------------------------

data_countryB1 <- data_countryB %>% 
  mutate(
    pop=
      # fix Serbia/Kosovo/Montenegro
      ifelse(country_name=="Kosovo"&year<1950, round({data_countryB %>% filter(country_name=="Serbia")}$pop[match(year, {data_countryB %>% filter(country_name=="Serbia")}$year)]*767274/(767274+6017683), 0), 
      ifelse(country_name=="Montenegro"&year<1950, round({data_countryB %>% filter(country_name=="Serbia")}$pop[match(year, {data_countryB %>% filter(country_name=="Serbia")}$year)]*402260/(402260+6017683), 0), # NOT REMOVED FROM ANYWHERE ELSE!
                    
      # fix Israel/Palestine
      ifelse(country_name=="Palestine"&year<1950, round({data_countryB %>% filter(country_name=="Israel")}$pop[match(year, {data_countryB %>% filter(country_name=="Israel")}$year)]*944807/(944807+1284157), 0), 
      ifelse(country_name=="South Sudan"&year<1950, round({data_countryB %>% filter(country_name=="Sudan")}$pop[match(year, {data_countryB %>% filter(country_name=="Sudan")}$year)]*2492077/(2492077+6191323), 0), 
             pop))))
  )

data_countryB2 <- data_countryB1 %>% 
  mutate(
    pop=
      ifelse(country_name=="Serbia"&year<1950, pop-{data_countryB1 %>% filter(country_name=="Kosovo")}$pop[match(year, {data_countryB1 %>% filter(country_name=="Kosovo")}$year)], 
      ifelse(country_name=="Israel"&year<1950, pop-{data_countryB1 %>% filter(country_name=="Palestine")}$pop[match(year, {data_countryB1 %>% filter(country_name=="Palestine")}$year)], 
                    
      # fix Sudan/South Sudan
      ifelse(country_name=="Sudan"&year<1950, pop-{data_countryB1 %>% filter(country_name=="South Sudan")}$pop[match(year, {data_countryB1 %>% filter(country_name=="South Sudan")}$year)], 
            pop)))
  )


#### 1.3 Interpolation ---------------------------------------------------------------

source(path(path_hrmnz, "/population-interpolation.R")) # interpolate population figures where missing
data_countryB3 <- data_countryB2 %>% pop_interp()


### 2. Economics ---------------------------------------------------------------

#### 2.1 GDP ---------------------------------------------------------------

data_gdp1 <- data0 %>% # take IMF data starting 2015
  filter(variable=="gdp_pc", source%in%c("imf", "gapminder"), unit=="usd2021ppp", year>=2015, year<=2026) %>% 
  dplyr::select(obsid, source, value) %>% 
  pivot_wider(names_from = source, values_from = value) %>% 
  mutate(
    gdp_pc=ifelse(is.na(imf)==F, imf, ifelse(is.na(gapminder)==F, gapminder, NA))
  ) %>% 
  dplyr::select(obsid, gdp_pc)

data_countryC <- data_countryB3 %>% 
  left_join({data_gdp1 %>% rename(country_obsid=obsid)}, by="country_obsid")

data_gdp2 <- data0 %>% # take Gapminder data before 1951
  filter(variable=="gdp_pc", source=="gapminder", year<=1950) %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_countryC0 <- data_countryC %>% 
  mutate(
    gdp_pc=ifelse(country_obsid%in%data_gdp2$obsid, data_gdp2$gdp_pc[match(country_obsid, data_gdp2$obsid)], gdp_pc),
  )

gmean <- 
  function(x,na.rm=TRUE)
  { 
    exp(mean(log(x),na.rm=na.rm)) }

data_gdp30 <- data0 %>% # for growth between 1950-2015, take average of four sources
  filter(variable=="gdp_pc_r", dataset%in%c("gd001-v27", "weoapr2021", "mpd2020", "pwt10.0"), year>1950, year<=2015) %>% 
  dplyr::select(obsid, dataset, value) %>% #
  pivot_wider(names_from = dataset, values_from = value) %>% 
  mutate(
    `gd001-v27`=ifelse(is.nan(`gd001-v27`)==T, NA, ifelse(is.infinite(`gd001-v27`)==T, NA, ifelse(`gd001-v27`==(-100), NA, `gd001-v27`))),
    weoapr2021=ifelse(is.nan(weoapr2021)==T, NA, ifelse(is.infinite(weoapr2021)==T, NA, ifelse(weoapr2021==(-100), NA, weoapr2021))),
    mpd2020=ifelse(is.nan(mpd2020)==T, NA, ifelse(is.infinite(mpd2020)==T, NA, ifelse(mpd2020==(-100), NA, mpd2020))),
    `pwt10.0`=ifelse(is.nan(`pwt10.0`)==T, NA, ifelse(is.infinite(`pwt10.0`)==T, NA, ifelse(`pwt10.0`==(-100), NA, `pwt10.0`))),
    #gdp_pc_r=ifelse(is.na(mpd2020)==F, mpd2020, ifelse(is.na(`gd001-v27`)==F, `gd001-v27`, NA)),
  ) %>% 
  mutate(`gd001-v27`=1+(`gd001-v27`/100), weoapr2021=1+(weoapr2021/100), mpd2020=1+(mpd2020/100), `pwt10.0`=1+(`pwt10.0`/100)) %>% 
  pivot_longer(!obsid, names_to = "dataset", values_to = "value") %>% 
  group_by(obsid) %>% 
  summarise(gdp_pc_r=gmean(value, na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(obsid, gdp_pc_r) %>% 
  rename(geometric=gdp_pc_r)
  #mutate(geometric=1+(gdp_pc_r/100))

data_gdp3c <- data_countryC0 %>% 
  filter(year%in%c(2015)) # end of period to be interpolated
data_gdp3c2 <- data_countryC0 %>% 
  filter(year%in%c(1950)) # start of period to be interpolated

data_gdp3b <- data_gdp30 %>% # compare "real cumulative" growth (IMF and Gapminder pre-1951) to cumulative observed in Maddison/Gapminder
  mutate(country=substr(obsid,1,4)) %>% group_by(country) %>% 
  summarise(cumulative=prod(geometric, na.rm=F)) %>% 
  ungroup() %>% 
  mutate(real_cumulative=data_gdp3c$gdp_pc[match(country, data_gdp3c$country)]/data_gdp3c2$gdp_pc[match(country, data_gdp3c2$country)])

data_gdp3 <- data_gdp30 %>% # interpolate growth to align with "real cumulative" growth
  mutate(geometric1=geometric*((data_gdp3b$real_cumulative[match(substr(obsid,1,4), data_gdp3b$country)]/data_gdp3b$cumulative[match(substr(obsid,1,4), data_gdp3b$country)])^(1/65))) %>% 
  mutate(gdp_pc_r=(geometric1-1)*100) %>% 
  dplyr::select(obsid, gdp_pc_r)


data_countryD <- data_countryC0 %>% 
  left_join({data_gdp3 %>% rename(country_obsid=obsid)}, by="country_obsid")

for(i in base_country$country){ # apply interpolated growth to levels
  for(j in 2014:1951){
    data_countryD$gdp_pc[data_countryD$country==i&data_countryD$year==j] <- data_countryD$gdp_pc[data_countryD$country==i&data_countryD$year==j+1]/((data_countryD$gdp_pc_r[data_countryD$country==i&data_countryD$year==j+1]/100)+1)
  }
}

data_countryE0 <- data_countryD %>% 
  group_by(country) %>% 
  mutate(
    gdp_pc_r=((gdp_pc/lag(gdp_pc, 1))-1)*100,
    gdp=gdp_pc*pop,
  ) %>% 
  ungroup()


#### 2.2 GNP ---------------------------------------------------------------

data_gnp <- data0 %>%
  filter(variable%in%c("gnp_gdp"), source=="nfriedlander") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_countryE1 <- data_countryE0 %>% 
  left_join({data_gnp %>% rename(country_obsid=obsid)}, by="country_obsid") %>% 
  mutate(
    gnp_gdp=ifelse(year<1950,1,ifelse(year>2021,data_gnp$gnp_gdp[match(paste(country,"2021",sep=""),data_gnp$obsid)],gnp_gdp))
  ) %>% 
  mutate(
    gnp=gdp*ifelse(is.na(gnp_gdp)==T,1,gnp_gdp),
  ) %>% 
  select(-gnp_gdp)


#### 2.3 Labor and capital ---------------------------------------------------------------

data_labkap1 <- data0 %>%
  filter(variable%in%c("kap_gdp", "kap_dep", "labsh", "emp_pop", "hrs_emp"), source=="uog") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)


data_countryE <- data_countryE1 %>% 
  left_join({data_labkap1 %>% rename(country_obsid=obsid)}, by="country_obsid") %>% 
  group_by(country) %>% 
  mutate(
    capital=kap_gdp*gdp,
  ) %>% 
  mutate(
    emp=round(pop*emp_pop, 0),
    savr=((capital-(lag(capital,1)*(1-(lag(kap_dep,1)/100))))/gdp)*100,
  ) %>% 
  ungroup()


### 3. Demography ---------------------------------------------------------------

data_demo1 <- data0 %>% 
  filter(variable%in%c("tfr", "lexb", "births", "deaths", "netmigs", "imr", "cmr", "age_mdn", "age_mean_chldbear"), source=="un") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_demo2 <- data0 %>% 
  filter(variable%in%c("tfr", "lexb", "cmr"), source=="gapminder", year<1950) %>% 
  filter(substr(country,1,3)%notin%c("NA1", "NA2")) %>%  #GET RID OF NAs???
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_demo3 <- data0 %>% 
  filter(variable%in%c("pop0_4", "pop5_14", "pop15_24", "pop25_39", "pop40_64", "pop65_74", "pop75_plus"), source=="un") %>% 
  filter(substr(country,1,3)%notin%c("NA1", "NA2")) %>%  #GET RID OF NAs???
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_demo4 <- data0 %>% 
  filter(variable%in%c("pop0_4", "pop5_14", "pop15_24", "pop25_39", "pop40_64", "pop65_74", "pop75_plus"), source=="ipums-usa") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)


data_countryF <- data_countryE %>% 
  left_join({data_demo3 %>% rename(country_obsid=obsid)}, by="country_obsid") %>% 
  left_join({data_demo1 %>% rename(country_obsid=obsid)}, by="country_obsid")

data_countryG <- data_countryF %>% 
  mutate(
    tfr=ifelse(country_obsid%in%data_demo2$obsid, data_demo2$tfr[match(country_obsid, data_demo2$obsid)], tfr),
    lexb=ifelse(country_obsid%in%data_demo2$obsid, data_demo2$lexb[match(country_obsid, data_demo2$obsid)], lexb),
    cmr=ifelse(country_obsid%in%data_demo2$obsid, data_demo2$cmr[match(country_obsid, data_demo2$obsid)], cmr),
    
    pop0_4=ifelse(country_obsid%in%data_demo4$obsid, data_demo4$pop0_4[match(country_obsid, data_demo4$obsid)], pop0_4),
    pop5_14=ifelse(country_obsid%in%data_demo4$obsid, data_demo4$pop5_14[match(country_obsid, data_demo4$obsid)], pop5_14),
    pop15_24=ifelse(country_obsid%in%data_demo4$obsid, data_demo4$pop15_24[match(country_obsid, data_demo4$obsid)], pop15_24),
    pop25_39=ifelse(country_obsid%in%data_demo4$obsid, data_demo4$pop25_39[match(country_obsid, data_demo4$obsid)], pop25_39),
    pop40_64=ifelse(country_obsid%in%data_demo4$obsid, data_demo4$pop40_64[match(country_obsid, data_demo4$obsid)], pop40_64),
    pop65_74=ifelse(country_obsid%in%data_demo4$obsid, data_demo4$pop65_74[match(country_obsid, data_demo4$obsid)], pop65_74),
    pop75_plus=ifelse(country_obsid%in%data_demo4$obsid, data_demo4$pop75_plus[match(country_obsid, data_demo4$obsid)], pop75_plus),
  )


### 4. Income Distribution ---------------------------------------------------------------

data_dist1 <- data0 %>% 
  filter(variable%in%c("is_0_2", "is_2_6", "is_6_20", "is_20_60", "is_60_200", "is_200_plus"), source=="gapminder") %>%
  dplyr::select(obsid, variable, value) %>% 
  distinct() %>% # WHY IS THIS NEEDED?? CODI 1995 duplicated??
  pivot_wider(names_from = variable, values_from = value)

data_countryH <- data_countryG %>% 
  left_join({data_dist1 %>% rename(country_obsid=obsid)}, by="country_obsid")


### 5. CO2 ---------------------------------------------------------------

data_co2 <- data0 %>% 
  filter(variable=="co2", source=="owid") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_countryI <- data_countryH %>% 
  left_join({data_co2 %>% rename(country_obsid=obsid)}, by="country_obsid")


### 6. Energy ---------------------------------------------------------------

data_ener <- data0 %>% 
  filter(variable=="energy_pc", source=="owid", year>=1985, year<=2019) %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)


data_countryJa <- data_countryI %>% 
  left_join({data_ener %>% rename(country_obsid=obsid)}, by="country_obsid") %>% 
  mutate(
    energy=(pop*energy_pc)/1000000000,
  )


### 7. Democracy and institutions ---------------------------------------------------------------

data_dem1 <- data0 %>% 
  filter(variable=="uds", source=="xmarquez") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_dem2 <- data0 %>% 
  filter(variable%in%c("vdem", "vdem_libdem", "vdem_liberal", "vdem_suffrage"), source=="vdem") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)


data_countryJ <- data_countryJa %>% 
  left_join({data_dem1 %>% rename(country_obsid=obsid)}, by="country_obsid") %>% 
  left_join({data_dem2 %>% rename(country_obsid=obsid)}, by="country_obsid")


### 8. Education ---------------------------------------------------------------

data_edu1 <- data0 %>% 
  filter(variable=="edu_mnyr_15pl", source=="vdem") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)

data_edu2 <- data0 %>% 
  filter(variable=="edu_tertiary_share", source=="owid") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)


data_countryKa <- data_countryJ %>% 
  left_join({data_edu1 %>% rename(country_obsid=obsid)}, by="country_obsid") %>% 
  left_join({data_edu2 %>% rename(country_obsid=obsid)}, by="country_obsid") %>% 
  mutate(
    edu_tertiary_share=
        ifelse(year%in%seq(1971,2046,5), ifelse(is.na(lag(edu_tertiary_share,1))==F, lag(edu_tertiary_share,1)*((lead(edu_tertiary_share,4)/lag(edu_tertiary_share,1))^(1/5)), edu_tertiary_share), 
        ifelse(year%in%seq(1972,2047,5), ifelse(is.na(lag(edu_tertiary_share,2))==F, lag(edu_tertiary_share,2)*((lead(edu_tertiary_share,3)/lag(edu_tertiary_share,2))^(2/5)), edu_tertiary_share), 
        ifelse(year%in%seq(1973,2048,5), ifelse(is.na(lag(edu_tertiary_share,3))==F, lag(edu_tertiary_share,3)*((lead(edu_tertiary_share,2)/lag(edu_tertiary_share,3))^(3/5)), edu_tertiary_share), 
        ifelse(year%in%seq(1974,2049,5), ifelse(is.na(lag(edu_tertiary_share,4))==F, lag(edu_tertiary_share,4)*((lead(edu_tertiary_share,1)/lag(edu_tertiary_share,4))^(4/5)), edu_tertiary_share), 
               edu_tertiary_share))))
  )


### 9. Homicide ---------------------------------------------------------------

data_crim1 <- data0 %>% 
  filter(variable=="homicide", source=="owid") %>% 
  dplyr::select(obsid, variable, value) %>% 
  pivot_wider(names_from = variable, values_from = value)


data_countryK <- data_countryKa %>% 
  left_join({data_crim1 %>% rename(country_obsid=obsid)}, by="country_obsid")


## Write ---------------------------------------------------------------


data_countryZ <- data_countryK
write_csv(data_countryZ, path(path_out, "/data.csv"))

