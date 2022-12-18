
## Setup ---------------------------------------------------------------
  
source("./setup.R")

## Base ---------------------------------------------------------------

Hyrs1 <- paste("obs", paste("", c("0001", "0050", paste("0", seq(100,950,50), sep=""), seq(1000,1150,50), seq(1200,1745,5), seq(1750,2100,1), seq(2110,2300,10)), sep=""), sep="")


## Read ---------------------------------------------------------------

Hdata0 <- read_csv(path(path_out, "/data.csv"))


## Process ---------------------------------------------------------------

#Hdata_hsub0 <- data.frame(year=Hyrs1, data=NA) %>% 
  #pivot_wider(names_from = year, values_from = data)

#Hdata_hsub1 <- bind_cols(Hbase_hist_subs, Hdata_hsub0) %>%
  #pivot_longer(cols=starts_with("obs"), names_to="year", names_prefix="obs", values_to="data") %>% 
  #mutate(sov_obsid=paste(sovereignty, year, sep=""), year=as.numeric(year)) %>% 
  #dplyr::select(sub_name, sov_obsid, sovereignty_name, hist_sov_name, sovereignty, year)

#Hist_subs <- read_csv(path(path_base, "/nmf-hist-subs.csv"))

Hist_subs <- read_csv(path(path_data, "/nfriedlander/nmf-hist-subs.csv"))


Hist_subsA <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_a:hist_sub_a_rpp) %>% drop_na(hist_sub_a) %>% 
  mutate(hist_sub=paste(hist_sub_a), hist_sub_pop=as.numeric(hist_sub_a_pop), hist_sub_gnp=as.numeric(hist_sub_a_gnp), hist_sub_rpp=as.numeric(hist_sub_a_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsB <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_b:hist_sub_b_rpp) %>% drop_na(hist_sub_b) %>% 
  mutate(hist_sub=paste(hist_sub_b), hist_sub_pop=as.numeric(hist_sub_b_pop), hist_sub_gnp=as.numeric(hist_sub_b_gnp), hist_sub_rpp=as.numeric(hist_sub_b_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsC <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_c:hist_sub_c_rpp) %>% drop_na(hist_sub_c) %>% 
  mutate(hist_sub=paste(hist_sub_c), hist_sub_pop=as.numeric(hist_sub_c_pop), hist_sub_gnp=as.numeric(hist_sub_c_gnp), hist_sub_rpp=as.numeric(hist_sub_c_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsD <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_d:hist_sub_d_rpp) %>% drop_na(hist_sub_d) %>% 
  mutate(hist_sub=paste(hist_sub_d), hist_sub_pop=as.numeric(hist_sub_d_pop), hist_sub_gnp=as.numeric(hist_sub_d_gnp), hist_sub_rpp=as.numeric(hist_sub_d_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsE <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_e:hist_sub_e_rpp) %>% drop_na(hist_sub_e) %>% 
  mutate(hist_sub=paste(hist_sub_e), hist_sub_pop=as.numeric(hist_sub_e_pop), hist_sub_gnp=as.numeric(hist_sub_e_gnp), hist_sub_rpp=as.numeric(hist_sub_e_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsF <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_f:hist_sub_f_rpp) %>% drop_na(hist_sub_f) %>% 
  mutate(hist_sub=paste(hist_sub_f), hist_sub_pop=as.numeric(hist_sub_f_pop), hist_sub_gnp=as.numeric(hist_sub_f_gnp), hist_sub_rpp=as.numeric(hist_sub_f_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsG <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_g:hist_sub_g_rpp) %>% drop_na(hist_sub_g) %>% 
  mutate(hist_sub=paste(hist_sub_g), hist_sub_pop=as.numeric(hist_sub_g_pop), hist_sub_gnp=as.numeric(hist_sub_g_gnp), hist_sub_rpp=as.numeric(hist_sub_g_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsH <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_h:hist_sub_h_rpp) %>% drop_na(hist_sub_h) %>% 
  mutate(hist_sub=paste(hist_sub_h), hist_sub_pop=as.numeric(hist_sub_h_pop), hist_sub_gnp=as.numeric(hist_sub_h_gnp), hist_sub_rpp=as.numeric(hist_sub_h_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsI <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_i:hist_sub_i_rpp) %>% drop_na(hist_sub_i) %>% 
  mutate(hist_sub=paste(hist_sub_i), hist_sub_pop=as.numeric(hist_sub_i_pop), hist_sub_gnp=as.numeric(hist_sub_i_gnp), hist_sub_rpp=as.numeric(hist_sub_i_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsJ <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_j:hist_sub_j_rpp) %>% drop_na(hist_sub_j) %>% 
  mutate(hist_sub=paste(hist_sub_j), hist_sub_pop=as.numeric(hist_sub_j_pop), hist_sub_gnp=as.numeric(hist_sub_j_gnp), hist_sub_rpp=as.numeric(hist_sub_j_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)
Hist_subsK <- Hist_subs %>% dplyr::select(country_obsid, hist_sub_k:hist_sub_k_rpp) %>% drop_na(hist_sub_k) %>% 
  mutate(hist_sub=paste(hist_sub_k), hist_sub_pop=as.numeric(hist_sub_k_pop), hist_sub_gnp=as.numeric(hist_sub_k_gnp), hist_sub_rpp=as.numeric(hist_sub_k_rpp)) %>%
  dplyr::select(country_obsid, hist_sub, hist_sub_pop, hist_sub_gnp, hist_sub_rpp)

Hist_subs1 <- bind_rows(Hist_subsA, Hist_subsB, Hist_subsC, Hist_subsD, Hist_subsE, Hist_subsF, Hist_subsG, Hist_subsH, Hist_subsI, Hist_subsJ, Hist_subsK)


Hist_subs2 <- Hist_subs1 %>% 
  left_join(Hdata0, by=c("country_obsid")) %>% 
  mutate(hist_sub_gnp=ifelse(is.na(hist_sub_gnp)==F, hist_sub_gnp, NA), hist_sub_rpp=ifelse(is.na(hist_sub_rpp)==F, hist_sub_rpp, 1), pop1=pop) %>% 
  group_by(hist_sub, year) %>% 
  summarise(
    pop=round(sum(pop1*hist_sub_pop, na.rm=T),0),
    gnp=sum(gnp*(hist_sub_gnp/hist_sub_rpp), na.rm=T),
    #gnp=sum(gnp*(hist_sub_gnp), na.rm=T),
  ) %>% 
  ungroup() %>% 
  select(hist_sub, year, pop, gnp)
Hist_subs2[Hist_subs2==0] <- NA


## Write ---------------------------------------------------------------


write_csv(Hist_subs2, path(path_out2, "/data-hist-sub.csv"))

