
## Format and combine the data into a single "long" file

### Setup

source("./setup.R")

### Run formatting scripts

source(path(path_data, "/boe/boe.R"))
source(path(path_data, "/fao/fao.R"))
source(path(path_data, "/gapminder/gapminder.R"))
source(path(path_data, "/imf/imf.R"))
source(path(path_data, "/nfriedlander/nfriedlander.R"))
source(path(path_data, "/owid/owid.R"))
source(path(path_data, "/pbl/pbl.R"))
source(path(path_data, "/un/un.R"))
source(path(path_data, "/uog/uog.R"))
source(path(path_data, "/vdem/vdem.R"))
#source(path(path_data, "/xmarquez/xmarquez.R")) #NO VERSION OF PACKAGE FOR THIS VERSION OF R

### Read and merge

data_un <- read_csv(path(path_raw_out, "/un.csv"))
data_gapminder <- read_csv(path(path_raw_out, "/gapminder.csv"))
data_pbl <- read_csv(path(path_raw_out, "/pbl.csv"))
data_boe <- read_csv(path(path_raw_out, "/boe.csv"))
data_imf <- read_csv(path(path_raw_out, "/imf.csv"))
data_nfriedlander <- read_csv(path(path_raw_out, "/nfriedlander.csv"))
data_uog <- read_csv(path(path_raw_out, "/uog.csv"))
data_owid <- read_csv(path(path_raw_out, "/owid.csv"))
data_xmarquez <- read_csv(path(path_raw_out, "/xmarquez.csv"))
data_vdem <- read_csv(path(path_raw_out, "/vdem.csv"))

data_fao_stat <- read_csv(path(path_raw_out, "/fao-static.csv"))

data_out <- bind_rows(data_un, data_gapminder, data_pbl, data_boe, data_imf, data_nfriedlander, data_uog, data_owid, data_xmarquez, data_vdem)
data_out_stat <- bind_rows(data_fao_stat)

rm(data_un, data_gapminder, data_pbl, data_boe, data_imf, data_nfriedlander, data_uog, data_owid, data_xmarquez, data_vdem, data_fao_stat)

### Write

write_csv(data_out, path(path_int_out, "/data.csv"))
write_csv(data_out_stat, path(path_int_out, "/data-static.csv"))

