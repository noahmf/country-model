
# library packages:

library(fs)
library(tidyverse)


# functions:

`%notin%` <- Negate(`%in%`)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# ggplot themes:

#theme_nf_r <- theme_gray() + theme(
  #panel.background = element_blank(),
  ##aspect.ratio = (3/4),
  
  #axis.ticks = element_line(size=.5),
  #axis.line = element_line(size=.5, color="black"),
  #panel.grid.major.y = element_line(size=.075, color="black"),
  
  #text = element_text(color="black"),
  #plot.title = element_text(hjust=0, face="bold"),
  #plot.subtitle = element_text(hjust=0),
#)

theme_nf_r <- theme_linedraw() + theme(
  plot.title = element_text(face="bold"),
  panel.grid.major = element_line(size=.05),
  panel.grid.minor = element_line(size=0),
)

theme_nf_r_gis <- theme_gray() + theme(
  panel.background = element_blank(),
  
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  
  text = element_text(color="black"),
  plot.title = element_text(hjust=0.5, face="bold"),
  plot.subtitle = element_text(hjust=0.5),
)


# paths:

main_path <- "."
path_base <- path(main_path, "/1-data-base")
path_data <- path(main_path, "/2-data-raw")
path_raw_out <- path(main_path, "/3-data-raw-out")
path_int_out <- path(main_path, "/4-data-int-out")
path_hrmnz <- path(main_path, "/5-harmonization")
path_out <- path(main_path, "/6-data-out")
path_out2 <- path(main_path, "/7-data-out2")
path_model_out <- path(main_path, "/7-model-out")

