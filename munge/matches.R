#' # Match Analysis
#' ### Digging through Five Thirty Eight's match data
#' 
#+settings
knitr::opts_chunk$set(dpi = 200, fig.width = 8, fig.height = 5, message = F, warning = F)
#ezspin_pt(file_name = "file_name", project_directory = ".", file_folder = "munge", keep_html = F)

#+ packages
library(tidyverse)
library(tidymodels)
library(expappr)

#+ download_data
spi_matches <- readRDS("./data/fivethirtyeight/spi_matches2018-09-22.RDS")
intl_rankings <- readRDS("./data/fivethirtyeight/intl_rankings2018-09-22.RDS")
club_rankings <- readRDS("./data/fivethirtyeight/club_rankings2018-09-22.RDS")

#' First, let's explore the data structure of each of these files

spi_matches %>% 
  head() %>% 
  knitr::kable()
