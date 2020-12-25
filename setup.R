Sys.setenv(TZ='UTC') # Local Time Zone

# load libraries --------------------------------------

library(here)         # manage directories
library(janitor)      # data cleaning
library(modelsummary) # freq tables
library(skimr)        # skim data
library(ggplot2)      # data viz
library(ggthemes)     # data viz themes
library(hrbrthemes)   # data viz themes
library(sf)           # read and manipulate spatial data
library(data.table)   # fast data wrangling
#library(collapse)     # insanely fast data transformation
library(foreign)      # read data in strange formats
library(magrittr)     # pipe operator
library(ggmap)        # Google API
library(dodgr)        # calculate distance between points
library(r5r)          # calculate travel distance and intineraries 
library(geobr)        # Brazil's spatial data
library(pbapply)      # progress bar
library(readr)        # rapid data read 
library(tidyr)        # data manipulating
library(stringr)      # strings operations
library(lubridate)    # handle date formats  
library(maptools)
library(mapview)      # interactive maps
library(fixest)       # fast fixed effects
library(sandwich)     # fast estimators
library(broom)        # model summary
library(RColorBrewer) # color palettes
library(paletteer)    # color palettes
library(extrafont)    # text fonts
library(ggtext)       # text tool for data viz
library(knitr)        # knit documents
library(furrr)        # vectorize in parallel
library(purrr)        # funcional programming
library(forcats)      # handle factors
library(parallel)     # optimize operations
library(future.apply) # more optimization
library(dplyr)        # better than data.table!
library(beepr)        # tells me when work is done
library(patchwork)    # plot composition
library(Hmisc)        # calculate weighted quantiles
library(osmdata)      # Download OpenStreetMaps data (transit networks)
library(opentripplanner) # Use OTP from R: https://github.com/ITSLeeds/opentripplanner
library(h3jsr)        # h3 hexagonons
library(bit64)        # viz large numbers

# Set some options and functions --------------------

source('fun.R')

options(scipen = 99999)

# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html

options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)


# Set files' paths at Ipea's directories ----------------------------------------

path_cadunico <- "//Storage6/bases/DADOS/RESTRITO/CADASTRO_UNICO/csv/"
path_rais <- "//Storage6/bases/DADOS/RESTRITO/RAIS/"
path_caged <- "//Storage6/bases/DADOS/RESTRITO/CAGED_ID/"
path_fies <- "//storage1/bases4/RESTRITO/FIES/"

# Loads metros_br df
metros_br <- readr::read_rds(here::here('data-raw','metros_br.rds'))