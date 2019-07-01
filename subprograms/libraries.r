

#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
# library("here")
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
# library("writexl")
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("gridExtra")
library("ggrepel")

library("knitr")
library("kableExtra")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("btools") # library that I created (install from github)
library("bdata")

# library("qtax")

library("BEAData")
