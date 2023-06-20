#' @title This script import different source of data and unify the format

rm(list = ls())

library(openxlsx)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

source("./code/utility/config.R")

DBpath <- file.path(DROPBOX_PATH, "/data/cleaned_data/data.xlsx")
df <- openxlsx::read.xlsx(DBpath, 
                          startRow = 2, detectDates = T)

#' @section Create processed time series to feed the model
#' yt = [Rt rgdpt rmrt, loant ccostt sentt hpt]′
#' `R`    : interbank overnight rate
#' `rgdp` : real GDP
#' `rmr`  : real mortgage rate ~= mr - \Delta CPI
#'          more precisely, rmr = (1 + mr)/(1 + inflation rate) - 1
#' `loan` : the home purchase loans (loan 1) / real GDP
#' `ccost`: the variable construct
#' `sent` : sentiment index
#' `hp`   : house price index

### How we generate variables
# genr loanall = loanall/3 
# genr loanratio = loan1/ngdp
# genr rhp = hp_tw1/cpi
# genr rmr = mr - 100*log(cpi/cpi(-4))
# genr pir = log(hp_tw1) - (log(NGDP) - log(pop_tw2*1000)) 


df <- df %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::rename(ngdp = nominalGDP,
         cpi = CPI,
         rgdp = rGDP,
         ccost = construct) %>% 
  # create variables 
  mutate(loanall = loanall / 3,
         loanratio = loan1 / ngdp,
         rhp = hp_tw1 / cpi,
         rmr = mr - 100 * log(cpi / lag(cpi, 4)),
         # robustness 2
         pir = log(hp_tw1) - (log(ngdp) - log(pop_tw2 * 1000))
         ) %>% 
  # taking log
  # %vname = "rgdp ccost rhp cci cpi hp_tw1"
  mutate(lrgdp = log(rgdp),
         lccost = log(ccost),
         lrhp = log(rhp),
         lcci = log(cci),
         lcpi = log(cpi),
         # robustness 3
         lhp_tw1 = log(hp_tw1),
         # robustness 4
         lhp_tpe = log(hp_tpe1) - log(cpi)
         )
data_clean <- df %>% 
  dplyr::select(date, season, 
         R,
         lrgdp,
         rgdp,
         rmr,
         loanratio,
         lccost,
         ccost,
         sent,
         lrhp,
         rhp,
         pir,
         lhp_tw1,
         lhp_tpe,
         cpi)


# Save RDS file
saveRDS(data_clean, file.path(DROPBOX_PATH, "./data/cleaned_data/data.RDS"))

