library(readxl)
library(purrr)
library(dplyr)
setwd('~/Documents/Int_fin/cpi')
# Read in quarterly CPI data and calcualte annual averages to allign CPI data with spot rate data
us_cpi <- read.csv('./USACPIALLMINMEI.csv')
uk_cpi <- read.csv('./GBRCPIALLMINMEI.csv')
fr_cpi <- read.csv('./FRACPIALLMINMEI.csv')
gr_cpi <- read.csv('./DEUCPIALLMINMEI.csv')
in_cpi <- read.csv('./INDCPIALLMINMEI.csv')

annualise <- function(dframe,start_year){
  out_df <- data.frame(Year=character(),Average_CPI=double(),stringsAsFactors = FALSE)
  year <- start_year
  a <- 1
  b <- 12
  c <- 1
  while (b < length(dframe[,1])) {
    x <- mean(dframe[a:b,2])
    out_df[c,1] <- as.numeric(year)
    out_df[c,2] <- x
    a <- a+12
    b <- b+12
    c <- c+1
    year <- year+1
  }
  return(out_df)
}

us_cpi_annual <- annualise(us_cpi,1960)
uk_cpi_annual <- annualise(uk_cpi,1960)
fr_cpi_annual <- annualise(fr_cpi,1960)
gr_cpi_annual <- annualise(gr_cpi,1960)
in_cpi_annual <- annualise(in_cpi,1960)

# Read in spot rate data, trim date structure and express all rates in USD per foreign currency

uk_spot_rates <- read.csv('./AEXUSUK.csv',stringsAsFactors = FALSE)
uk_spot_rates[,1] <- as.numeric(substr(uk_spot_rates[,1],1,4))

eu_spot_rates <- read.csv('./EXUSEU.csv',stringsAsFactors = FALSE)
eu_spot_rates[,1] <- as.numeric(substr(eu_spot_rates[,1],1,4))
eu_spot_rates <- annualise(eu_spot_rates,1999)

fr_spot_rates <- read.csv('./CCUSMA02FRA618N.csv',stringsAsFactors = FALSE)
fr_spot_rates[,1] <- as.numeric(substr(fr_spot_rates[,1],1,4))
fr_spot_rates[,2]  <- 1/(fr_spot_rates[,2])

gr_spot_rates <- read.csv('./CCUSSP01DEA650N.csv', stringsAsFactors = FALSE)
gr_spot_rates[,1] <- as.numeric(substr(gr_spot_rates[,1],1,4))
gr_spot_rates[,2] <- 1/(fr_spot_rates[,2])

in_spot_rates <- read.csv('./AEXINUS.csv', stringsAsFactors = FALSE)
in_spot_rates[,1] <- as.numeric(substr(in_spot_rates[,1],1,4))
in_spot_rates[,2]  <- 1/(in_spot_rates[,2])

# Computing Relative PPP - USA is always the home currency

get_rate <- function(df,year){
  out <- df[(which(df[,1]==year)),2]
  return(out)
}

get_ppp_df <- function(base_year,end_year,foreign_spot,foreign_cpi){
  base_spot <- get_rate(foreign_spot,base_year)
  us_base_cpi <- get_rate(us_cpi_annual,base_year)
  f_base_cpi  <- get_rate(foreign_cpi,base_year)
  years <- c(base_year:end_year)

  delta_spot <- unlist(purrr::map(years,function(x) log(get_rate(foreign_spot,x)) - log(base_spot) ))
  home_inflation <- unlist(purrr::map(years,function(x) log(get_rate(us_cpi_annual,x)) - log(us_base_cpi) ))
  f_inflation <- unlist(purrr::map(years,function(x) log(get_rate(foreign_cpi,x)) - log(f_base_cpi) ))
  delta_inflation <- home_inflation - f_inflation
  rppp <- delta_spot - delta_inflation
  absolute_error <- abs(rppp)

  out <- as.data.frame(cbind(years,home_inflation,f_inflation,delta_inflation,rppp, absolute_error))
  return(out)
}


# Variations in the validity of Relative PPP for US Dollar against UK Pound sterling from 1971 to 2018
uk_rppp <- get_ppp_df(1971,2018,uk_spot_rates,uk_cpi_annual)

# Variations in the validity of Relative PPP for US Dollar against Indian Rupee from 1973 to 2018
in_rpp <- get_ppp_df(1973,2018,in_spot_rates,in_cpi_annual)

# Post Euro France & Germany
fr_rppp <-get_ppp_df(2000,2018,eu_spot_rates,fr_cpi_annual)
gr_rpp <- get_ppp_df(2000,2018,eu_spot_rates,gr_cpi_annual)
