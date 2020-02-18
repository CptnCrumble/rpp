library(readxl)
library(purrr)
library(dplyr)
library(ggplot2)
library(varhandle)
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

### Need to check data source, values look right but column header is alarming...
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

# Helper function
get_rate <- function(df,year){
  out <- df[(which(df[,1]==year)),2]
  return(out)
}

# Calcualte RPP for every year where t-1 is a fixed base year
get_long_rpp <- function(base_year,end_year,foreign_spot,foreign_cpi,country_label){
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
  time_period <- c(0:(length(delta_spot)-1))
  out <- as.data.frame(cbind(time_period,delta_spot,home_inflation,f_inflation,delta_inflation,rppp, absolute_error,rep(country_label,times=length(delta_spot))))
  out <- out[-1,]
  return(out)
}

# Calculate RPPP for a group of years where t-1 is the prevous year
get_short_rpp <- function(start_year,end_year,foreign_spot,foreign_cpi,country_label){
  years <- c((start_year+1):end_year)
  
  delta_spot <- round(((unlist(purrr::map(years,function(x) log(get_rate(foreign_spot,x)) - log(get_rate(foreign_spot,(x-1))) )))*100),digits = 3) 
  home_inflation <- (unlist(purrr::map(years,function(x) log(get_rate(us_cpi_annual,x)) - log(get_rate(us_cpi_annual,(x-1))) )))*100
  f_inflation <- (unlist(purrr::map(years,function(x) log(get_rate(foreign_cpi,x)) - log(get_rate(foreign_cpi,(x-1))) )))*100
  delta_inflation <- round((home_inflation - f_inflation),digits = 3) 
  rppp <- delta_spot - delta_inflation
  absolute_error <- abs(rppp)
  
  out <- as.data.frame(cbind(years,delta_spot,home_inflation,f_inflation,delta_inflation,rppp, absolute_error,rep(country_label,times=length(delta_spot))))
  names <- c("Year","Spot_rate_change","US_Inflation","Foreign_Inflation","Inflation_rate difference","RPPP_error","Absolute_RPPP_error","Country")
  colnames(out) <- names
  return(out)
}

india_short_rpp_values <- get_short_rpp(1973,2018,in_spot_rates,in_cpi_annual,"India")
uk_short_rpp_values <- get_short_rpp(1973,2018,uk_spot_rates,uk_cpi_annual,"UK")

plot_data <- rbind(india_short_rpp_values,uk_short_rpp_values)
plot_data$Spot_rate_change <- unfactor(plot_data$Spot_rate_change)
plot_data$RPPP_error <- unfactor(plot_data$RPPP_error)
plot_data$Absolute_RPPP_error <- unfactor(plot_data$Absolute_RPPP_error)
plot_data$`Inflation_rate difference` <- unfactor(plot_data$`Inflation_rate difference`)

ggplot(data = plot_data, aes(Year,RPPP_error))+
  geom_point(aes(colour=Country))+
  geom_hline(yintercept = 0)+
  scale_x_discrete(breaks =c("1974","1980","1985","1990","1995","2000","2005","2010","2015","2018"))

# Plot of FR vs German Inf rates over time - but the currency is the same!!!
# Plot RPPP over time, any pattern, does it improve?

# High & erratic inflation  - INdia 
# Plot RPPP over time, any pattern, does it improve?

# Does RPPP perform better with more stable currency? eg the pound

# RPPP for comparisons with a Pegged currency? Chyyyyna???

#Does RPPP perform better over the long haul?

