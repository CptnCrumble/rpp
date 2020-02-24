library(readxl)
library(purrr)
library(dplyr)
library(ggplot2)
library(varhandle)
library(reshape2)

# Read in quarterly CPI data and calcualte annual averages to allign CPI data with spot rate data
us_cpi <- read.csv('./original_data/USACPIALLMINMEI.csv',stringsAsFactors = FALSE)
uk_cpi <- read.csv('./original_data/GBRCPIALLMINMEI.csv',stringsAsFactors = FALSE)
fr_cpi <- read.csv('./original_data/FRACPIALLMINMEI.csv',stringsAsFactors = FALSE)
gr_cpi <- read.csv('./original_data/DEUCPIALLMINMEI.csv',stringsAsFactors = FALSE)
ch_cpi <- read.csv('./original_data/CHNCPIALLMINMEI.csv',stringsAsFactors = FALSE)

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
ch_cpi_annual <- annualise(ch_cpi,1993)

# Read in spot rate data, trim date structure
# direct spot rates needed, conversion applied where necessary

uk_spot_rates <- read.csv('./original_data/AEXUSUK.csv',stringsAsFactors = FALSE)
uk_spot_rates[,1] <- as.numeric(substr(uk_spot_rates[,1],1,4))

eu_spot_rates <- read.csv('./original_data/AEXUSEU.csv',stringsAsFactors = FALSE)
eu_spot_rates[,1] <- as.numeric(substr(eu_spot_rates[,1],1,4))

ch_spot_rates <- read.csv('./original_data/AEXCHUS.csv', stringsAsFactors = FALSE)
ch_spot_rates[,1] <- as.numeric(substr(ch_spot_rates[,1],1,4))
ch_spot_rates[,2] <- 1/(ch_spot_rates[,2])


# Helper function
get_rate <- function(df,year){
  out <- df[(which(df[,1]==year)),2]
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
  names <- c("Year","Spot_rate_change","US_Inflation","Foreign_Inflation","Inflation_rate_difference","RPPP_error","Absolute_RPPP_error","Country")
  colnames(out) <- names
  return(out)
}

china_short_rpp_values <- get_short_rpp(1994,2018,ch_spot_rates,ch_cpi_annual,"China")
uk_short_rpp_values <- get_short_rpp(1973,2018,uk_spot_rates,uk_cpi_annual,"UK")
france_short_rpp_values <- get_short_rpp(1999,2018,eu_spot_rates,fr_cpi_annual, "France")
germany_short_rpp_values <- get_short_rpp(1999,2018,eu_spot_rates,gr_cpi_annual, "Germany")

## PLot of RPPP score, FR, GR
ggplot(data = fr_gr, aes(x=Year,y=RPPP_Score, colour=Country))+
  geom_point(size=2,alpha=0.5)+
  scale_colour_manual(values = c("#001489","#DD0000"))+
  ylim(-20,20)+
  ylab("RPPP Score")+
  ggtitle("RPPP Score for France and Germany since adopting the Euro", subtitle = "Dashed line marks RPPP equilibrium")+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Decima WE")
  )

# t-test france
f1 <- unfactor(france_short_rpp_values$Spot_rate_change)
f2 <- unfactor(france_short_rpp_values$Inflation_rate_difference)
t.test(f1,f2)
#t-test germany
g1 <- unfactor(germany_short_rpp_values$Spot_rate_change)
g2 <- unfactor(germany_short_rpp_values$Inflation_rate_difference)
t.test(g1,g2)


# Plot of China RPPP over time
ch_plot <- cbind.data.frame(unfactor(china_short_rpp_values$Year),unfactor(china_short_rpp_values$RPPP_error),china_short_rpp_values$Country)
colnames(ch_plot) <- c("Year","RPPP_Score","Country")

ggplot(data = ch_plot, aes(x=Year, y=RPPP_Score))+
  geom_point(aes(color="FF0000"), size=2.2)+
  ylab("RPPP Score")+
  ggtitle("RPPP Score for China", subtitle = "Dashed line marks RPPP equilibrium")+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  ylim(-20,20)+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Decima WE"),
    legend.position = "none"
  )+
  scale_x_discrete(name ="Year",
                   limits=c(1990,1995,2000,2005,2010,2015,2018))

# China t-test
c1 <- unfactor(china_short_rpp_values$Spot_rate_change)
c2 <- unfactor(china_short_rpp_values$Inflation_rate_difference)
t.test(c1,c2)

# Plot of RPPP for the UK
uk_plot <- cbind.data.frame(unfactor(uk_short_rpp_values$Year),unfactor(uk_short_rpp_values$RPPP_error),uk_short_rpp_values$Country)
colnames(uk_plot) <- c("Year","RPPP_Score","Country")

# Uk Plot
ggplot(data = uk_plot, aes(x=Year,y=RPPP_Score, colour=Country))+
  geom_point(size=2)+
  scale_colour_manual(values = c("#138808"))+
  ylab("RPPP Score")+
  ggtitle("RPPP Score for the UK", subtitle = "Dashed line marks RPPP equilibrium")+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  ylim(-20,20)+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Decima WE"),
    legend.position = "none"
  )+
  scale_x_discrete(name ="Year",
                   limits=c(1975,1980,1985,1990,1995,2000,2005,2010,2015,2018))

# uk t-test
u1 <- unfactor(uk_short_rpp_values$Spot_rate_change)
u2 <- unfactor(uk_short_rpp_values$Inflation_rate_difference)
t.test(u1,u2)


# ---Does RPPP improve with  increasing dT ? ---
# Dropped from final essay, not enough space.

# Calcualte RPP for every year where t-1 is a fixed base year
get_long_rpp <- function(base_year,end_year,foreign_spot,foreign_cpi,country_label){
  base_spot <- get_rate(foreign_spot,base_year)
  us_base_cpi <- get_rate(us_cpi_annual,base_year)
  f_base_cpi  <- get_rate(foreign_cpi,base_year)
  years <- c(base_year:end_year)

  delta_spot <- (unlist(purrr::map(years,function(x) log(get_rate(foreign_spot,x)) - log(base_spot) )))*100
  home_inflation <- (unlist(purrr::map(years,function(x) log(get_rate(us_cpi_annual,x)) - log(us_base_cpi) )))*100
  f_inflation <- (unlist(purrr::map(years,function(x) log(get_rate(foreign_cpi,x)) - log(f_base_cpi) )))*100
  delta_inflation <- home_inflation - f_inflation
  rppp <- delta_spot - delta_inflation
  absolute_error <- abs(rppp)
  time_period <- c(0:(length(delta_spot)-1))
  out <- as.data.frame(cbind(time_period,delta_spot,home_inflation,f_inflation,delta_inflation,rppp, absolute_error,rep(country_label,times=length(delta_spot))))
  out <- out[-1,]
  return(out)
}

uk_long <- get_long_rpp(1974,2018,uk_spot_rates,uk_cpi_annual,"UK")
uk_l_plot <- cbind.data.frame(unfactor(uk_long$time_period),unfactor(uk_long$rppp),uk_long$V8)
colnames(uk_l_plot) <- c("Time_differential","RPPP_score","Country")

france_long <- get_long_rpp(1999,2018,eu_spot_rates,fr_cpi_annual,"France")
fr_l_plot <- cbind.data.frame(unfactor(france_long$time_period),unfactor(france_long$rppp),france_long$V8)
colnames(fr_l_plot) <- c("Time_differential","RPPP_score","Country")

germany_long <- get_long_rpp(1999,2018,eu_spot_rates,gr_cpi_annual,"Germany")
gr_l_plot <- cbind.data.frame(unfactor(germany_long$time_period),unfactor(germany_long$rppp),germany_long$V8)
colnames(gr_l_plot) <- c("Time_differential","RPPP_score","Country")

china_long <- get_long_rpp(1994,2018,ch_spot_rates,ch_cpi_annual,"China")
ch_l_plot <- cbind.data.frame(unfactor(china_long$time_period),unfactor(china_long$rppp),china_long$V8)
colnames(ch_l_plot) <- c("Time_differential","RPPP_score","Country")

long_plot <- rbind(uk_l_plot,fr_l_plot,gr_l_plot,ch_l_plot)

# Plot
ggplot(data = long_plot, aes(x=Time_differential,y=RPPP_score, colour=Country))+
  geom_line()+
  geom_point(size=0.5)+
  scale_colour_manual(values = c("#138808","#001489","red","#FFCE00","#DD0000"))+
  ylab("RPPP Score")+
  xlab("Time differential (years)")+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  #ylim(-20,20)+
  ggtitle("The affect of an increasing time differential on RPPP score", subtitle = "Dashed line marks RPPP equilibrium")+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Decima WE")
  )
