####################################################
#DATA ANALYSIS TEXTBOOK
#FUNDAMENTALS OF REGRESSION ANALYSIS
#ILLUSTRATION STUDY
#Hotel prices and distance to city center

# several cities and dates
#data downloaded from a hotels price comparison site on Oct 29, 2017


# v 1.1 2020-04-01 huxtable added + graph changes
# v 1.1 2020-04-23 names ok

#################

# CLEAR MEMORY
rm(list=ls())

#install.packages("rms")

# Inmort libraries
library(haven)
library(data.table)
library(rms)
library(plyr)
library(xtabs)
library(dplyr)
library(lspline)
library(tidyverse)
library(huxtable)

select<-dplyr::select
summarize <-dplyr::summarize
filter <- dplyr::filter

# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

data_in <- paste0(dir,"da_data_repo/hotels-europe/clean/")
data_out<- paste0(dir,"da_case_studies/ch09-hotels-europe-stability/")
output  <- paste0(dir,"da_case_studies/ch09-hotels-europe-stability/output/")
func    <- paste0(dir,"da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))



# load in clean and tidy data and create workfile
hotels_europe_price <- read_csv(paste0(data_in,"hotels-europe_price.csv"))
hotels_europe_features <- read_csv(paste0(data_in,"hotels-europe_features.csv"))

data <- left_join(hotels_europe_price, hotels_europe_features, by = "hotel_id")
rm(hotels_europe_price)
rm(hotels_europe_features)

# filter a few cities
data <- data %>% filter(city_actual == "Vienna" | city_actual == "Amsterdam" | city_actual =="Barcelona")
data <- data %>% filter(accommodation_type == "Hotel" | accommodation_type == "Apartment")


# drop long stay , 1000E+
data <- data %>% filter(nnights!=4) %>% filter(price<=1000)


# check for duplicates
data<-data %>% filter(!duplicated(data))

# filter for days
data$date <- ifelse(data$month==11 & data$weekend==0, "2017-NOV-weekday", 
                    ifelse(data$month==11 & data$weekend==1, "2017-NOV-weekend", 
                           ifelse(data$month==12 & data$holiday==1, "2017-DEC-holiday", 
                                  ifelse(data$month==6 & data$weekend==1, "2018-JUNE-weekend", NA))))

data <- data %>% filter(!is.na(date))

# Crosstabs - number of obs
table(data$city)
xtabs(~ data$accommodation_type + data$city, data)
xtabs(~ data$date + data$city, data)

# take log if price
data <- data %>% mutate(lnprice=log(price))

# select variables
data <- data %>% select("hotel_id", "date", "city", "accommodation_type", "stars", "rating", "distance", "price", "lnprice")

write_csv(data,paste(data_out,"hotels_work.csv",sep=""))               

#############################################################################################################################x
# External validity by time
data <- read_csv(paste0(data_out,"hotels_work.csv"))

data <- data %>% filter(stars>=3 & stars <=4) %>%
  filter(accommodation_type=="Hotel") %>%
  filter(city=="Vienna")
table(data$date)

data %>% select(distance,price,lnprice) %>% summary()


# TODO simplify, make it nicer as before

# summary stats by variables
data %>% group_by(date) %>% summarize(mean=mean(distance), min=min(distance),
                                             max=max(distance), median=median(distance),n=n()) 
data %>% group_by(date) %>% summarize(mean=mean(price), min=min(price),
                                      max=max(price), median=median(price),n=n()) 
data %>% group_by(date) %>% summarize(mean=mean(lnprice), min=min(lnprice),
                                      max=max(lnprice), median=median(lnprice),n=n()) 

# Regressions with three dates for textbook
# Original regression
reg_rob<-ols(lnprice ~ lspline(distance, 2)  ,filter(data,date=="2017-NOV-weekday"), x=TRUE)
reg_rob
# show robust SE
robcov(reg_rob)

# Other dates
coeff_intercept <- c()
coeff_dist_0_2 <- c()
coeff_dist_2_7 <- c()
i=0

#for (d in unique(data %>% filter(date!="2017-NOV-weekday") %>% select(date))){
for (d in unique(data %>% filter(date!="2017-NOV-weekday") %>% select(date))) {
  i=i+1
  reg <- ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE)
  print(ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE))
  coeff_intercept[d] <- reg$coefficients[[1]]
  coeff_dist_0_2[d] <- reg$coefficients[[2]]
  coeff_dist_2_7[d] <- reg$coefficients[[3]]
}

d<-data.frame(coeff_intercept, coeff_dist_0_2, coeff_dist_2_7)

# we compare coeffs for different dates
rm(reg, reg_rob, i, coeff_intercept, coeff_dist_0_2, coeff_dist_2_7, d)

# TODO
# Table 9.3 dates, all  hotels
# simplify and use stargazer or estimatR, show regressions with robust SE as in chapter09


# same with hotels restricted to be the same

# first create variable that counts the number of times a hotel is in the data
data <- data %>%
  group_by(hotel_id) %>%
  mutate(hotelcount = n())
table(data$hotelcount)

data <- data %>% filter(hotelcount==4)


# original regression
reg2_rob<-ols(lnprice ~ lspline(distance, 2)  ,filter(data,date=="2017-NOV-weekday"), x=TRUE)
robcov(reg2_rob)

# other dates 
coeff_intercept <- c()
coeff_dist_0_2 <- c()
coeff_dist_2_7 <- c()
i=0

#for (d in unique(data %>% filter(date!="2017-NOV-weekday") %>% select(date))){
for (d in unique(data %>% filter(date!="2017-NOV-weekday") %>% select(date))) {
  i=i+1
  reg <- ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE)
  print(ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE))
  coeff_intercept[d] <- reg$coefficients[[1]]
  coeff_dist_0_2[d] <- reg$coefficients[[2]]
  coeff_dist_2_7[d] <- reg$coefficients[[3]]
}

data.frame(coeff_intercept, coeff_dist_0_2, coeff_dist_2_7)

rm(reg, reg2_rob, i, coeff_intercept, coeff_dist_0_2, coeff_dist_2_7, d)
# ERROR, baseline is not added.

# TODO
# Table 9.4 dates, same hotels


#########################################################################################################################xx
# External validity by city
data <- read_csv(paste0(data_out,"hotels_work.csv"))

data <- data %>% filter(stars>=3 & stars <=4) %>%
  filter(accommodation_type=="Hotel") %>%
  filter(date=="2017-NOV-weekday")

data %>% group_by(city) %>% summarize(mean=mean(distance), min=min(distance),
                                      max=max(distance), median=median(distance),n=n()) 
data %>% group_by(city) %>% summarize(mean=mean(price), min=min(price),
                                      max=max(price), median=median(price),n=n()) 
data %>% group_by(city) %>% summarize(mean=mean(lnprice), min=min(lnprice),
                                      max=max(lnprice), median=median(lnprice),n=n()) 

# Regressions for three cities
# original regression
reg3_rob<-ols(lnprice ~ lspline(distance, 2)  ,filter(data,city=="Vienna"), x=TRUE)
robcov(reg3_rob)

# other cities 
coeff_intercept <- c()
coeff_dist_0_2 <- c()
coeff_dist_2_7 <- c()
i=0

#for (c in unique(data[data$city=="Amsterdam" | data$city=="Barcelona", ]$city)) {
for (c in  c("Amsterdam","Barcelona")) {
  i=i+1
  reg <- ols(lnprice ~ lspline(distance, 2)  ,filter(data,city==c), x=TRUE)
  #print(ols(lnprice ~ lspline(distance, 2)  ,data[data$city==c, ], x=TRUE))
  coeff_intercept[c] <- reg$coefficients[[1]]
  coeff_dist_0_2[c] <- reg$coefficients[[2]]
  coeff_dist_2_7[c] <- reg$coefficients[[3]]
}

data.frame(coeff_intercept, coeff_dist_0_2, coeff_dist_2_7)

rm(reg, reg3_rob, i, coeff_intercept, coeff_dist_0_2, coeff_dist_2_7, c)

# TODO
# Table 9.5 cities

#######################################################################################################################
# External validity by accommodation type: hotels vs apartments
data <- read_csv(paste0(data_out,"hotels_work.csv"))


data <- data %>% filter(stars>=3 & stars <=4) %>%
  filter(city=="Vienna") %>%
  filter(date=="2017-NOV-weekday")


table(data$accommodation_type, data$stars)

data %>% group_by(stars) %>% summarize(mean=mean(distance), min=min(distance),
                                       max=max(distance), median=median(distance),n=n())
data %>% group_by(stars) %>% summarize(mean=mean(price), min=min(price),
                                       max=max(price), median=median(price),n=n()) 
data %>% group_by(stars) %>% summarize(mean=mean(lnprice), min=min(lnprice),
                                       max=max(lnprice), median=median(lnprice),n=n()) 

# regressions
reg4_rob<-ols(lnprice ~ lspline(distance, 2)  ,filter(data,accommodation_type=="Hotel"), x=TRUE)
robcov(reg4_rob)

reg5_rob<-ols(lnprice ~ lspline(distance, 2)  ,filter(data,accommodation_type=="Apartment"), x=TRUE)
robcov(reg5_rob)

# TODO
# Table 9.6 hotels v apartments
rm(reg4_rob, reg5_rob)

