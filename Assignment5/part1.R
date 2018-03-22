---
title: "part1"
author: "Zirui Liu, Lingjia Zhang, Lin Li"
date: "3/18/2018"
---
  
## There is no data for station 46035 in 2013. Some temperture data is 999.
## We skip data for 2013 and ignore temperature data that is invalid.

library(tidyverse)
library(stringr)
library(zoo)
library(shiny)
library(reshape2)
library(ggplot2)
#install.packages("devtools")
library(devtools)
#install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

#1 Visualize data
url_a <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url_b <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1988:2017)
#urls <- str_c(url_a, years, url_b, sep = "")
filenames <- str_c("mr", years, sep = "")
N <- length(urls)

# Takes year as input and returns full url address
get_url <- function(i){
  urls <- str_c(url_a, i, url_b, sep = "")
  urls
}

# Takes year as input and returns filename
get_filenames <- function(i){
  filenames <- str_c("mr", i, sep = "")
  filenames
}

# get historical data based on url and filename
get_MR <- function(x, y){
  N <- length(x)
  for (i in 1:N){
    suppressMessages(assign(y[i], read_table(x[i], col_names = TRUE)))
    file <- get(y[i])
    colnames(file)[1] <- "YYYY"
    file$YYYY <- as.numeric(file$YYYY)
    
    if(i == 1){
      MR <- file
    }
    else{
      MR <- rbind(MR, file)
    }
  }
  MR
}

# Takes year as input and sets temperature out of range as NA
clean_mr <- function(i){
  a <- get_MR(get_url(i), get_filenames(i))
  b <- a%>% select(YYYY, MM, DD, ATMP, WTMP)
  b$ATMP <- as.numeric(b$ATMP)
  b$WTMP <- as.numeric(b$WTMP)
  b$ATMP[b$ATMP>100] <- NA
  b$WTMP[b$WTMP>100] <- NA
  b
  }

# data from 1987 to 2011
for (i in c(1987:1998)){
  if(i == 1987){
    data1987_1998 <- clean_mr(i)
  }else{
    data1987_1998 <- rbind(data1987_1998, clean_mr(i))
  }
}

data1987_1998$YYYY <- paste("19",data1987_1998$YYYY,sep="")

for (i in c(1999:2011)){
  if(i == 1999){
    data1999_2011 <- clean_mr(i)
  }else{
    data1999_2011 <- rbind(data1999_2011, clean_mr(i))
  }
}

# data from 2012 to 2014
data2012 <- clean_mr(2012)[-1,]
data2013 <- clean_mr(2013)[-1,]
data2014 <- clean_mr(2014)[-1,]

# data from 2015 to 2017
for (i in c(2015:2017)){
  if(i == 2015){
    data2015_2017 <- clean_mr(i)
  }else{
    data2015_2017 <- rbind(data2015_2017, clean_mr(i))
  }
}

# total data
data_total <- rbind(data1987_1998,data1999_2011,data2012,data2013,data2014,data2015_2017)

data_total <-data_total %>% filter(!is.na(ATMP))
data_total <-data_total %>% filter(!is.na(WTMP))
data_total$ATMP<-as.numeric(data_total$ATMP)
data_total$WTMP<-as.numeric(data_total$WTMP)

# get the time of each data
data_total$Time <-strptime(with(data_total, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")
data_total$Time <- as.Date(data_total$Time)

# get data at noon everyday
daily<-subset(data_total,hh==12)

# get yearly average data
for (i in 1987:2017){
  if(i==1987){
    avg <- data_total %>% filter(YYYY==i) %>% group_by(YYYY) %>% summarise(avg_atmp=mean(ATMP),avg_wtmp=mean(WTMP))
  }
  else{
    avg <- rbind(avg, data_total %>% filter(YYYY==i) %>% group_by(YYYY) %>% summarise(avg_atmp=mean(ATMP),avg_wtmp=mean(WTMP)))
  }
}

# plot daily temperature
plot_daily <-ggplot(daily, aes(x = Time)) + 
  geom_line(aes(y = WTMP), colour="blue", size = 0.7) + 
  geom_line(aes(y = ATMP), colour = "red", size = 0.7) +
  ylab(label="Celsius degree") + 
  xlab("Year")+
  ggtitle("Daily Temperature")
plot_daily

#save the file
#write.csv(data_total,"total_data.csv",row.names = FALSE)
#write.csv(avg,"avg.csv",row.names = FALSE)


#2 Explore data

#a Is there significant change of temperature between 1988 and 2017?
data_1987 <- data_total %>% filter(YYYY==1987) 
data_2017 <- data_total %>% filter(YYYY==2017) 

# Compare air temperature
t.test(data_1987$ATMP,data_2017$ATMP)
# The result is p < 2.2e^16 < 0.05. Thus, we reject the null hypothesis.
# Between 1988 and 2017, air temperature has the same mean.

# Compare water temperature
t.test(data_1987$WTMP,data_2017$WTMP)
# The result is p < 2.2e^16 < 0.05. Thus, we reject the null hypothesis.
# Between 1988 and 2017, water temperature has the same mean.

# In conclusion, there is no significant change of temperature between 1988 and 2017.

#b What is trend of temperature changing?
# Use liner regression to visualize the temperature change
reg_ATMP=ggplot(daily,aes(x=Time,y=ATMP))+geom_point()+geom_smooth(method="lm")+ggtitle("Air temperature")
reg_WTMP=ggplot(daily,aes(x=Time,y=WTMP))+geom_point()+geom_smooth(method="lm")+ggtitle("Water temperature")
ggplot2.multiplot(reg_ATMP,reg_WTMP)
# In conclusion, from liner regression line, we can see the temperature is rising.


