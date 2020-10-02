
library(cowplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(R.utils)
library(lubridate)
library(lattice)

data <-read.csv("HoneyData.csv", sep =",")
#clean up messy file
print(names(data))
colnames(data) <- c("date","gallons","water","color","X", "X.1","X.2","X.3","X.4","X.5")
data <- data %>% select(c("date","gallons","water","color"))
str(data)

data$date <- parse_date_time(data$date,"mdy")

totgal <- data %>% select(gallons) %>% sum
