
library(cowplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(R.utils)
library(lubridate)
library(lattice)
library(RColorBrewer)

data <-read.csv("HoneyData.csv", sep =",")
#clean up messy file
print(names(data))
colnames(data) <- c("date","gallons","water","color","X", "X.1","X.2","X.3","X.4","X.5")
data <- data %>% select(c("date","gallons","water","color"))
colnames(data)<-c("date","gallons","water","hcolor")
str(data)

data$date <- parse_date_time(data$date,"mdy")

totgal <- data %>% select(gallons) %>% sum


#349FE4
hcolorsPal <- brewer.pal(5,"Oranges")
honeyColors <-colorRampPalette(hcolorsPal)
colorNames <- function(mm){
      jack <- NA
      if (is.na(mm)) {jack <- NA
      } else if(mm >= 0 & mm < 34) {jack <- "white"
      } else if(mm >= 34 & mm <50) {jack <- "extra_light_amber"
      } else if(mm >= 50 & mm <85) {jack <- "light_amber"
      } else if(mm >= 85 & mm <114) {jack <- "amber"
      } else if(mm >= 114) {jack <- "dark_amber"
      } else {jack <- NA}
      return(jack)}

#extra light amber (34+ to  50 mm)  
#light amber (50+ to 85 mm)  
#amber (85+ to 114 mm)
#dark amber (114+ mm
#"YlOrRd" or "Oranges"

data2 <- data %>%
      mutate(h20FractOfTot = gallons*water/totgal) %>%
      mutate(colFractOfTot = gallons*hcolor/totgal)

data2$jack <-lapply(data2$hcolor, colorNames)

data3 <- data2 %>% group_by(jack)

colordist <- data2 %>% 
      group_by(jack) %>%
      summarize(eaColor = sum(gallons))

ggplot(data= data3, aes(x=hcolor),na.omit = TRUE) +
      geom_bar(aes(y = gallons),stat="identity",position = "stack")

