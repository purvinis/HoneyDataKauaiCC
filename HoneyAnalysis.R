

library(dplyr)
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


#349FE4
hcolorsPal <- brewer.pal(5,"YlOrBr")
honeyColors <-colorRampPalette(hcolorsPal)
jackNames = c("white","extra_light_amber",
              "light_amber","amber","dark_amber")
jackNamesRev = c("dark_amber","amber",
                 "light_amber","extra_light_amber","white")
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
#"YlOrBr" or "Oranges"

totgal2 <- na.omit(data) %>% select(gallons) %>% sum
totgalmeas <- data %>% select(gallons) %>% sum
data2 <- na.omit(data) %>%
      mutate(FractOfTot = gallons/totgal2) %>%
   mutate(month = month(date,label=TRUE,abbr=TRUE))%>%
   mutate(year = year(date))

data2$jack <-unlist(lapply(data2$hcolor, colorNames))
data2 <- data.frame(data2)
print(str(data2))

colordist <- data2 %>% 
      group_by(jack) %>%
      summarize(eaColor = sum(gallons))

waterdist<- data2 %>% 
   group_by(water) %>%
   summarize(sumWater = sum(gallons))

totAveWater <- sapply((waterdist$water %*% waterdist$sumWater)/totgal2,sum)

data3 <- data2 %>%arrange(hcolor) %>%  #need Jack colors to be a factor
   mutate(fjack = factor(jack,levels = jackNames))


p1 <- ggplot(data3, aes(x=fjack,y = FractOfTot *100)) +
   geom_col(aes(fill = fjack))+
   theme(axis.text.x =element_text(angle = 90))+
   xlab("Honey color")+
   ylab("Percent of total honey volume")+
   labs(title = "Honey Color Distribuion")+
   scale_fill_manual(NULL, values=hcolorsPal)
png(filename = "Honey_Color_Distribution.png")
print(p1)
dev.off()

p2 <- ggplot(data3, aes(x=water,y = FractOfTot *100)) +
   geom_col(aes(fill = fjack,width = 0.15))+
   theme(axis.text.x =element_text(angle = 90))+
   xlab("Percent water")+
   ylab("Percent of total honey volume")+
   labs(title = "Honey Water content Distribuion")+
   scale_fill_manual(NULL, values=hcolorsPal)
png(filename = "Honey_Water_Distribution.png")
print(p2)
dev.off()

#You need to include stat=identity, which is basically telling ggplot2 you will
#provide the y-values for the barplot, rather than counting the aggregate number
#of rows for each x value, which is the default stat=count.stat="identity",position = "stack".
#Alternatively, use geom_col


p3 <-ggplot(data3, aes(x= month,water))+ 
   geom_point(aes(color = fjack, size = gallons))+
   scale_color_manual(values=hcolorsPal)+
   xlab("Month")+
   ylab("Percent water")+
   labs(title = "Honey Water content by month and color",
        color = "Jack's Color")

png(filename = "PercentWaterByMonthBubble.png")
print(p3)
dev.off()

data4 <- data3 %>%
   group_by(month) %>%
   summarize(avGalMon = mean(gallons))

p4 <-ggplot(data3, aes(x=month,y = gallons/6)) +
   geom_col(aes(fill = water))+
   theme(axis.text.x =element_text(angle = 90))+
   xlab("Month")+
   ylab("Average volume, gallons")+
   labs(title = "Average Honey volume/month")

png(filename = "AverageVolmonth.png")
print(p4)
dev.off()
