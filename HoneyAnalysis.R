

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

totgal <- data %>% select(gallons) %>% sum


#349FE4
hcolorsPal <- brewer.pal(5,"Oranges")
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
#"YlOrRd" or "Oranges"

totgal2 <- na.omit(data) %>% select(gallons) %>% sum
totgalmeas <- data %>% select(gallons) %>% sum
data2 <- na.omit(data) %>%
      mutate(h20FractOfTot = gallons*water/totgal2) %>%
      mutate(colFractOfTot = gallons*hcolor/totgal2) %>%
   mutate(month = month(date,label=TRUE,abbr=TRUE))%>%
   mutate(year = year(date))

data2$jack <-unlist(lapply(data2$hcolor, colorNames))
data2 <- data.frame(data2)
print(str(data2))

colordist <- data2 %>% 
      group_by(jack) %>%
      summarize(eaColor = sum(gallons))


data3 <- data2 %>%arrange(hcolor) %>%
   mutate(fjack = factor(jack,levels = jackNames))

ggplot(data3, aes(x=fjack,y = colFractOfTot)) +
   geom_col(aes(fill = fjack))+
   theme(axis.text.x =element_text(angle = 90))+
   xlab("Honey color")+
   ylab("Percent of total volume")+
   scale_fill_brewer(palette = "YlOrRd")

#You need to include stat=identity, which is basically telling ggplot2 you will
#provide the y-values for the barplot, rather than counting the aggregate number
#of rows for each x value, which is the default stat=count.stat="identity",position = "stack"

#colordistbymonth <- data3 %>% 
#  arrange(factor(jack,
#                  levels = c("white","extra_light_amber",
#                 "light_amber","amber","dark_amber")))



ggplot(data3, aes(x= month, y= gallons))+
       geom_col(aes(fill = fjack)) 


+
   scale_fill_brewer(palette = " ")
 


waterdistbymonth <- data3 %>% 
   group_by(month)%>%
   summarize(waterav = mean(water))



png(filename = "plot1.png")
print(p1)
dev.off()

                