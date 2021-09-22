library(tidyverse)

july_temp<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tavg-1-7-1895-2021.csv",skip=3, header=T)

january_temp<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tavg-1-1-1895-2021.csv",skip=3, header=T)

combined_temp<-bind_rows(january_temp, july_temp) %>% mutate(Year=as.numeric(substr(Date,1,4)),Month_num=substr(Date,5,6),
                                                             Temperature_celsius=(Value-32)*(5/9)) %>%
  mutate(Month=as.factor(ifelse(Month_num=="01","January","July")))


combined_temp_max_min<- combined_temp %>% group_by(Year) %>% summarise(max_temp=max(Temperature_celsius),min_temp=min(Temperature_celsius))

temperature_plot<-ggplot(data=combined_temp) + 
  geom_line(aes(x=Year,y = Temperature_celsius, color=Month)) +
  geom_point(aes(x=Year,y = Temperature_celsius, color=Month))+
  geom_ribbon(data=combined_temp_max_min,aes(x=Year,ymin=min_temp,ymax=max_temp, fill="blue", alpha=0.5))+theme_classic()
  
  
temperature_plot

tiff(filename="Temperature_Time_Series.tiff", 
     units="in", bg="white", height=4, width=12, res=600, pointsize=6.8, 
     compression="lzw")
temperature_plot
dev.off()