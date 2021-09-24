library(tidyverse)

#Collect mean temperature for january and july
july_temp<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tavg-1-7-1895-2021.csv",skip=3, header=T)
january_temp<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tavg-1-1-1895-2021.csv",skip=3, header=T)

combined_temp<-bind_rows(january_temp, july_temp) %>% mutate(Year=as.numeric(substr(Date,1,4)),Month_num=substr(Date,5,6),
                                                             Temperature_celsius=(Value-32)*(5/9)) %>%
  mutate(Month=as.factor(ifelse(Month_num=="01","January","July")))

#Collect max and min temperature for january and july
july_temp_max<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tmax-1-7-1895-2021.csv", skip=3, header=T)
january_temp_max<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tmax-1-1-1895-2021.csv", skip=3, header=T)
july_temp_min<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tmin-1-7-1895-2021.csv", skip=3, header=T)
january_temp_min<- read.csv(file="https://www.ncdc.noaa.gov/cag/statewide/time-series/4-tmin-1-1-1895-2021.csv", skip=3, header=T)

combined_july_max_min<-july_temp_max %>% rename(max_temp=Value) %>% left_join(july_temp_min) %>% rename(min_temp=Value) %>%
  mutate(Year=as.numeric(substr(Date,1,4))) %>%
  mutate(max_temp_c=(max_temp-32)*(5/9),min_temp_c=(min_temp-32)*(5/9))

combined_january_max_min<-january_temp_max %>% rename(max_temp=Value) %>% left_join(january_temp_min) %>% rename(min_temp=Value) %>%
  mutate(Year=as.numeric(substr(Date,1,4))) %>%
  mutate(max_temp_c=(max_temp-32)*(5/9),min_temp_c=(min_temp-32)*(5/9))

january_temp_1901_2000<-combined_temp %>% filter(Year>1900&Year<=2000&Month=="January") %>% select(Temperature_celsius)
average_january_temp_1901_2000<-mean(january_temp_1901_2000$Temperature_celsius)
july_temp_1901_2000<-combined_temp %>% filter(Year>1900&Year<=2000&Month=="July") %>% select(Temperature_celsius)
average_july_temp_1901_2000<-mean(july_temp_1901_2000$Temperature_celsius)


temperature_plot<-ggplot() + 
  geom_line(data=combined_temp,aes(x=Year,y = Temperature_celsius, color=Month)) +
  geom_point(data=combined_temp,aes(x=Year,y = Temperature_celsius, color=Month))+
  scale_colour_manual(values = c("blue","red")) +
  geom_ribbon(data=combined_july_max_min,aes(x=Year,ymin=min_temp_c,ymax=max_temp_c),fill="red",alpha=0.3)+
  geom_ribbon(data=combined_january_max_min,aes(x=Year,ymin=min_temp_c,ymax=max_temp_c),fill="blue",alpha=0.3)+
  geom_hline(yintercept=average_january_temp_1901_2000, color="darkblue",linetype=3)+
  geom_hline(yintercept=average_july_temp_1901_2000, color="darkred",linetype=3)+
  theme_linedraw()
  
  
temperature_plot

tiff(filename="Temperature_Time_Series.tiff", 
     units="in", bg="white", height=6, width=10, res=600, pointsize=6.8, 
     compression="lzw")
temperature_plot
dev.off()


#combined_temp_max_min<- combined_temp %>% group_by(Year) %>% summarise(max_temp=max(Temperature_celsius),min_temp=min(Temperature_celsius))
