#installed.package("tidyverse")
library(tidyverse)
read_csv("data/BOM_data.csv")
BOM <- read_csv("data/BOM_data.csv")
BOM %>% 
  filter(Temp_min_max > 0., Rainfall > 0.) %>% 
  group_by(Station_number) %>% 
  summarise(num_rows = n())


BOM2 <- BOM %>% 
  filter(Temp_min_max > 0., Rainfall > 0.) %>% 
  group_by(Station_number) %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Temp_min > 0., Temp_max > 0.)
BOM2 %>%  mutate(Temp_diff = Temp_max - Temp_min )


read_csv("data/BOM_stations.csv")
BOM_Stations <- read_csv("data/BOM_stations.csv")
?filter
write.csv("res/By_Station.csv")
?mutate

#Challenge 1
BOM %>% 
  filter(Temp_min_max > -99., Rainfall >= 0.) %>% 
  group_by(Station_number) %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Temp_min > -99., Temp_max > -99.) %>%   
  summarise(num_rows = n())

#Questsion 1
BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% 
  group_by(Station_number) %>% 
  summarise(num_rows = n())

#Questsion 2
BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  group_by(Station_number,Month) %>% 
  summarise(mean_tem_diff = mean(Temp_diff)) %>% 
  filter(mean_tem_diff == min(mean_tem_diff))  

#Questsion 3