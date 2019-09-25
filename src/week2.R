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
BOM %>% 
  filter(Temp_min_max > 0., Rainfall > 0.) %>% 
  group_by(Station_number) %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Temp_min > 0., Temp_max > 0.) %>%   
  mutate(Temp_diff = Temp_max - Temp_min ) %>% 
  summarise(Temp_m = mean(Temp_min))