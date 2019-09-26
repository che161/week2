#installed.package("tidyverse")
library(tidyverse)
read_csv("data/BOM_data.csv")
BOM <- read_csv("data/BOM_data.csv")

#read_csv("data/BOM_stations.csv",col_names = FALSE)
read_csv("data/BOM_stations.csv")

#Questsion 1
BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% 
  group_by(Station_number) %>% 
  summarise(num_rows = n()) %>% 
  write.csv("res/Question1.csv")

#Questsion 2
BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% 
  mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
  group_by(Month) %>% 
  summarise(mean_tem_diff = mean(Temp_diff)) %>% 
  filter(mean_tem_diff == min(mean_tem_diff)) %>% 
  write.csv("res/Question2.csv")

#Questsion 3
#BOM_Stations <- read_csv("data/BOM_stations.csv",col_names = FALSE)
BOM_Stations <- read_csv("data/BOM_stations.csv")
BOM_Stations
gather(BOM_Stations, Station_number, val, -info) %>% 
  spread(info, val) %>% 
    select(Station_number, state)
#BOM_St_Tidy <- 
MinTemp <- BOM %>% 
    separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
    filter(Temp_min != "-", Temp_max != "-", Rainfall >= 0.) %>% 
    mutate(Temp_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>% 
    group_by(Station_number) %>% 
    summarise(mean_tem_diff = mean(Temp_diff))  
MinTemp 
 
BOM_Stations_Tidy <- gather(BOM_Stations, Station_number, val, -info) %>% 
  spread(info, val) %>% 
  select(Station_number, state)
BOM_Stations_Tidy
# change station number from string to numeric
BOM_Stations_Tidy2 <-   mutate(BOM_Stations_Tidy, station = as.numeric(Station_number)) %>% 
    select(station,state) %>% 
  rename(Station_number = station)

inner_join(MinTemp,BOM_Stations_Tidy2) %>% 
  filter(mean_tem_diff == min(mean_tem_diff))  %>% 
  write.csv("res/Question3.csv")

#Questsion 4
BOM_Stations <- read_csv("data/BOM_stations.csv")
BOM_Stations
Station_Lon <-   gather(BOM_Stations, Station_number, val, -info) %>% 
  spread(info, val) %>% 
  select(Station_number, lon)
# change station number from string to numeric
Station_Lon2 <-   mutate(Station_Lon, station = as.numeric(Station_number)) %>% 
  select(station,lon) %>% 
  rename(Station_number = station)
Station_Lon2

#work out mean solar
MeanSol <- BOM %>% 
  separate(col=Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% 
  filter(Solar_exposure != "-") %>% 
  mutate(Solar = as.numeric(Solar_exposure)) %>% 
  group_by(Station_number) %>% 
  summarise(mean_solar = mean(Solar))  
MeanSol 

inner_join(MeanSol,Station_Lon2) %>% 
  filter(lon == min(lon) | lon == max(lon)) %>% 
  write.csv("res/Question4.csv")
