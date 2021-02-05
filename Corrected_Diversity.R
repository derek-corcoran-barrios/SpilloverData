library(tidyverse)
library(raster)

All_Data_Long_ID <- readRDS("ID_Table_Full.rds")
Richness_Mammals <- readRDS("Richness_Mammals.rds")

Mammal_Rich_ID <- raster::extract(Richness_Mammals, All_Data_Long_ID[,-1]) %>% as.data.frame() 

Mammal_Rich_ID <- bind_cols(All_Data_Long_ID, Mammal_Rich_ID)

colnames(Mammal_Rich_ID)[4] <- "Mammal_Richness"

saveRDS(Mammal_Rich_ID, "Mammal_Rich_Temp.rds")

library(tidyverse)
library(raster)

All_Data_Long_ID <- readRDS("ID_Table_Full.rds")
Richness_Birds <- readRDS("Richness_Birds.rds")

Bird_Rich_ID <- raster::extract(Richness_Birds, All_Data_Long_ID[,-1]) %>% as.data.frame() 

Bird_Rich_ID <- bind_cols(All_Data_Long_ID, Bird_Rich_ID)

colnames(Bird_Rich_ID)[4] <- "Bird_Richness"

saveRDS(Bird_Rich_ID, "Bird_Rich_Temp.rds")

All_Rich_ID <- full_join(Bird_Rich_ID, Mammal_Rich_ID)
saveRDS(All_Rich_ID, "All_Rich_Temp.rds")
library(dplyr)
All_Data_Long_ID <- readRDS("All_Data_Long_ID.rds") %>% dplyr::select(-Birds, -Mammals, -Vertebrates)
saveRDS(All_Data_Long_ID, "All_Data_Long_ID2.rds")
#readr::write_csv(All_Data_Long_ID, "All_Data_Long_ID2.csv")

print(nrow(All_Data_Long_ID))

rm(All_Data_Long_ID)

gc()

library(readr)
All_Data_Long_ID <- readr::read_rds("All_Data_Long_ID2.rds") 

