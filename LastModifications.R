library(tidyverse)

Imputed <- readRDS("Imputed.rds")

Imputed <- Imputed %>% mutate(I_c = N*0.08, I_c_Bats = N_bats*0.08)

saveRDS(Imputed, "Imputed.rds")


## CropArea

library(raster)

Mold <- readRDS("Mold.rds")

e <- new("Extent", xmin = -5987108.08220469, xmax = -3214144.65769225, 
         ymin = -2622929.84025584, ymax = 1044537.91474447)



ID_Table <- readRDS("ID_Table_Full.rds") %>% dplyr::filter(x >= e[1], x <= e[2], 
                                                           y >= e[3], y <=e[4]) %>% 
  st_as_sf(coords = c(2,3),crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  



ImputedSub <-  Imputed %>% dplyr::filter(ID %in% ID_Table$ID)
saveRDS(ImputedSub, "ImputedSub.rds")

AllDataSub <- readRDS("All_Data_Long_ID3.rds") %>% dplyr::filter(ID %in% ID_Table$ID)

AllDataSub <- AllDataSub %>% dplyr::filter(ID_Scen %in% c(1,5))


saveRDS(AllDataSub, "AllDataSub.rds")


library(sf)

ID_Table <- readRDS("ID_Table_Full.rds") %>% dplyr::filter(x >= e[1], x <= e[2], 
                                                           y >= e[3], y <=e[4]) %>% 
  st_as_sf(coords = c(2,3),crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

Test <- AllDataSub %>% dplyr::select(ID, ID_Scen, Forest, Year) %>% dplyr::filter(Year == 2050) %>% pivot_wider(names_from = ID_Scen, values_from = Forest) %>% rename(Scen1= `1`, Scen5= `5`) %>% mutate(DeltaScen = Scen1 - Scen5) %>% slice_min(order_by = DeltaScen, n = 2000) %>% arrange(DeltaScen)

IDMax <- ID_Table %>% dplyr::filter(ID == Test$ID[1])

buffer <- IDMax %>% st_buffer(dist = 110000)

ID_Table <- readRDS("ID_Table_Full.rds") %>% dplyr::filter(x >= e[1], x <= e[2], 
                                                           y >= e[3], y <=e[4]) %>% 
  st_as_sf(coords = c(2,3),crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% st_crop(buffer)


ImputedSub <-  Imputed %>% dplyr::filter(ID %in% ID_Table$ID)
saveRDS(ImputedSub, "ImputedSub2.rds")

AllDataSub <- readRDS("All_Data_Long_ID3.rds") %>% dplyr::filter(ID %in% ID_Table$ID)

AllDataSub <- AllDataSub %>% dplyr::filter(ID_Scen %in% c(1,5))


saveRDS(AllDataSub, "AllDataSub2.rds")
