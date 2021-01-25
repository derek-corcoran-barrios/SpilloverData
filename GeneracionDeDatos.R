library(sf)
library(tidyverse)
library(raster)
require("ncdf4")
library(tidyverse)

dir.create("Temp")
rasterOptions(tmpdir = paste0(getwd(), "/Temp"))

### Agregar Poblacion

# Coso <- stack("gpw_v4_population_density_adjusted_rev11_2pt5_min.nc")
# 
# Coso <- Coso[[1:5]]
# 
# Coso <- Coso*area(Coso)
# 
# names(Coso) <- paste0("Year", seq(2000, 2020, by = 5))
# 
# Coso <- projectRaster(Coso, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# 
# Coso <- readAll(Coso)
# 
# saveRDS(Coso, "TotalPoblacional.rds")

Coso <- read_rds("TotalPoblacional.rds")

Mold <- read_rds("Mold.rds")

Equal <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

#Area <- read_csv("Defor Data/uni_id_area.csv")


Files <- list.files(path = "Defor_Data", pattern = ".csv", recursive = T, full.names = T)

Files <- Files[Files != "Defor Data/uni_id_area.csv"]
Files <- Files[str_detect(Files, "dvc", negate = T)]

Files <- Files[str_detect(Files, "_forest")]

Outputs <- Files %>% str_replace_all("Defor_Data/", "Output/") %>% str_replace_all(".csv", ".rds")
Folders <- data.frame(Outputs = Outputs) %>% 
  tidyr::separate(Outputs, into = c("V1", "V2", "V3"), sep = "/") %>%
  mutate(Folder1 = V1, Folder2 = paste(V1, V2, sep = "/"), Scenario = str_remove_all(V3, ".rds")) %>% 
  dplyr::select(Folder1, Folder2, Scenario) %>% 
  dplyr::filter(str_detect(Scenario, "_forest"))

## Diversidad

Birds <- raster("bird_aoh_10km_richness.tif")
Mammals <- raster("mammals_aoh_10km_richness.tif")

All <- stack(Birds, Mammals)
All <- projectRaster(All, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

rm(Birds)
rm(Mammals)

gc()

for(i in 1:length(Files)){ 
  dir.create("Temp")
  rasterOptions(tmpdir = paste0(getwd(), "/Temp"))
  
  Area <- read_csv("Defor_Data/uni_id_area.csv")
  Temp <- read_csv(Files[i]) %>% 
    left_join(Area)
  rm(Area)
  
  gc()
  
  message("Join with shapefile")
  
  ShapeTemp <- read_rds("shape.rds")  %>% 
    left_join(Temp)
  rm(Temp)
  
  gc()
  
  ShapeTemp <- ShapeTemp %>% dplyr::filter_at(vars(contains("for_")), ~!is.na(.x)) %>% mutate_at(vars(contains("for_")), ~round((.x/area)*100))
  
  ShapeTemp <- ShapeTemp %>% dplyr::select(contains("for_")) %>% 
    mutate_if(is.numeric,~case_when(.x >= 100 ~ 100,
                                    .x <= 0 ~ 0,
                                    TRUE ~ .x))
  Cols <- colnames(ShapeTemp)
  Cols <- Cols[Cols != "geometry"]
  Years <- Cols %>% str_remove_all("defor_") %>% str_remove_all("for_") %>% as.numeric()
  
  Test_DF <- list()
  
  dir.create(Folders$Folder1[i])
  dir.create(Folders$Folder2[i])
  message("Starting a list")
  for(j in 1:length(Cols)){
    Test <-  ShapeTemp %>% fasterize::fasterize(raster = Mold, field = Cols[j])
    Test <- Test %>% trim()
    # saveRDS(Test, paste0("Output/flat_1000d_forest_asia/", "for", Years[i], ".rds"))
    Test_DF[[j]] <- Test %>% 
      as("SpatialPixelsDataFrame") %>% 
      as.data.frame()
    Test_DF[[j]]$Year = NA
    Test_DF[[j]]$Year = Years[j]
    # saveRDS(Test_DF,"Output/flat_1000d_forest_asia/for_DF.rds")
    message(paste(j, "of", length(Cols)))
    gc()
  }
  unlink(paste0(getwd(), "/Temp"), recursive = T)
  Long_format <- bind_rows(Test_DF) %>% rename(Forest = layer) %>% mutate(Scenario = Folders$Scenario[i])
  saveRDS(Long_format,Outputs[i])
  gc()
  TotalPoblacional <- resample(Coso, Test)
  
  Temp <- TotalPoblacional[[1]]
  
  values(Temp) <- NA
  
  NamesLayers <- paste0("Year", 2000:2020)
  
  TotalPoblacional <- stack(TotalPoblacional[[1]], Temp, Temp, Temp, Temp,
                            TotalPoblacional[[2]], Temp, Temp, Temp, Temp,
                            TotalPoblacional[[3]], Temp, Temp, Temp, Temp,
                            TotalPoblacional[[4]], Temp, Temp, Temp, Temp,
                            TotalPoblacional[[5]])
  
  names(TotalPoblacional) <- NamesLayers
  
  TotalPoblacional <- approxNA(TotalPoblacional)
  
  
  
  TotalPoblacional_DF <- TotalPoblacional %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame() %>% 
    pivot_longer(starts_with("Year"), values_to = "Population", names_to = "Year") %>% 
    mutate(Population = as.integer(round(Population)), Year = as.integer(as.numeric(str_remove_all(Year, "Year"))))
  
  
  
  All_Data_Long <- Long_format %>% left_join(TotalPoblacional_DF)
  
  All_Data_Long_ID <- All_Data_Long %>% 
    group_by(x, y) %>% 
    mutate(ID =cur_group_id()) %>% 
    ungroup()
  
  ID_Table <- All_Data_Long_ID %>% 
    dplyr::select(ID,x,y) %>% 
    distinct()
  
  All_Data_Long_ID <- All_Data_Long_ID %>%
    dplyr::select(Forest, Year, Population, ID) %>% 
    mutate_if(is.numeric, as.integer)
  
  saveRDS(All_Data_Long_ID, paste0(Folders$Folder2[i], "/",Folders$Scenario[i], "_Data_ID.rds"))

  
  saveRDS(ID_Table, paste0(Folders$Folder2[i], "/",Folders$Scenario[i], "_ID_Table.rds"))
  
  gc()
  
  All_Temp <- resample(All, Test)
  
  All_Temp_DF <- All_Temp %>% 
    as("SpatialPixelsDataFrame") %>% 
    as.data.frame() %>% 
    right_join(ID_Table) %>% 
    dplyr::select(-x,-y) %>% 
    mutate_all(round) %>% 
    mutate_all(as.integer) %>%
    rename(Mammals = mammals_aoh_10km_richness, Birds = bird_aoh_10km_richness) %>% 
    mutate(Vertebrates = Mammals + Birds) %>% 
    full_join(All_Data_Long_ID) %>% 
    relocate(ID, .before = where(is.integer)) %>% 
    arrange(Year, ID) %>% 
    mutate(Scenario = Folders$Scenario[i])
  
  
  saveRDS(All_Temp_DF, paste0(Folders$Folder2[i], "/",Folders$Scenario[i], "_Data_Complete_ID.rds"))
  gc()
  print(paste("Scenario", i, "of" ,length(Files), "ready!", Sys.time()))
}


## Get it all together

Datas <- list.files(path ="Output", pattern = "Data_Complete_ID", full.names = T, recursive = T) 

Ids <- list.files(path ="Output", pattern = "ID_Table", full.names = T, recursive = T) 

Finals <- list()

for(i in 1:length(Datas)){
  TempData  <- readRDS(Datas[i])
  TempID <- readRDS(Ids[i])
  Temp <-full_join(TempData, TempID)
  Finals[[i]] <- Temp %>% dplyr::select(-ID)
  rm(TempData)
  rm(Temp)
  rm(TempID)
  gc()
  message(i)
}



Finals <- Finals %>% purrr::reduce(bind_rows) %>% 
  group_by(x, y) %>% 
  mutate(ID =cur_group_id()) %>% 
  ungroup()


ID_Table <- Finals %>% 
  dplyr::select(ID,x,y) %>% 
  distinct()

saveRDS(ID_Table, "ID_Table_Full.rds")

All_Data_Long_ID <- Finals %>%
  dplyr::select(Forest, Year, Population, Birds, Mammals, Vertebrates, ID) %>% 
  mutate_if(is.numeric, as.integer)

saveRDS(All_Data_Long_ID, "All_Data_Long_ID.rds")