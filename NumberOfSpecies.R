library(raster)
library(sf)
library(tidyverse)

Files <- list.files(path = "Mammals/", pattern = ".tif", full.names = T)

Mold <- read_rds("Mold.rds")

Richness <- raster(Files[1]) #%>% projectRaster(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", method="ngb") %>% crop(Mold)
Richness[is.na(Richness[])] <- 0 
m <- c(0, Inf, 1, -Inf, 0, 0)
m <- matrix(m, ncol=3, byrow=TRUE)
Richness <- reclassify(Richness, m)

for(i in 2:length(Files)){
  dir.create(paste0(getwd(),"/Temp"))
  rasterOptions(tmpdir = paste0(getwd(),"/Temp"))
  Temp <- raster(Files[i]) #%>% projectRaster(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", method="ngb") %>% crop(Mold)
  Temp[is.na(Temp[])] <- 0
  m <- c(0, Inf, 1, -Inf, 0, 0)
  m <- matrix(m, ncol=3, byrow=TRUE)
  Temp <- reclassify(Temp, m)
  Richness <- Richness + Temp
  
  unlink(paste0(getwd(),"/Temp"))

  gc()
  if((i %% 250) == 0){
    message(paste(i, "of", length(Files), Sys.time()))
    plot(Richness, main = paste(i, "Species"))
    saveRDS(Richness, "Richness_Mammals.rds")
  }
}

saveRDS(Richness, "Richness_Mammals.rds")
Richness<- readRDS("Richness_Mammals.rds")
unlink(paste0(getwd(),"/Temp"), recursive = T)
Richness <- Richness %>% 
  projectRaster(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", method="ngb") %>% 
  crop(Mold)

saveRDS(Richness, "Richness_Mammals.rds")


Files <- list.files(path = "Birds/", pattern = ".tif", full.names = T)

Mold <- read_rds("Mold.rds")

Richness <- raster(Files[1]) #%>% projectRaster(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", method="ngb") %>% crop(Mold)
Richness[is.na(Richness[])] <- 0 
m <- c(0, Inf, 1, -Inf, 0, 0)
m <- matrix(m, ncol=3, byrow=TRUE)
Richness <- reclassify(Richness, m)

for(i in 2:length(Files)){
  dir.create(paste0(getwd(),"/Temp"))
  rasterOptions(tmpdir = paste0(getwd(),"/Temp"))
  Temp <- raster(Files[i]) #%>% projectRaster(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", method="ngb") %>% crop(Mold)
  Temp[is.na(Temp[])] <- 0
  m <- c(0, Inf, 1, -Inf, 0, 0)
  m <- matrix(m, ncol=3, byrow=TRUE)
  Temp <- reclassify(Temp, m)
  Richness <- Richness + Temp
  
  unlink(paste0(getwd(),"/Temp"))
  
  gc()
  if((i %% 250) == 0){
    message(paste(i, "of", length(Files), Sys.time()))
    plot(Richness, main = paste(i, "Species"))
    saveRDS(Richness, "Richness_Birds.rds")
  }
}

saveRDS(Richness, "Richness_Birds.rds")
Richness<- readRDS("Richness_Birds.rds")
unlink(paste0(getwd(),"/Temp"), recursive = T)
Richness <- Richness %>% 
  projectRaster(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", method="ngb") %>% 
  crop(Mold)

saveRDS(Richness, "Richness_Birds.rds")