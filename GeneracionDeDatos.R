library(sf)
library(tidyverse)
library(raster)


Mold <- read_rds("Mold.rds")

Equal <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

#Area <- read_csv("Defor Data/uni_id_area.csv")


Files <- list.files(path = "Defor Data", pattern = ".csv", recursive = T, full.names = T)

Files <- Files[Files != "Defor Data/uni_id_area.csv"]

Files <- Files[str_detect(Files, "_forest")]

Outputs <- Files %>% str_replace_all("Defor Data/", "Output/") %>% str_replace_all(".csv", ".rds")
Folders <- data.frame(Outputs = Outputs) %>% 
  tidyr::separate(Outputs, into = c("V1", "V2", "V3"), sep = "/") %>%
  mutate(Folder1 = V1, Folder2 = paste(V1, V2, sep = "/"), Scenario = str_remove_all(V3, ".rds")) %>% 
  dplyr::select(Folder1, Folder2, Scenario) %>% 
  dplyr::filter(str_detect(Scenario, "_forest"))