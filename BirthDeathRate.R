library(raster)
library(rworldxtra)
library(sf)
library(tidyverse)

data("countriesHigh")

Mold <- read_rds("Mold.rds")

World <- countriesHigh %>% 
  st_as_sf() %>% 
  st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% 
  st_make_valid() %>% 
  st_crop(extent(Mold)) %>% 
  rename(code = ISO3) %>% 
  mutate(code = as.character(code), code = ifelse(code == "SDS", "SSD", code)) %>% 
  select(code)

rm(countriesHigh)

plot(World["code"])


Birth <- read_csv("birth-rate-vs-death-rate.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::filter(!is.na(birth_rate_crude_per_1_000_people), !is.na(death_rate_crude_per_1_000_people)) %>% 
  group_split(code) %>% 
  purrr::map(~dplyr::filter(.x, year == max(year))) %>% 
  reduce(bind_rows) %>% 
  dplyr::select(code, birth_rate_crude_per_1_000_people, death_rate_crude_per_1_000_people)
gc()

World2 <- World %>% merge(Birth) 

Birth_rate <- rasterize(as_Spatial(World2), Mold, field = "birth_rate_crude_per_1_000_people")

Death_rate <- rasterize(as_Spatial(World2), Mold, field = "death_rate_crude_per_1_000_people")

plot(Death_rate)


library(tidyverse)
library(raster)

All_Data_Long_ID <- readRDS("ID_Table_Full.rds")
All_Rich_Temp <- readRDS("All_Rich_Temp.rds")

B_ID <- raster::extract(Birth_rate, All_Rich_Temp[,2:3]) %>% as.data.frame()
colnames(B_ID) <- "Birth"
D_ID <- raster::extract(Death_rate, All_Rich_Temp[,2:3]) %>% as.data.frame()
colnames(D_ID) <- "Death"

One_Time <- list(All_Rich_Temp, B_ID, D_ID) %>% reduce(bind_cols) 


library(recipes)


rec <- recipe(Bird_Richness ~.,
              data = One_Time)

ratio_recipe <- rec %>%
  step_knnimpute(all_predictors(), neighbors = 10)
ratio_recipe2 <- prep(ratio_recipe, training = One_Time)
imputed <- bake(ratio_recipe2, One_Time)

saveRDS(imputed, "Imputed.rds")

imputed <- imputed %>% dplyr::select(-ip.Death, -x, -y)

imputed <- imputed %>% relocate(Bird_Richness, .before = "Birth")
saveRDS(imputed, "Imputed.rds")

imputed <- imputed %>% mutate(Richness = Bird_Richness + Mammal_Richness)

imputed$Alpha <- predict(powerlaw.model, newdata = imputed)

imputed <- imputed %>% mutate(N = ((exp(Richness/Alpha))-1)*Alpha, N = ifelse(is.nan(N), 0, N))

saveRDS(imputed, "Imputed.rds")
