library(tidyverse)
library(data.table)

Data <- read_rds("All_Data_Long_ID2.rds") %>% pull(Scenario) %>% unique()

gc()

DF <- tibble(Scenario = Data) %>% 
  mutate(ID_Scen = case_when(str_detect(Scenario, "bau_forest") ~ 1,
                             str_detect(Scenario, "flat_1000d_") ~ 2,
                             str_detect(Scenario, "flat_5000d_") ~ 3,
                             str_detect(Scenario, "ppdcam_0dol_") ~ 4,
                             str_detect(Scenario, "ppdcam_flat_1000dol_") ~ 5,
                             str_detect(Scenario, "ppdcam_flat_5000dol_") ~ 6))

saveRDS(DF, "Scenario_ID.rds")

gc()

library(data.table)

DF <- readRDS("Scenario_ID.rds")

DF <- as.data.table(DF)

gc()

Data <- as.data.table(readRDS("All_Data_Long_ID2.rds"))



NewData <- list()

for(i in 1:nrow(DF)){
  Temp <- Data[Scenario == DF$Scenario[i]]
  Temp <- data.table::merge.data.table(x = Temp, y = DF, 
                                       by.x = "Scenario", by.y = "Scenario", 
                                       all.x = TRUE, all.y = FALSE)
  Temp <- Temp[,Scenario:=NULL]
  NewData[[i]] <- Temp
  Data <- Data[!(Scenario %chin% DF$Scenario[1:i])]
  rm(Temp)
  gc()
  saveRDS(NewData, "All_Data_Long_ID3.rds")
  message(paste(i, "of", nrow(DF)))
}

