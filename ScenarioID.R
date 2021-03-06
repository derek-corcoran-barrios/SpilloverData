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
                             str_detect(Scenario, "ppdcam_flat_5000dol_") ~ 6,
                             str_detect(Scenario, "ppdcam_discont_0dol") ~ 7,
                             str_detect(Scenario, "ppdcam_discont_flat_1000dol") ~ 8,
                             str_detect(Scenario, "ppdcam_discont_flat_5000dol") ~ 9))

saveRDS(DF, "Scenario_ID.rds")

#gc()

library(data.table)

DF <- readRDS("Scenario_ID.rds")

DF <- as.data.table(DF)

gc()

Data <- as.data.table(readRDS("All_Data_Long_ID2.rds"))



NewData <- data.table::merge.data.table(x = Data, y = DF, 
                                        by.x = "Scenario", by.y = "Scenario", 
                                        all.x = TRUE, all.y = FALSE)

NewData <- NewData %>% as.data.frame() %>% dplyr::select(-Scenario)

NewData <- NewData %>% mutate_if(is.numeric, as.integer)

saveRDS(NewData, "All_Data_Long_ID3.rds")

Fit <- readRDS("ModelCarryingK.rds")
NewData <- readRDS("All_Data_Long_ID3.rds")

Coefficients <- coefficients(Fit)

NewData <- NewData %>% mutate(K = (-Coefficients[1] - Coefficients[3]*Forest)/(Coefficients[2] + Coefficients[4]*Forest),
                              K = as.integer(round(K)))

saveRDS(NewData, "All_Data_Long_ID3.rds")
