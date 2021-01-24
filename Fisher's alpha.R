  library(tidyverse)
  library(rvest)
  
  
  Test2 <- list()
  
  temp <- tibble(Richness = NA, 
                 count = NA,
                 Fishers_Alpha = NA,
                 Goods_U = NA,
                 Shannons_H = NA)
  
  Code <- ".panel div .myPanel div div"
  
  for(i in 3299:3500){
      url <- paste0("http://ecoregister.org/?a=samplePage&sample_no=",i)
    Temp <- read_html(url) %>% 
      html_nodes(Code) %>% 
      html_text()
  
    Test2[[i]] <- temp
    Test2[[i]]$Richness <- Temp[1] %>% str_remove_all(" species") %>% as.numeric()
    Test2[[i]]$count <- Temp[3] %>% str_remove_all("total count ") %>% as.numeric()
    Test2[[i]]$Fishers_Alpha <- Temp[5] %>% str_remove_all("Fisher's α: ") %>% as.numeric()
    Test2[[i]]$Goods_U <- Temp[9] %>% str_remove_all("Good's u: ") %>% as.numeric()
    Test2[[i]]$Shannons_H <- Temp[8] %>% str_remove_all("Shannon's H: ") %>% as.numeric()
    if((i %% 100) == 0){
      message(paste(i, "Already"))
      saveRDS(Test2, "Alroy_Data.rds")
    }
  }
  
  Test2 <- readRDS("Alroy_Data.rds")
  
  Data <- Test2 %>% 
    reduce(bind_rows) %>% 
    dplyr::filter(!is.na(Fishers_Alpha))
  
  saveRDS(Data, "Alroy_Data.rds")
  
  ggplot(Data, aes(x = Richness, y = Fishers_Alpha)) + 
    geom_point(aes(size = count)) + 
    geom_smooth(method = "lm") +
    theme_bw() 
  
  ggplot(Data, aes(x = Richness, y = count)) + 
    geom_point(aes(size = count)) + 
    geom_smooth(method = "lm") +
    theme_bw() 
  
  ggplot(Data, aes(x = Fishers_Alpha, y = count)) + 
    geom_point(aes(size = count)) + 
    geom_smooth(method = "lm") +
    theme_bw()  +
    scale_y_log10() +
    scale_x_log10()
  