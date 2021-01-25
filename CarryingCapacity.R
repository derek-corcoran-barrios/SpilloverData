library(tidyverse)

Data <- read_rds("All_Data_Long_ID.rds")

Data <- Data %>% dplyr::select(ID, Year, Population, Forest)

Data <- Data %>% arrange(ID, Year) %>% 
  group_by(ID) %>% 
  mutate(T_1 = lead(Population))

Data <- Data[complete.cases(Data),]

Data <- Data %>% 
  group_by(ID) %>% 
  mutate(r = log(T_1/Population)) %>% 
  dplyr::filter(!is.na(r))



library(glmnet)
x <- model.matrix(r ~ Population*Forest, data = Data)
x <- x[, -1]
y <- Data$r



set.seed(2021)
ridge <- cv.glmnet(x, y, alpha = 0, nfolds = 10, lower.limits = -Inf, upper.limits = c(0, Inf, Inf))

DF <- ridge$glmnet.fit$beta %>% as.matrix() %>% t() %>% as.data.frame()

DF$Lambda <- ridge$glmnet.fit$lambda

DF <- DF %>% pivot_longer(-Lambda, names_to = "Parametro", values_to = "Estimador")

ggplot(DF, aes(x = Lambda, y = Estimador, color = Parametro)) + geom_path() + geom_vline(xintercept = ridge$lambda.min, color = "red", lty = 2) + theme_bw() 


Mejor_Ridge <- glmnet(x, y, alpha = 0, lambda = ridge$lambda.min, lower.limits = -Inf, upper.limits = c(0, Inf, Inf))

broom::tidy(Mejor_Ridge)
saveRDS(Mejor_Ridge, "ridge.rds")

set.seed(2020)
lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10, lower.limits = -Inf, upper.limits = c(-0.0000001, Inf, Inf))

DF <- lasso$glmnet.fit$beta %>% as.matrix() %>% t() %>% as.data.frame()

DF$Lambda <- lasso$glmnet.fit$lambda

DF <- DF %>% pivot_longer(-Lambda, names_to = "Parametro", values_to = "Estimador")

ggplot(DF, aes(x = Lambda, y = Estimador, color = Parametro)) + geom_path() + geom_vline(xintercept = lasso$lambda.min, color = "red", lty = 2) + theme_bw() 


Mejor_Lasso <- glmnet(x, y, alpha = 1, lambda = lasso$lambda.min, lower.limits = -Inf, upper.limits = c(-0.0000001, Inf, Inf))

saveRDS(Mejor_Lasso, "Lasso.rds")


set.seed(2020)
ElasticNet <- cv.glmnet(x, y, alpha = 0.5, nfolds = 10)

Mejor_Lasso <- readRDS("Lasso.rds")

NewDF <- expand.grid(Population = seq(from = 0, to = max(Data$Population), length.out = 40), Forest = c(3, seq(0,100, by = 20)))

###

newx <- model.matrix( ~ Population*Forest, data = NewDF)
newx <- newx[, -1]

NewDF$Pred <- predict(Mejor_Lasso, newx = newx)

NewDF$Forest <- as.factor(NewDF$Forest)


ggplot(Data, aes(x = Population, y = r)) + geom_hex() + scale_fill_viridis_c()

ggplot(NewDF, aes(x = Population, y = Pred, group = Forest)) + 
  geom_path(aes(color = Forest)) + 
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() + 
  scale_x_continuous(label = scales::comma) + 
  ylab("growth rate r")