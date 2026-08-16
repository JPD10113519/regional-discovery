#PREDSload.R
library(glmnet)

data <-readRDS("data/6var_usa.rds")
PREDS <- makeX(data, na.impute=TRUE)
## only NA is median income in loving county, texas -- the single smallest county in the country!
rm(data)
