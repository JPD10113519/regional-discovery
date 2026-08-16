#PREDSload.R
library(glmnet)

data <-readRDS("data/refinedpreds_v1.rds")
PREDS <- makeX(data[,-1], na.impute=TRUE) ## drop FIPS codes obviously
## only NA is median income in loving county, texas -- the single smallest county in the country!
rm(data)
