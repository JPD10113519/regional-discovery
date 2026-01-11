#PREDSload.R
library(glmnet)

data <-readRDS("data/refinedpreds_v1.rds")
PREDS <- makeX(data)
rm(data)
