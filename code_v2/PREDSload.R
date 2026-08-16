#PREDSload.R
library(glmnet)

data <-readRDS("data/swissdatatight.rds")

## missing is appenzell innerhoden. It's the smallest by population (16k) and sort of weird.
## missing data from BAMF but I can manually impute.
data$PctFarRight[16] <- 2.4 ## 2.4% far right
data$PctUnemployed[16] <- 1.00 ## about 1% unemployment

PREDS <- makeX(data %>% select(-Kanton))
rm(data)
