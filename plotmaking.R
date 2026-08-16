library(terra)
# Load county shapefile
map <- vect("data/cb_2023_us_county_500k")
cus_abbrs <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", 
               "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", 
               "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", 
               "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", 
               "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
               "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
cus <- subset(map, map$STUSPS %in% cus_abbrs)
cus <- cus[order(cus$GEOID),]


rois <- readRDS("data/polisci_rois.rds")

source("code_v2/model_functions.R")


library(colorspace)
genplot <- function(ROI) {
  iteration <- run_iteration(target_dev=98, p_cutoff=0.5, ROI=ROI, max_iterations=100)
  
  ROI <- iteration$final_roi
  cus$color <- rgb(r = (26 + 188*ROI), g = (95 - 27*ROI), b = (168-46*ROI), maxColorValue=255)
  
  plot(cus, col=cus$color, border=darken(cus$color, amount=0.25))
}

genplot(rois[["ACP_AfAmSouth"]])
genplot(rois[["ACP_GrayingAmerica"]])
genplot(rois[["ACP_LDSEnclaves"]])