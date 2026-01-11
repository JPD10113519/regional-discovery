## Hypothesis: some converged "deep south" rois are the same as "african american south" rois

deep <- readRDS("../output/AN_DeepSouth_v1/AN_DeepSouth_v1.rds")
## I ran afamsouth in the usa directory. Gonna clean this up later.

afam <- readRDS("../../Schweiz/output/CB_AN/afamsouth_zoom.rds")

deeprois <- unique(deep$final_roi)
afamrois <- unique(afam$final_roi)

common_rois <- intersect(deeprois, afamrois)

library(terra)
# Load county shapefile
map <- vect("../data/cb_2023_us_county_500k")
cus_abbrs <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", 
               "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", 
               "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", 
               "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", 
               "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
               "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
cus <- subset(map, map$STUSPS %in% cus_abbrs)
cus <- cus[order(cus$GEOID),]

# Make maps

output_dir <- "deep_vs_afam_south_rois/"

for (i in seq_along(common_rois)) {
  cat("Creating map", i, "of", length(common_rois), "\n")
  
  cus$roi <- common_rois[[i]]
  map_colors <- ifelse(cus$roi == 1, "#4682B4", "#f5f5f5")
  
  output_file <- file.path(output_dir, paste0("common_roi_", i, ".png"))
  png(output_file, width = 800, height = 520, bg = "white")
  par(mar = c(1, 1, 2, 1))
  plot(cus, col = map_colors, border = NA)  # border = NA removes borders
  title(paste("Common ROI", i, "-", sum(cus$roi), "counties"))
  dev.off()
}

## want to compare some other ones like this. 