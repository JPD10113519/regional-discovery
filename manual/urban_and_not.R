## Hypothesis: tidewater is finding "us urbanism" and evangelical hubs are finding "not urbanism"
## perfect complements
## there aren't any :/
## Maybe ruralmiddleamerica has some
## again no. This sucks.

urban <- readRDS("../output/AN_Tidewater_v1/AN_Tidewater_v1.rds")
#rural <- readRDS("../output/ACP_EvangelicalHubs_v1/ACP_EvangelicalHubs_v1.rds")
rural <- readRDS("../output/ACP_RuralMiddleAmerica_v1/ACP_RuralMiddleAmerica_v1.rds")

urbanrois <- unique(urban$final_roi)
ruralrois <- unique(rural$final_roi)

ruralcomps <- lapply(ruralrois, function(x) 1 - x)

common_rois <- intersect(urbanrois, ruralcomps)


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

output_dir <- "urban_and_not_rois/"

plotint <- function(common_rois, output_dir, name) {
for (i in seq_along(common_rois)) {
  cat("Creating map", i, "of", length(common_rois), "\n")
  
  cus$roi <- common_rois[[i]]
  map_colors <- ifelse(cus$roi == 1, "#4682B4", "#f5f5f5")
  
  output_file <- file.path(output_dir, paste0(name, i, ".png"))
  png(output_file, width = 800, height = 520, bg = "white")
  par(mar = c(1, 1, 2, 1))
  plot(cus, col = map_colors, border = NA)  # border = NA removes borders
  title(paste(name, "Common ROI", i, "-", sum(cus$roi), "counties"))
  dev.off()
}
}
## want to compare some other ones like this. 

## Let's do more urban things. 

urban2 <- readRDS("../output/ACP_BigCities_v1/ACP_BigCities_v1.rds")
urban3 <- readRDS("../output/ACP_Exurbs_v1/ACP_Exurbs_v1.rds")

urb2rois <- unique(urban2$final_roi)
urb3rois <- unique(urban3$final_roi)

common_12 <- intersect(urbanrois, urb2rois)
common_13 <- intersect(urbanrois, urb3rois)
common_23 <- intersect(urb2rois, urb3rois)

plotint(common_12, output_dir = "urbanrois", name="int_12_")
plotint(common_13, output_dir = "urbanrois", name="int_13_") ## this one is huge!
## there are many rois, several of which are significant, that are reached from both tidewater and ACP_Exurbs!
plotint(common_23, output_dir = "urbanrois", name="int_23_")
