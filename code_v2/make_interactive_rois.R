## interactive plotting
library(BFS)
library(base64enc)
library(dplyr)
library(digest)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(jsonlite)

## Pull job name from command line
args <- commandArgs(trailingOnly = TRUE)
job_name <- args[1]
roi_name <- args[2]

# ==============================================================================
# Load data
# ==============================================================================
source("code_v2/model_functions.R")

results <- readRDS(paste0("output/", job_name, "/", job_name, ".rds"))

fullfrench <- "10100011000000000000000000"
fullfrench <- "11111011000000000000000000"
#fullfrench <- "00000100111111111011111110"
#germancantons <- "11101011000000000000000001"
ROIint <- strtoi(fullfrench
                 ,base=2)
seed_roi <- task_id_to_binary(ROIint)


output_file <- paste0("output/", job_name, "/", job_name, "_rois.html")

# Load Swiss canton map once
map <- bfs_get_base_maps(geom = "kant")
## needs to be reordered, annoyingly:
order <- c("Vaud", "Valais", "Genève", "Bern", "Fribourg", "Solothurn", 
           "Neuchâtel", "Jura", "Basel-Stadt", "Basel-Landschaft", "Aargau", 
           "Zürich", "Glarus", "Schaffhausen", "Appenzell Ausserrhoden", "Appenzell Innerrhoden", 
           "St. Gallen", "Graubünden", "Thurgau", "Luzern", "Uri", "Schwyz", "Obwalden", 
           "Nidwalden", "Zug", "Tessin")
map <- map[match(order, map$name),]

# --------------------------------------------------------------------------
# Step 1: Create binary string representation for hashing/comparison
# --------------------------------------------------------------------------
results$binary_string <- sapply(results$final_roi, function(x) paste(x, collapse = ""))

# --------------------------------------------------------------------------
# Step 2: Assign deterministic colors per unique ROI
# --------------------------------------------------------------------------
unique_rois <- unique(results$binary_string)
n_unique    <- length(unique_rois)

color_palette <- rainbow(n_unique, s = 0.7, v = 0.85)

roi_hashes <- sapply(unique_rois, function(roi) digest(roi, algo = "md5"))
roi_order  <- order(roi_hashes)
sorted_rois <- unique_rois[roi_order]
roi_color_map <- setNames(color_palette, sorted_rois)

results$color <- roi_color_map[results$binary_string]

print(paste("Unique ROIs:", n_unique))

# --------------------------------------------------------------------------
# Step 3: Get unique regions
# --------------------------------------------------------------------------
unique_regions <- results %>%
  distinct(binary_string, .keep_all = TRUE) %>%
  select(binary_string, color, final_roi) %>%
  arrange(binary_string) %>%
  mutate(map_id = paste0("map_", row_number()))

print(paste("Number of unique regions:", nrow(unique_regions)))

# --------------------------------------------------------------------------
# Step 4: Function to create base64 Swiss canton map
# --------------------------------------------------------------------------
create_map_base64 <- function(roi_vector, region_color, seed_vector) {
  map_local <- map  # avoid modifying the global map object
  
  # Fill: highlight final ROI in region color, seed ROI in grey, rest white
  map_local$fill_color <- rgb(0.96, 0.96, 0.96)          # default light grey
  map_local$fill_color[roi_vector == 1]  <- region_color  # final ROI
  
  # Seed border: bold outline for seed cantons
  map_local$border_color <- NA
  map_local$border_width <- 0.2
  map_local$border_color[seed_vector == 1] <- "#808080"
  map_local$border_width[seed_vector == 1] <- 1.0
  
  p <- ggplot(data = map_local) +
    geom_sf(aes(fill = fill_color),
            color    = map_local$border_color,
            linewidth = map_local$border_width) +
    scale_fill_identity() +
    theme_void()
  
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = p, width = 4, height = 3, dpi = 100, bg = "white")
  
  img_data   <- readBin(temp_file, "raw", file.info(temp_file)$size)
  map_base64 <- base64encode(img_data)
  unlink(temp_file)
  
  return(map_base64)
}


# --------------------------------------------------------------------------
# Step 5: Generate all unique maps
# --------------------------------------------------------------------------
print("Generating maps...")
unique_regions$map_base64 <- mapply(
  create_map_base64,
  unique_regions$final_roi,
  unique_regions$color,
  MoreArgs = list(seed_vector = seed_roi),
  SIMPLIFY  = TRUE,
  USE.NAMES = FALSE
)

## tester function
#render_base64 <- function(b64) htmltools::browsable(htmltools::HTML(paste0('<img src="data:image/png;base64,', b64, '">')))
#render_base64(unique_regions$map_base64[7])

# --------------------------------------------------------------------------
# Step 6: Join map_id back to results
# --------------------------------------------------------------------------
results <- results %>%
  left_join(select(unique_regions, binary_string, map_id), by = "binary_string")

print(paste("Any NAs in map_id?", any(is.na(results$map_id))))

# --------------------------------------------------------------------------
# Step 7: Create map library for JavaScript
# --------------------------------------------------------------------------
map_library <- setNames(
  paste0('<img src="data:image/png;base64,',
         unique_regions$map_base64,
         '" width="400" style="display:block;"><br>',
         '<i style="color:#666;">Grey borders show seed canton</i>'),
  unique_regions$map_id
)

# --------------------------------------------------------------------------
# Step 8: Add customdata to results
# --------------------------------------------------------------------------
results$customdata_str <- paste(
  results$map_id,
  results$dev_value,
  results$p_cutoff,
  results$iterations,
  sep = "|"
)

# --------------------------------------------------------------------------
# Step 9: Build ggplot heatmap
# --------------------------------------------------------------------------
print("Creating plot...")
# Step 10: Build ggplot
# Step 9: Build ggplot heatmap
p <- ggplot(results, aes(x = dev_value, y = p_cutoff, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  labs(title = "ROI Dynamics: Interactive Parameter Space",
       x     = "Expressiveness",
       y     = "Selectivity") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

# Step 10: Convert to plotly (ONCE)
interactive_p <- ggplotly(p, tooltip = "none")

trace_to_map <- results %>%
  distinct(color, .keep_all = TRUE) %>%
  arrange(color) %>%  # This order should match how ggplotly creates traces
  pull(map_id)

# Create map library indexed by trace number (0-indexed)
trace_map_library <- setNames(
  paste0('<img src="data:image/png;base64,', 
         unique_regions$map_base64[match(trace_to_map, unique_regions$map_id)], 
         '" width="400" style="display:block;"><br>',
         '<i style="color:#666;">Black borders show seed region</i>'),
  0:(length(trace_to_map) - 1)
)

interactive_p <- onRender(interactive_p, sprintf("
  function(el, x) {
    console.log('JavaScript started');
    
    var traceMapLibrary = %s;
    console.log('Trace map library loaded, size:', Object.keys(traceMapLibrary).length);
    
    var tooltip = document.createElement('div');
    tooltip.id = 'county-map-tooltip';
    Object.assign(tooltip.style, {
      position: 'fixed',
      backgroundColor: 'white',
      border: '2px solid #333',
      borderRadius: '8px',
      padding: '15px',
      zIndex: '10000',
      display: 'none',
      pointerEvents: 'none',
      boxShadow: '0 6px 12px rgba(0,0,0,0.15)',
      maxWidth: '450px'
    });
    document.body.appendChild(tooltip);
    console.log('Tooltip created');
    
    el.on('plotly_hover', function(data) {
      var point = data.points[0];
      var traceNum = point.curveNumber;
      
      console.log('Hovered trace:', traceNum);
      console.log('Map exists for trace:', traceNum in traceMapLibrary);
      
      if (traceMapLibrary[traceNum]) {
        var content = '<div style=\"font-family: Arial, sans-serif;\">';
        content += '<b>Converged Region:</b><br>';
        content += traceMapLibrary[traceNum];
        content += '</div>';
        
        var x = data.event.clientX;
        var y = data.event.clientY;
        var tooltipWidth = 450;
        var tooltipHeight = 350;
        
        var left = (x + tooltipWidth + 20 > window.innerWidth) ? 
                   x - tooltipWidth - 20 : x + 20;
        var top = (y + tooltipHeight + 20 > window.innerHeight) ? 
                  y - tooltipHeight - 20 : y + 20;
        
        tooltip.style.left = left + 'px';
        tooltip.style.top = top + 'px';
        tooltip.innerHTML = content;
        tooltip.style.display = 'block';
        console.log('Tooltip displayed for trace', traceNum);
      } else {
        console.log('No map found for trace', traceNum);
      }
    });
    
    el.on('plotly_unhover', function() {
      tooltip.style.display = 'none';
    });
  }
", toJSON(trace_map_library, auto_unbox = TRUE)))
# --------------------------------------------------------------------------
# Step 12: Save as HTML
# --------------------------------------------------------------------------
print(paste("Saving to", output_file))
saveWidget(interactive_p, 
           file = output_file,
           selfcontained = TRUE,
           title = "ROI Parameter Space Explorer")