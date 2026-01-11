## interactive plotting
library(terra)
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
# FUNCTION: Process results and create interactive plot
# ==============================================================================
results <- readRDS(paste0("output/", job_name, "/", job_name, ".rds"))

rois <- readRDS("data/polisci_rois.rds")
seed_roi <- rois[[roi_name]]

output_file <- paste0("output/", job_name, "/", job_name, "_rois.html")

# --------------------------------------------------------------------------
# Step 1: Create binary string representation for hashing/comparison
# --------------------------------------------------------------------------
results$binary_string <- sapply(results$final_roi, function(x) paste(x, collapse = ""))

# --------------------------------------------------------------------------
# Step 2: Create color from hash (RANDOMIZED per ROI, but deterministic)
# --------------------------------------------------------------------------
# Get unique ROIs
unique_rois <- unique(results$binary_string)
n_unique <- length(unique_rois)

# Create a color palette
color_palette <- rainbow(n_unique, s = 0.7, v = 0.85)

# Create a deterministic but unique mapping
# Hash each ROI, sort by hash, then assign colors sequentially
roi_hashes <- sapply(unique_rois, function(roi) {
  digest(roi, algo = "md5")
})

# Sort ROIs by their hash values (this gives deterministic randomization)
roi_order <- order(roi_hashes)
sorted_rois <- unique_rois[roi_order]

# Assign colors in this sorted order
roi_color_map <- setNames(color_palette, sorted_rois)

# Apply to results
results$color <- roi_color_map[results$binary_string]

print(paste("Unique ROIs:", n_unique))
print(paste("Unique colors:", length(unique(results$color))))
print("Color assignment is unique and deterministic!")

# --------------------------------------------------------------------------
# Step 3: Get unique regions (BASED ON BINARY_STRING, NOT COLOR!)
# --------------------------------------------------------------------------
unique_regions <- results %>%
  distinct(binary_string, .keep_all = TRUE) %>%  # Keep first occurrence of each unique ROI
  select(binary_string, color, final_roi) %>%
  arrange(binary_string) %>%  # Sort by ROI, not color
  mutate(map_id = paste0("map_", row_number()))

print(paste("Number of unique regions:", nrow(unique_regions)))

# --------------------------------------------------------------------------
# Step 4: Load county shapefile
# --------------------------------------------------------------------------
map <- vect("data/cb_2023_us_county_500k")
cus_abbrs <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", 
               "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", 
               "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", 
               "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", 
               "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
               "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
cus <- subset(map, map$STUSPS %in% cus_abbrs)
cus <- cus[order(cus$GEOID),]

# --------------------------------------------------------------------------
# Step 5: Function to create base64 map with seed border
# --------------------------------------------------------------------------
create_map_base64 <- function(roi_vector, region_color) {
  cus$final_roi <- roi_vector
  cus$seed_roi <- seed_roi
  
  mapcols <- ifelse(cus$final_roi == 1, region_color, "#f5f5f5")
  border_cols <- ifelse(cus$seed_roi == 1, "#808080", NA)
  border_width <- ifelse(cus$seed_roi == 1, 0.5, 0.1)
  
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 400, height = 260, bg = "white")
  par(mar = c(0, 0, 0, 0))
  plot(cus, col = mapcols, border = border_cols, lwd = border_width)
  dev.off()
  
  img_data <- readBin(temp_file, "raw", file.info(temp_file)$size)
  map_base64 <- base64encode(img_data)
  unlink(temp_file)
  
  return(map_base64)
}

# --------------------------------------------------------------------------
# Step 6: Generate all unique maps
# --------------------------------------------------------------------------
print("Generating maps...")
unique_regions$map_base64 <- mapply(
  create_map_base64,
  unique_regions$final_roi,
  unique_regions$color,
  SIMPLIFY = TRUE,
  USE.NAMES = FALSE
)

# --------------------------------------------------------------------------
# Step 7: Join map_id back to results (based on binary_string!)
# --------------------------------------------------------------------------
results <- results %>%
  left_join(select(unique_regions, binary_string, map_id), 
            by = "binary_string")

# DEBUG: Check the join worked
print("Sample of results with map_id:")
print(head(results[, c("dev_value", "p_cutoff", "map_id", "binary_string")]))
print(paste("Any NAs in map_id?", any(is.na(results$map_id))))

# --------------------------------------------------------------------------
# Step 8: Create map library for JavaScript
# --------------------------------------------------------------------------
map_library <- setNames(
  paste0('<img src="data:image/png;base64,', 
         unique_regions$map_base64, 
         '" width="400" style="display:block;"><br>',
         '<i style="color:#666;">Black borders show seed region</i>'),
  unique_regions$map_id
)

print(paste("Map library has", length(map_library), "entries"))
print(paste("First map_id:", names(map_library)[1]))

# --------------------------------------------------------------------------
# Step 9: Add customdata to results BEFORE plotting
# --------------------------------------------------------------------------
results$customdata_str <- paste(
  results$map_id,
  results$dev_value, 
  results$p_cutoff,
  results$iterations,
  sep = "|"
)

# --------------------------------------------------------------------------
# Step 10: Create ggplot with customdata in aes
# --------------------------------------------------------------------------
print("Creating plot...")
p <- ggplot(results, aes(x = dev_value, y = p_cutoff, fill = color, 
                         customdata = customdata_str)) +
  geom_tile() +
  scale_fill_identity() +
  labs(title = "ROI Dynamics: Interactive Parameter Space",
       x = "Deviance Value",
       y = "P-cutoff") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

# --------------------------------------------------------------------------
# Step 11: Convert to plotly - customdata should be preserved
# --------------------------------------------------------------------------
interactive_p <- ggplotly(p, tooltip = "none")

# Check if customdata made it through
print("Checking if customdata was preserved:")
if(!is.null(interactive_p$x$data[[1]]$customdata)) {
  print("Success! First customdata:")
  print(head(interactive_p$x$data[[1]]$customdata))
} else {
  print("Customdata not preserved through ggplotly")
}

# Parse the customdata in JavaScript instead of trying to set it in R
interactive_p <- onRender(interactive_p, sprintf("
  function(el, x) {
    console.log('onRender function called');
    
    var mapLibrary = %s;
    console.log('Number of maps in library:', Object.keys(mapLibrary).length);
    
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
    
    el.on('plotly_hover', function(data) {
      console.log('Hover event triggered');
      var point = data.points[0];
      
      // Check different possible locations for customdata
      var customStr = point.customdata || point.text || null;
      console.log('Custom string:', customStr);
      
      if (customStr && typeof customStr === 'string' && customStr.includes('|')) {
        var parts = customStr.split('|');
        var mapId = parts[0];
        var devValue = parseFloat(parts[1]);
        var pCutoff = parseFloat(parts[2]);
        var steps = parseInt(parts[3]);
        
        console.log('Parsed - Map ID:', mapId);
        
        if (mapId && mapLibrary[mapId]) {
          var content = '<div style=\"font-family: Arial, sans-serif;\">';
          content += '<div style=\"margin-bottom: 10px;\">';
          content += '<b>Parameters:</b><br>';
          content += 'Deviance: ' + devValue.toFixed(2) + '<br>';
          content += 'P-cutoff: ' + pCutoff.toFixed(3) + '<br>';
          content += 'Steps to convergence: ' + steps + '<br>';
          content += '</div>';
          content += '<div style=\"margin-top: 10px;\">';
          content += '<b>Converged Region:</b><br>';
          content += mapLibrary[mapId];
          content += '</div>';
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
        }
      }
    });
    
    el.on('plotly_unhover', function() {
      tooltip.style.display = 'none';
    });
    
    el.on('plotly_relayout', function() {
      tooltip.style.display = 'none';
    });
  }
", toJSON(map_library, auto_unbox = TRUE)))

print("=== PLOTLY STRUCTURE DEBUG ===")
print(paste("Total traces:", length(interactive_p$x$data)))

for(i in 1:min(3, length(interactive_p$x$data))) {
  trace <- interactive_p$x$data[[i]]
  print(paste("\n--- Trace", i, "---"))
  print(paste("Type:", trace$type))
  print(paste("Has x:", !is.null(trace$x)))
  print(paste("Has y:", !is.null(trace$y)))
  print(paste("Has customdata:", !is.null(trace$customdata)))
  
  if(!is.null(trace$x)) {
    print(paste("Number of points:", length(trace$x)))
  }
  
  if(!is.null(trace$customdata)) {
    print(paste("Customdata length:", length(trace$customdata)))
    print("First customdata entry:")
    print(trace$customdata[1])
    print("Customdata class:")
    print(class(trace$customdata))
  }
}

# --------------------------------------------------------------------------
# Step 11: Simple version - just show the map for the hovered trace
# --------------------------------------------------------------------------

# Create a map library indexed by trace/color instead of individual points
# We need to map from color -> map_id

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

print("Done!")
print(paste("File size reduced by storing only", nrow(unique_regions), "unique maps instead of", nrow(results)))