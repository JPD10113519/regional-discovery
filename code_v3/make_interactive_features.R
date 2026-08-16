## interactive plotting for features
library(dplyr)
library(digest)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
job_name <- args[1]

# ==============================================================================
# FUNCTION: Process results and create interactive plot for FEATURES
# ==============================================================================
results <- readRDS(paste0("output/", job_name, "/", job_name, ".rds"))

output_file <- paste0("output/", job_name, "/", job_name, "_features.html")

# --------------------------------------------------------------------------
# Step 1: Create binary string representation for hashing/comparison
# ORDER DOESN'T MATTER - sort each feature list first
# --------------------------------------------------------------------------
results$binary_string <- sapply(results$final_features, function(x) {
  paste(sort(x), collapse = "")
})

# --------------------------------------------------------------------------
# Step 2: Create color from hash (RANDOMIZED per feature set, but deterministic)
# --------------------------------------------------------------------------
unique_features <- unique(results$binary_string)
n_unique <- length(unique_features)

color_palette <- rainbow(n_unique, s = 0.7, v = 0.85)

# Hash and sort for deterministic randomization
feature_hashes <- sapply(unique_features, function(feat) {
  digest(feat, algo = "md5")
})

feature_order <- order(feature_hashes)
sorted_features <- unique_features[feature_order]
feature_color_map <- setNames(color_palette, sorted_features)

results$color <- feature_color_map[results$binary_string]

print(paste("Unique feature sets:", n_unique))
print(paste("Unique colors:", length(unique(results$color))))

# --------------------------------------------------------------------------
# Step 3: Get unique feature sets
# --------------------------------------------------------------------------
unique_feature_sets <- results %>%
  distinct(binary_string, .keep_all = TRUE) %>%
  select(binary_string, color, final_features) %>%
  arrange(binary_string) %>%
  mutate(trace_id = paste0("trace_", row_number()))

print(paste("Number of unique feature sets:", nrow(unique_feature_sets)))

# --------------------------------------------------------------------------
# Step 4: Create feature display HTML for each unique set
# --------------------------------------------------------------------------
create_feature_html <- function(feature_list) {
  n_features <- length(feature_list)
  sorted_features <- sort(feature_list)
  
  # If 20 or fewer, show all
  if (n_features <= 20) {
    feature_display <- paste(sorted_features, collapse = "<br>")
  } else {
    # Show first 15 and indicate there are more
    feature_display <- paste(
      paste(sorted_features[1:15], collapse = "<br>"),
      sprintf("<br><i>... and %d more features</i>", n_features - 15)
    )
  }
  
  html <- sprintf(
    '<div style="max-height: 300px; overflow-y: auto;">
      <b>Number of features:</b> %d<br><br>
      <b>Features:</b><br>
      %s
    </div>',
    n_features,
    feature_display
  )
  
  return(html)
}

unique_feature_sets$feature_html <- sapply(
  unique_feature_sets$final_features,
  create_feature_html
)

# --------------------------------------------------------------------------
# Step 5: Join trace_id back to results
# --------------------------------------------------------------------------
results <- results %>%
  left_join(select(unique_feature_sets, binary_string, trace_id), 
            by = "binary_string")

print(paste("Any NAs in trace_id?", any(is.na(results$trace_id))))

# --------------------------------------------------------------------------
# Step 6: Create ggplot
# --------------------------------------------------------------------------
print("Creating plot...")
p <- ggplot(results, aes(x = dev_value, y = p_cutoff, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  labs(title = "Feature Sets: Interactive Parameter Space",
       x = "Deviance Value",
       y = "P-cutoff") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

# --------------------------------------------------------------------------
# Step 7: Convert to plotly
# --------------------------------------------------------------------------
interactive_p <- ggplotly(p, tooltip = "none")

# --------------------------------------------------------------------------
# Step 8: Create trace-to-feature mapping
# --------------------------------------------------------------------------
trace_to_features <- results %>%
  distinct(color, .keep_all = TRUE) %>%
  arrange(color) %>%
  pull(trace_id)

trace_feature_library <- setNames(
  unique_feature_sets$feature_html[match(trace_to_features, unique_feature_sets$trace_id)],
  0:(length(trace_to_features) - 1)
)

# --------------------------------------------------------------------------
# Step 9: Add JavaScript for interactive tooltips
# --------------------------------------------------------------------------
interactive_p <- onRender(interactive_p, sprintf("
  function(el, x) {
    console.log('JavaScript started');
    
    var traceFeatureLibrary = %s;
    console.log('Trace feature library loaded, size:', Object.keys(traceFeatureLibrary).length);
    
    var tooltip = document.createElement('div');
    tooltip.id = 'feature-tooltip';
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
      maxWidth: '400px',
      fontFamily: 'Arial, sans-serif',
      fontSize: '13px'
    });
    document.body.appendChild(tooltip);
    console.log('Tooltip created');
    
    el.on('plotly_hover', function(data) {
      var point = data.points[0];
      var traceNum = point.curveNumber;
      
      console.log('Hovered trace:', traceNum);
      
      if (traceFeatureLibrary[traceNum]) {
        tooltip.innerHTML = traceFeatureLibrary[traceNum];
        
        var x = data.event.clientX;
        var y = data.event.clientY;
        var tooltipWidth = 400;
        var tooltipHeight = 400;
        
        var left = (x + tooltipWidth + 20 > window.innerWidth) ? 
                   x - tooltipWidth - 20 : x + 20;
        var top = (y + tooltipHeight + 20 > window.innerHeight) ? 
                  y - tooltipHeight - 20 : y + 20;
        
        tooltip.style.left = left + 'px';
        tooltip.style.top = top + 'px';
        tooltip.style.display = 'block';
        console.log('Tooltip displayed for trace', traceNum);
      }
    });
    
    el.on('plotly_unhover', function() {
      tooltip.style.display = 'none';
    });
  }
", toJSON(trace_feature_library, auto_unbox = TRUE)))

# --------------------------------------------------------------------------
# Step 10: Save as HTML
# --------------------------------------------------------------------------
print(paste("Saving to", output_file))
saveWidget(interactive_p, 
           file = output_file,
           selfcontained = TRUE,
           title = "Feature Set Parameter Space Explorer")

print("Done!")
print(paste("Storing only", nrow(unique_feature_sets), "unique feature sets instead of", nrow(results)))