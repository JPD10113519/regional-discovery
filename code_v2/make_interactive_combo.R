## interactive combined plotting: ROI (hue) + Features (shade)
library(BFS)
library(base64enc)
library(dplyr)
library(digest)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(jsonlite)
library(grDevices)

args <- commandArgs(trailingOnly = TRUE)
job_name <- args[1]
roi_name <- args[2]

source("code_v2/model_functions.R")

results <- readRDS(paste0("output/", job_name, "/", job_name, ".rds"))

fullfrench <- "11111011000000000000000000"
ROIint <- strtoi(fullfrench, base = 2)
seed_roi <- task_id_to_binary(ROIint)

output_file <- paste0("output/", job_name, "/", job_name, "_combined.html")

# Swiss canton map
map <- bfs_get_base_maps(geom = "kant")
order <- c("Vaud", "Valais", "Genève", "Bern", "Fribourg", "Solothurn",
           "Neuchâtel", "Jura", "Basel-Stadt", "Basel-Landschaft", "Aargau",
           "Zürich", "Glarus", "Schaffhausen", "Appenzell Ausserrhoden", "Appenzell Innerrhoden",
           "St. Gallen", "Graubünden", "Thurgau", "Luzern", "Uri", "Schwyz", "Obwalden",
           "Nidwalden", "Zug", "Tessin")
map <- map[match(order, map$name), ]

# --------------------------------------------------------------------------
# Step 1: Binary strings for ROI and feature set
# --------------------------------------------------------------------------
results$roi_string     <- sapply(results$final_roi,      function(x) paste(x, collapse = ""))
results$feature_string <- sapply(results$final_features, function(x) paste(sort(x), collapse = "|"))

# Combined key = ROI + features (each unique combo gets its own color/tile group)
results$combo_string <- paste(results$roi_string, results$feature_string, sep = "###")

# --------------------------------------------------------------------------
# Step 2: Assign ROI base hues (deterministic via hash)
# --------------------------------------------------------------------------
unique_rois <- unique(results$roi_string)
n_rois <- length(unique_rois)
roi_hashes <- sapply(unique_rois, function(r) digest(r, algo = "md5"))
roi_sorted <- unique_rois[order(roi_hashes)]
roi_base_hues <- rainbow(n_rois, s = 0.75, v = 0.85)
roi_hue_map <- setNames(roi_base_hues, roi_sorted)

print(paste("Unique ROIs:", n_rois))

# --------------------------------------------------------------------------
# Step 3: Within each ROI, assign shades per unique feature set
# --------------------------------------------------------------------------
# For each ROI, enumerate its unique feature sets and vary V (brightness) + S (saturation)
combo_color_map <- list()

for (roi in unique_rois) {
  base_hex <- roi_hue_map[[roi]]
  base_hsv <- rgb2hsv(col2rgb(base_hex))
  h <- base_hsv["h", 1]
  
  # Feature sets appearing with this ROI
  feats_in_roi <- unique(results$feature_string[results$roi_string == roi])
  feat_hashes <- sapply(feats_in_roi, function(f) digest(f, algo = "md5"))
  feats_sorted <- feats_in_roi[order(feat_hashes)]
  n_feats <- length(feats_sorted)
  
  if (n_feats == 1) {
    s_vals <- 0.75
    v_vals <- 0.85
  } else {
    # Spread saturation and value to create distinguishable shades of same hue
    s_vals <- 0.75
    v_vals <- seq(0.95, 0.55, length.out = n_feats)
  }
  
  for (i in seq_along(feats_sorted)) {
    shade_hex <- hsv(h = h, s = s_vals, v = v_vals[i])
    combo_key <- paste(roi, feats_sorted[i], sep = "###")
    combo_color_map[[combo_key]] <- shade_hex
  }
}

results$color <- unlist(combo_color_map[results$combo_string])
print(paste("Unique combo colors:", length(unique(results$color))))

# --------------------------------------------------------------------------
# Step 4: Unique combos -> build map image + feature HTML for each
# --------------------------------------------------------------------------
unique_combos <- results %>%
  distinct(combo_string, .keep_all = TRUE) %>%
  select(combo_string, roi_string, feature_string, color, final_roi, final_features) %>%
  arrange(combo_string) %>%
  mutate(combo_id = paste0("combo_", row_number()))

print(paste("Number of unique ROI x feature combos:", nrow(unique_combos)))

# Swiss map generator (one per unique ROI — cache to avoid redundant rendering)
create_map_base64 <- function(roi_vector, region_color, seed_vector) {
  map_local <- map
  map_local$fill_color   <- rgb(0.96, 0.96, 0.96)
  map_local$fill_color[roi_vector == 1] <- region_color
  map_local$border_color <- NA
  map_local$border_width <- 0.2
  map_local$border_color[seed_vector == 1] <- "#808080"
  map_local$border_width[seed_vector == 1] <- 1.0
  
  p <- ggplot(data = map_local) +
    geom_sf(aes(fill = fill_color),
            color = map_local$border_color,
            linewidth = map_local$border_width) +
    scale_fill_identity() +
    theme_void()
  
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = p, width = 4, height = 3, dpi = 100, bg = "white")
  img_data <- readBin(temp_file, "raw", file.info(temp_file)$size)
  b64 <- base64encode(img_data)
  unlink(temp_file)
  b64
}

# Cache maps by roi_string (geometry only depends on ROI; use the ROI's base hue for the map fill)
print("Generating ROI maps (cached per unique ROI)...")
roi_map_cache <- list()
for (roi in unique_rois) {
  example_row <- unique_combos[unique_combos$roi_string == roi, ][1, ]
  # Use ROI base hue (not shade) for map so ROI identity is clear
  roi_map_cache[[roi]] <- create_map_base64(
    example_row$final_roi[[1]],
    roi_hue_map[[roi]],
    seed_roi
  )
}

# Feature HTML
create_feature_html <- function(feature_list) {
  n_features <- length(feature_list)
  sorted_features <- sort(feature_list)
  if (n_features <= 20) {
    feature_display <- paste(sorted_features, collapse = "<br>")
  } else {
    feature_display <- paste(
      paste(sorted_features[1:15], collapse = "<br>"),
      sprintf("<br><i>... and %d more features</i>", n_features - 15)
    )
  }
  sprintf(
    '<div style="max-height: 280px; overflow-y: auto; margin-top: 8px;">
      <b>Number of features:</b> %d<br><b>Features:</b><br>%s
    </div>',
    n_features, feature_display
  )
}

unique_combos$feature_html <- sapply(unique_combos$final_features, create_feature_html)
unique_combos$map_base64   <- unlist(roi_map_cache[unique_combos$roi_string])

# --------------------------------------------------------------------------
# Step 5: Build ggplot heatmap
# --------------------------------------------------------------------------
print("Creating plot...")
p <- ggplot(results, aes(x = dev_value, y = p_cutoff, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  labs(title = "ROI (hue) × Feature Set (shade): Interactive Parameter Space",
       x = "Deviance Value",
       y = "P-cutoff") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

interactive_p <- ggplotly(p, tooltip = "none")

# --------------------------------------------------------------------------
# Step 6: trace -> combo mapping
# ggplotly creates one trace per unique fill color, sorted by color string.
# We replicate that ordering here.
# --------------------------------------------------------------------------
trace_colors <- results %>%
  distinct(color, .keep_all = FALSE) %>%
  arrange(color) %>%
  pull(color)

# Map each color -> combo (colors are unique per combo by construction)
color_to_combo <- unique_combos %>%
  select(color, combo_id) %>%
  distinct()

trace_combo_ids <- color_to_combo$combo_id[match(trace_colors, color_to_combo$color)]

trace_content_library <- setNames(
  mapply(function(cid) {
    row <- unique_combos[unique_combos$combo_id == cid, ]
    paste0(
      '<div style="font-family: Arial, sans-serif; font-size: 13px;">',
      '<b>Converged Region:</b><br>',
      '<img src="data:image/png;base64,', row$map_base64,
      '" width="380" style="display:block;"><br>',
      '<i style="color:#666;">Grey borders show seed canton</i>',
      row$feature_html,
      '</div>'
    )
  }, trace_combo_ids, USE.NAMES = FALSE),
  0:(length(trace_combo_ids) - 1)
)

# --------------------------------------------------------------------------
# Step 7: JS tooltip
# --------------------------------------------------------------------------
interactive_p <- onRender(interactive_p, sprintf("
  function(el, x) {
    var library = %s;
    console.log('Combined library loaded, size:', Object.keys(library).length);
    
    var tooltip = document.createElement('div');
    tooltip.id = 'combined-tooltip';
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
      maxWidth: '430px'
    });
    document.body.appendChild(tooltip);
    
    el.on('plotly_hover', function(data) {
      var traceNum = data.points[0].curveNumber;
      if (library[traceNum]) {
        tooltip.innerHTML = library[traceNum];
        var x = data.event.clientX, y = data.event.clientY;
        var tw = 430, th = 500;
        var left = (x + tw + 20 > window.innerWidth) ? x - tw - 20 : x + 20;
        var top  = (y + th + 20 > window.innerHeight) ? y - th - 20 : y + 20;
        if (top < 0) top = 10;
        tooltip.style.left = left + 'px';
        tooltip.style.top  = top + 'px';
        tooltip.style.display = 'block';
      }
    });
    el.on('plotly_unhover', function() { tooltip.style.display = 'none'; });
  }
", toJSON(trace_content_library, auto_unbox = TRUE)))

# --------------------------------------------------------------------------
# Step 8: Save
# --------------------------------------------------------------------------
print(paste("Saving to", output_file))
saveWidget(interactive_p,
           file = output_file,
           selfcontained = TRUE,
           title = "Combined ROI × Feature Parameter Space Explorer")
print("Done!")