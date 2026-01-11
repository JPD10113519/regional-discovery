## color_block_functions.R
## this builds on phase_diagram.R
library(ggplot2)
library(digest)

## run_iteration takes a starting ROI and hyperparameters
## and runs it until convergence 
run_iteration <- function(target_dev, p_cutoff, ROI, max_iterations=20) {
  iterations <- 1
  length_roi <- length(ROI)
  current_roi <- ROI
  prev_roi <- NULL
  
  while(iterations < max_iterations) {
    # Early termination checks
    roi_sum <- sum(current_roi)
    if (roi_sum %in% c(0, 1, length_roi - 1, length_roi)) break ## terminate if we'll get glmnet errors
    if (!is.null(prev_roi) && identical(prev_roi, current_roi)) break ## or if we get convergence
    
    # Fit model
    dev_model <- glmnet(x = PREDS, y = current_roi, family = "binomial", 
                        alpha = 1, standardize = FALSE)  # Consider if you need standardization
    
    # Find lambda (vectorized)
    lambda_id <- which.min(abs(dev_model$dev.ratio * 100 - target_dev))
    
    # Predict (could reuse dev_model object without refitting)
    dev_predictions <- predict(dev_model, newx = PREDS, 
                               s = dev_model$lambda[lambda_id], 
                               type = "response")
    
    prev_roi <- current_roi
    current_roi <- as.integer(dev_predictions > p_cutoff)
    iterations <- iterations + 1
  }
  
  coeffs <- coef(dev_model,s=dev_model$lambda[lambda_id])
  vars <- rownames(coeffs)[which(coeffs != 0)]
  vars <- vars[vars != "(Intercept)"]
  
  list(iterations = iterations, 
       init_roi = ROI, 
       final_features = vars,
       final_roi = current_roi)
}

## this uses a hash function to convert ROIs to colors in a way that's deterministic,
## but also gives color variety to similar sets and doesn't require a 70-million option color map
roi_to_color <- function(roi, saturation = 0.7, value = 0.85) {
  # Hash the roi
  hashval <- digest(roi, algo = "sha1")
  
  # Convert first 6 hex characters to integer (24 bits, safe range)
  hue_int <- strtoi(substr(hashval, 1, 6), base = 16L)
  
  # Map to [0, 1] range
  hue <- (hue_int %% 360) / 360
  
  color <- hsv(hue, saturation, value)
  return(color)
}

## this is a modified version of make_phase_diagram()
## we get final ROIs for a given starting ROI and a mesh of hyperparameter values
## instead of giving it a gradient by the size of the final ROI, it gives each ROI a unique color.
make_cb_diagram <- function(ROI, devrange=c(25,99), prange=c(0.01,0.99), res=50, filename=NA) {
  ## make our mesh
  deviance_vals <- seq(devrange[1], devrange[2], by = (devrange[2]-devrange[1])/res)
  p_cutoffs <- seq(prange[1], prange[2], by = (prange[2]-prange[1])/res)
  results <- expand.grid(deviance = deviance_vals, p_cutoff = p_cutoffs)
  ## also get some results stuff
  results$final_roi <- NA
  results$init_roi <- NA
  results$color <- NA
  results$sameflag <- NA
  ## populate our mesh
  for (i in 1:nrow(results)) {
    # Run your procedure with these hyperparameters
    outcome <- run_iteration(target_dev = results$deviance[i],
                             p_cutoff = results$p_cutoff[i],
                             ROI = ROI,
                             max_iterations=6)
    results$init_roi[i] <-paste(outcome$init_roi, collapse="")
    results$final_roi[i] <- paste(outcome$final_roi,collapse="")
  }

  if(!is.na(filename)) {
     plot_cb_diagram(results, init_roi = results$init_roi[1],filename)
  }
  ## this returns an object only if we don't give a filename, which is the default
  ## to save results AND plot, use plot_cb_diagram on the resulting object
  else{return(results)} 
}


plot_cb_diagram <- function(results, init_roi, filename) {
  ## black out our trivial fixed points
  results$sameflag <- sapply(results$final_roi, function(roi) {
    identical(roi, init_roi)})
  
  
  for(i in 1:length(results$final_roi)) {
    if(results$sameflag[i]) {results$color[i] <- "#000000"}
    else {results$color[i] <- roi_to_color(results$final_roi[[i]])}
  }
  
  png(filename = filename)
  print(ggplot(results, aes(x = dev_value, y = p_cutoff, fill = color)) +
          scale_fill_identity() +
          geom_tile() +
          labs(title = paste("Color Block Diagram of ROI Dynamics:\n",filename))) +
    theme(legend.position = "none")
  dev.off()
}


