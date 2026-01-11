# Get the array task ID from SLURM -- ranging from 0 to 199
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.numeric(args[1])
job_name <- args[2]
roi_name <- args[3]

starttime <- Sys.time() ## timing for logs

source("code/PREDSload.R") ## get prediction data ## this test is with refined
source("code/model_functions.R") ## get functions

## set up our ROI
rois <- readRDS("data/polisci_rois.rds")
ROI <- rois[[roi_name]]

## standard devrange values
devrange <- seq(85,99.9,length.out=100)
prange <- seq(0.01,0.99,length.out=100)

## batching by vertical striping. One dev value and half of the p value range.
devind <- task_id %% 100 + 1
dev <- devrange[devind]
phalf <- trunc(task_id/100)
if (phalf == 0) {
  pvals <- prange[1:50]
} else {
  pvals <- prange[51:100]
}

## premake results df
## pack it with as much useful stuff as possible.
results_save <- data.frame(
  dev_value = rep(dev, length(pvals)),
  p_cutoff = pvals,
  iterations = integer(length(pvals)),
  final_roi = I(vector("list", length(pvals))),  ## I() keeps as a list-column
  final_features = I(vector("list", length(pvals)))
)

## run it!
library(parallel)
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

results <- mclapply(pvals, function(p) {
  run_iteration(target_dev = dev, p_cutoff = p, ROI = ROI,max_iterations = 200)
}, mc.cores = n_cores)

## now populate and save
results_save$iterations <- sapply(results, function(x) x$iterations)
results_save$final_roi <- lapply(results, function(x) x$final_roi)
results_save$final_features <- lapply(results, function(x) x$final_features)


## file name should come out like
## "output/ACP_UrbanBurbs_v1/tmp/ACP_UrbanBurbs_v1_194.rds"
saveRDS(results_save, paste0("output/",job_name,"/tmp/",job_name, "_", sprintf("%03d", task_id), ".rds"))

endtime <- Sys.time()
elapsed <- endtime - starttime
cat("Time elapsed:", elapsed)

