# Get the array task ID from SLURM -- ranging from 0 to 599 (kicking density way up)
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.numeric(args[1])
job_name <- args[2]
roi_name <- args[3]

starttime <- Sys.time() ## timing for logs
library(dplyr)
source("code_v2/PREDSload.R") ## get prediction data ## my 5 good swiss variables here
source("code_v2/model_functions.R") ## get functions

## set up our ROI ## this dosen't need any roi_name input but whatever. Can call the job anything.
fullfrench <- "11111011000000000000000000"

ROIint <- strtoi(fullfrench,base=2)
ROI <- task_id_to_binary(ROIint)

set.seed(234234)
int <- runif(1, min=0, max = 2^26-1)
ROI <- task_id_to_binary(int)
## going to test a random one


## standard devrange value
devrange <- seq(60,99.99,length.out=300) ## i'm actually interested in lower dev values because the feature count is pretty low
prange <- seq(0.001,0.999,length.out=300) ## still full p value range

## batching by vertical striping. One dev value and half of the p value range.
devind <- task_id %% 300 + 1
dev <- devrange[devind]
phalf <- trunc(task_id/300)
if (phalf == 0) {
  pvals <- prange[1:150]
} else {
  pvals <- prange[151:300]
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
  run_iteration(target_dev = dev, p_cutoff = p, ROI = ROI,max_iterations = 100)
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

