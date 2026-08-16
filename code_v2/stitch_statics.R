library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
job_name <- args[1]

batch_files <- list.files(paste0("output/", job_name, "/tmp/"),
                          pattern = "\\.rds$",
                          full.names = TRUE)

results <- do.call(rbind, lapply(batch_files, readRDS))
saveRDS(results, paste0("output/", job_name, "/", job_name, ".rds"))

# static plot with convergence times
png(filename = paste0("output/", job_name, "/", job_name, "_convtimes.png"))
print(ggplot(results, aes(x = dev_value, y = p_cutoff, fill = iterations)) +
        scale_fill_gradient(low = "blue", high = "red") +
        geom_tile() +
        labs(title = "Convergence Times"))
dev.off()

## same plot for number of features
png(filename = paste0("output/", job_name, "/", job_name, "_featurecount.png"))
print(ggplot(results, aes(x = dev_value, y = p_cutoff, fill = lengths(final_features))) +
        scale_fill_gradient(low = "blue", high = "yellow") +
        geom_tile() +
        labs(title = "Convergence Times"))
dev.off()