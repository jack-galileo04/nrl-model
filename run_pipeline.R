#################### helper Script to Run the targets pipeline ####################


# Ensure correct package environment
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore(prompt = FALSE)

# Load targets
library(targets)

# Run pipeline
targets::tar_make()

# log file
log_file <- paste0("logs/run_", Sys.Date(), ".log")
dir.create("logs", showWarnings = FALSE)
sink(log_file)
sink(log_file, type = "message")
