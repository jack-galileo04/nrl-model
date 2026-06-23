#################### helper Script to Run the targets pipeline ####################


# Ensure correct package environment
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore(prompt = FALSE)

# Load targets
library(targets)

# Run pipeline
targets::tar_make()
