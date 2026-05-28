#################### helper Script to Run the targets pipeline ####################

.rs.restartR()

targets::tar_make()

targets::tar_visnetwork(targets_only = TRUE)
