createResultsFolder <- function(cfg){

  # Generate name of results folder and create the folder
  date <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
  cfg$results_folder <- gsub(":date:", date, cfg$results_folder, fixed = TRUE)
  cfg$results_folder <- gsub(":title:", cfg$title, cfg$results_folder, fixed = TRUE)

  # Create results folder
  if (!file.exists(cfg$results_folder)) {
    dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
    cat("   Creating results folder", cfg$results_folder, "\n")
  } else if (!cfg$force_replace) {
    stop(paste0("Results folder ", cfg$results_folder," could not be created because it already exists."))
  } else {
    cat("   Overwriting existing results folder:", cfg$results_folder, "\n")
    unlink(cfg$results_folder, recursive = TRUE)
    dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
  }

  # Save the cfg (with the updated name of the result folder) into the results folder.
  # Do not save the new name of the results folder to the .RData file in REMINDs main folder, because it
  # might be needed to restart subsequent runs manually and should not contain the time stamp in this case.
  save(cfg, file = file.path(cfg$results_folder, "config.Rdata"))

  cfg
}
