# If restarting runs, then choose the folders and start runs
restartRemind <- function(remind) {
  # Get path to output folder
  of <- file.path(remind, "output")

  # Choose results folder from list
  outputdirs <- chooseFolder(of, "Please choose the runs to be restarted")

  for (i in outputdirs) {
    cat("\nRestarting", i, "\n")

    # Read config.Rdata from results folder
    load(file.path(i, "config.Rdata"))

    # Overwrite results_folder in cfg with name of the folder the user wants to restart,
    # because user might have renamed the folder before restarting
    cfg$results_folder <- i # nolint

    if (slurmIsAvailable()) {
      # Update the slurmConfig setting to what the user chooses
      cfg$slurmConfig <- combineSlurmConfig(cfg$slurmConfig, chooseSlurmConfig())
      submitRemindRun(cfg)
    } else {
      withr::with_dir(cfg$results_folder, run())
    }
  }
}
