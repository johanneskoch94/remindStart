#' start
#'
#' Start the REMIND model.
#'
#' @param remind A character vector with the path to the remind main-folder. By default, it is assumed that the working
#'   directory is the remind main-folder.
#' @param configFile NULL or a character vector with the path to a scenario config csv file.
#' @param restart TRUE or FALSE
#' @param testOneRegi TRUE or FALSE
#'
#' @export
#'
#' @examples \dontrun{
#' # Start default remind run
#' start()
#'
#' # Start multiple runs with a scenario config file
#' start(configFile = "config/my_config.csv")
#'
#' # Restart a run
#' start(restart = TRUE)
#'
#' # Start from the command line
#' Rscript -e "remindStart::start()" --args config/my_config.csv
#' }
start <- function(remind = ".", configFile = NULL, restart = FALSE, testOneRegi = FALSE) {
  # Take into account that this function may be called from the command-line directly, and that the command-line
  # arguments may overwrite the default/given function arguments.
  userArgs <- handleArgs(normalizePath(remind), configFile, restart, testOneRegi)
  invisible(list2env(userArgs, environment()))
  cli::cli_alert_success("Starting REMIND found at {remind}")

  # If desired, restart existing REMIND runs. Then stop.
  if (restart) {
    restartRemind(remind)
    stopQuietly()
  }

  # If not restarting runs, get the scenarios to be run. If no configFile exists then a single default scenario
  # is returned.
  if (!is.null(configFile)) {
    cli::cli_progress_step("Reading {configFile}", "Starting REMIND runs configured with: {configFile}")
    scenarios <- read_scenario_cfg(configFile)
    cli::cli_progress_done()
  } else {
    scenarios <- data.frame("default" = "default", row.names = "default")
    cli::cli_alert_success("Starting REMIND run configured with: config/default.cfg")
  }

  # Saving Git information (do so now, so as not have to copy the .git folder in the next step)
  cli::cli_progress_step("Saving git information.")
  gitInfo <- getGitInfo(remind)

  # Create a temporary copy of remind, from which to prepare the runs
  cli::cli_progress_step("Creating temporary copy of remind from which to prepare the runs.")
  baseCopy <- createTmpBaseCopy(remind, scenarios)
  cli::cli_progress_done()

  # If possible, submit scenarios to SLURM. Otherwise run sequentially in active R-session.
  if (slurmIsAvailable()) {
    cli::cli_alert_info("Submitting runs to slurm:")
    submitScenariosToSlurm(userArgs, scenarios, gitInfo, baseCopy)
  } else {
    cli::cli_alert_info("Running remind.")
    runLocally(userArgs, scenarios, gitInfo, baseCopy)
  }
}
