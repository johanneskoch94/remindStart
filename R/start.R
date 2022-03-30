#' start
#'
#' Start the REMIND model.
#'
#' @param remind A character vector with the path to the remind main-folder. By default, it is assumed that the working
#'   directory is the remind main-folder.
#' @param configFile NULL or a character vector with the path to a scenario config csv file.
#' @param restart TRUE or FALSE
#' @param testOneRegi TRUE or FALSE
#' @param debug TRUE or FALSE
#' @param test TRUE or FALSE
#' @param interactive TRUE or FALSE
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
start <- function(remind = ".", configFile = NULL, debug = FALSE, interactive = FALSE, restart = FALSE, test = FALSE, testOneRegi = FALSE) {
  # Take into account that this function may be called from the command-line directly, and that the command-line
  # arguments may overwrite the default/given function arguments.
  userArgs <- handleArgs(normalizePath(remind), configFile, debug, interactive, restart, test, testOneRegi)
  invisible(list2env(userArgs, environment()))
  cli::cli_alert_success("Starting REMIND found at {remind}")

  # Check global options required to start remind.
  # Since these options could be defined in the remind .Rprofile, if the working directory is not the remind directory,
  # assume that the R session was not started from the remind folder, and that the .Rprofile hasn't yet been sourced.
  cli::cli_progress_step("Checking global options. Specifically the connection to the input data folders.")
  if (getwd() != remind) quietly(source(file.path(remind, ".Rprofile"))) # nolint
  checkOptions()
  cli::cli_progress_done()

  # If desired, restart existing REMIND runs. Then stop quietly.
  if (restart) {
    restartRemind(remind, debug, test, testOneRegi)
    withr::with_options(list(show.error.messages = FALSE), stop())
  }

  # If not restarting runs, get the scenarios to be run. If no configFile exists then a single default scenario
  # is returned.
  if (!is.null(configFile)) {
    cli::cli_progress_step("Reading {configFile}", "Starting REMIND runs configured with: {configFile}")
    scenarios <- read_scenario_cfg(configFile, debug, interactive, testOneRegi)
    cli::cli_progress_done()
  } else {
    if (! testOneRegi)
      scenarios <- data.frame("testOneRegi" = "testOneRegi", row.names = "testOneRegi")
    else
      scenarios <- data.frame("default" = "default", row.names = "default")
    cli::cli_alert_success("Starting REMIND run configured with: {remind}/config/default.cfg")
  }

  # Saving Git information (do so now, so as not have to copy the .git folder in the next step)
  cli::cli_progress_step("Saving git information.")
  gitInfo <- getGitInfo(remind)

  # Create a temporary copy of remind, from which to prepare the runs
  cli::cli_progress_step("Creating temporary copy of remind from which to prepare the runs.")
  baseCopy <- createTmpBaseCopy(remind, scenarios)
  cli::cli_progress_done()

  # If possible, submit scenarios to SLURM. Otherwise run sequentially in active R-session.
  if (test) cli::cli_alert_info("If this wasn't --test mode, I would start/submit the model now.")
  else {
    if (slurmIsAvailable()) {
      cli::cli_alert_info("Submitting runs to slurm:")
      submitScenariosToSlurm(userArgs, scenarios, gitInfo, baseCopy)
    } else {
      cli::cli_alert_info("Running remind.")
      runLocally(userArgs, scenarios, gitInfo, baseCopy)
      unlink(baseCopy, recursive = TRUE)
    }
  }
}
