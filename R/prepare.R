# Creation of the full.gms file. Working directory has to be the result folder.
prepare <- function(resultsDir = ".") {
  # Start timer
  timePrepareStart <- Sys.time()

  # Load and check configuration for consistency
  cfg <- loadAndCheckCfg(cfgFile = file.path(resultsDir, "config.Rdata"))

  # Display git information in log
  cat("\n", cfg$gitInfo$info_str, "\n")

  # Creating a copy of remind to prepare the run in isolation, then delete the tmp runCopy.
  runCopy <- paste0(dirname(cfg$remind_folder), "/tmp_remind_", cfg$title, format(Sys.time(), "_%Y-%m-%d_%H.%M.%S"), "/")
  copyRemind(from = cfg$remind_folder, to = runCopy)
  prepareRemind(cfg, runCopy)
  system(paste0("rm -rf ", runCopy))

  # Prepare MAGICC
  prepareMagicc(resultsDir, cfg$gms$cm_magicc_config)

  # Create the files containing the fixings for delay scenarios (for fixed runs)
  if (cfg$gms$cm_startyear > 2005 && (!file.exists("levs.gms.gz") || !file.exists("levs.gms"))) {
    create_fixing_files(cfg = cfg, input_ref_file = "input_ref.gdx")
  }

  # End timer
  timePrepareEnd <- Sys.time()

  # Save run statistics to local file
  cat("Saving timePrepareStart and timePrepareEnd to runstatistics.rda\n")
  lucode2::runstatistics(
    file = "runstatistics.rda",
    timePrepareStart = timePrepareStart,
    timePrepareEnd = timePrepareEnd
  )

  # Print run time
  prep_time <- timePrepareEnd - timePrepareStart
  cat("\nPreparation completed in", round(as.numeric(prep_time), 2), units(prep_time), "\n")
}
