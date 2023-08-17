# Creation of the full.gms file.
# Working directory has to be the result folder!
prepare <- function() {
  withr::local_options("warn" = 1)
  # Start timer
  timePrepareStart <- Sys.time()

  cfg <- NULL
  load("config.Rdata")

  # Display git information in log
  cat(cfg$gitInfo$info_str)

  # Creating a copy of remind to prepare the run in isolation, then delete the tmp runCopy.
  runCopy <- paste0(
    dirname(cfg$remind_folder), "/tmp_remind_", cfg$title, format(Sys.time(), "_%Y-%m-%d_%H.%M.%S"), "/"
  )
  folder.copy(from = cfg$remind_folder, to = runCopy)
  prepareRemind(cfg, runCopy)
  prepareMagicc(cfg, runCopy)
  system(paste0("rm -rf ", runCopy))

  # Create the files containing the fixings for delay scenarios (for fixed runs)
  if (cfg$gms$cm_startyear > 2005 && (!file.exists("levs.gms.gz") || !file.exists("levs.gms"))) {
    create_fixing_files(cfg)
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
  prepTime <- timePrepareEnd - timePrepareStart
  cat("Preparation completed in", round(as.numeric(prepTime), 2), units(prepTime), "\n")
}
