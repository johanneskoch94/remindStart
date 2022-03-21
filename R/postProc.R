# Postprocessing / Output Generation
postProc <- function() {
  # Start timer
  timeOutputStart <- Sys.time()

  # Load 'cfg'
  load("config.Rdata")

  # Change directory
  setwd("../../")

  # Set source_include so that 'output.R' knows it's included as
  # source (instead of being executed from the command line)
  source_include <- TRUE

  # reportCEScalib only works with the calibrate module
  if (cfg$gms$CES_parameters != "calibrate") cfg$output <- setdiff(cfg$output, "reportCEScalib")

  output    <- cfg$output
  outputdir <- cfg$results_folder

  cat("\nStarting output scripts ...")
  sys.source("output.R", envir = new.env())
  cat(" done!\n")

  # End timer
  timeOutputEnd <- Sys.time()

  # Save run statistics to local file
  cat("Saving timeOutputStart and timeOutputEnd to runstatistics.rda\n")
  lucode2::runstatistics(
    file = paste0(cfg$results_folder, "/runstatistics.rda"),
    timeOutputStart = timeOutputStart,
    timeOutputEnd = timeOutputEnd
  )

  # Print run time
  postProc_time <- timeOutputEnd - timeOutputStart
  cat("\nPost-processing completed in", round(as.numeric(postProc_time), 2), units(postProc_time), "\n")
}
