##################################################################
################# D E F I N E  debug_coupled #####################
##################################################################

# This function will be called in start_coupled() instead of the regular
# submit() (for REMIND) and start_run (for MAgPIE) functions if the debug
# mode is set to TRUE in start_coupled().
# It creates empty output folders and copies dummy reports into them
# without calling the start scripts of the models.

debug_coupled <- function(model = NULL, cfg) {
  if (is.null(model)) stop("COUPLING DEBUG: Coupling was run in debug mode but no model was specified")

  cat("   Creating results folder", cfg$results_folder, "\n")
  if (!file.exists(cfg$results_folder)) {
    dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
  } else if (!cfg$force_replace) {
    stop(paste0("Results folder ", cfg$results_folder, " could not be created because it already exists."))
  } else {
    cat("    Deleting results folder because it already exists:", cfg$results_folder, "\n")
    unlink(cfg$results_folder, recursive = TRUE)
    dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
  }

  if (model == "rem") {
    cat("COUPLING DEBUG: assuming REMIND\n")
    path_report <- "/home/dklein/REMIND_generic_C_SSP2EU-Tall-PkBudg1020-imp-rem-5.mif"
    to <- paste0(cfg$results_folder, "/REMIND_generic_", cfg$title, ".mif")
  } else if (model == "mag") {
    cat("COUPLING DEBUG: assuming MAGPIE\n")
    path_report <- "/home/dklein/report.mif"
    to <- paste0(cfg$results_folder, "/report.mif")
  } else {
    stop("COUPLING DEBUG: Coupling was started in debug mode but model is unknown")
  }

  cat("COUPLING DEBUG: to =", to, "\n")

  if (!file.copy(from = path_report, to = to)) cat(paste0("Could not copy ", path_report, " to ", to, "\n"))
  return(cfg$results_folder)
}

##################################################################
################# D E F I N E  start_coupled #####################
##################################################################
start_coupled <- function(coupled_config, debug = FALSE) {
  path_remind <- NULL
  path_magpie <- NULL
  cfg_rem <- NULL
  cfg_mag <- NULL
  runname <- NULL
  max_iterations <- NULL
  start_iter <- NULL
  n600_iterations <- NULL
  path_report <- NULL


  # Check for command-line arguments
  a <- R.utils::commandArgs(trailingOnly = TRUE)
  if (!(is.null(a) || identical(a, "--args"))) {
    a <- a[a != "--args"]
    coupled_config <- paste0(a[1], ".RData")
  }
  load(coupled_config)

  # save folder in which this script is executed
  mainwd <- getwd()

  # Retrieve REMIND settings
  cfg_rem <- gms::check_config(cfg_rem, paste0(path_remind, "config/default.cfg"), paste0(path_remind, "modules"))
  cfg_rem$slurmConfig <- "direct"
  # save default setting
  cm_iteration_max_tmp <- cfg_rem$gms$cm_iteration_max
  # save default remind output config and add "emulator" if missing
  cfg_rem_original <- c(setdiff(cfg_rem$output, "emulator"), "emulator")

  # retrieve MAgPIE settings
  cfg_mag <- gms::check_config(cfg_mag, paste0(path_magpie, "config/default.cfg"), paste0(path_magpie, "modules"))
  cfg_mag$sequential <- TRUE
  cfg_mag$force_replace <- TRUE
  cfg_mag$output <- c("rds_report") # ,"remind","report") # rds_report: MAgPIE4; remind,report: MAgPIE3 (glo.modelstat.csv)

  if (start_iter > max_iterations) stop("### COUPLING ### start_iter > max_iterations")

  # Start REMIND and MAgPIE iteratively
  for (i in start_iter:max_iterations) {
    cat("### COUPLING ### Iteration ", i, "\n")

    ##################################################################
    #################### R E M I N D #################################
    ##################################################################

    ####################### PREPARE REMIND ###########################

    cat("### COUPLING ### Preparing REMIND\n")
    cat("### COUPLING ### Set working directory from", getwd())
    setwd(path_remind)
    cat(" to", getwd(), "\n")
    submit <- NULL
    #source("scripts/start/submit.R", local = TRUE) # provide source of "get_magpie_data" and "start_run"

    cfg_rem$results_folder <- paste0("output/", runname, "-rem-", i)
    cfg_rem$title <- paste0(runname, "-rem-", i)
    cfg_rem$force_replace <- TRUE # overwrite existing output folders
    # cfg_rem$gms$biomass    <- "magpie_linear"

    # define gdx paths
    if (i == start_iter) {
      cat("### COUPLING ### gdx in first iteration taken from files2export$start \n")
    } else {
      cat("### COUPLING ### gdx taken from previous iteration\n")
      cfg_rem$files2export$start["input.gdx"] <- paste0("output/", runname, "-rem-", i - 1, "/fulldata.gdx")
      cfg_rem$files2export$start["input_bau.gdx"] <- paste0("output/", runname, "-rem-", i - 1, "/input_bau.gdx")
      cfg_rem$files2export$start["input_ref.gdx"] <- paste0("output/", runname, "-rem-", i - 1, "/input_ref.gdx")
    }

    # Control Negishi iterations
    itr_offset <- 1 # Choose this if negishi iterations should only be adjusted for coupling iteration numbers below 3
    # itr_offset <- start_iter # Choose this if negishi iterations should be adjusted for the first three iterations (regardless of their number)
    # 	This is the case after the coupling was restarted continuing from existing iterations.

    double_iterations <- 1
    if (cfg_rem$gms$cm_SlowConvergence == "on") double_iterations <- 2

    if (i == itr_offset) {
      # Set negisgi iteration to 1 for the first run
      cfg_rem$gms$cm_iteration_max <- 1 * double_iterations
      # } else if (i==itr_offset+1) {
      # 	cfg_rem$gms$cm_iteration_max <- 2*double_iterations
      # } else if (i==itr_offset+2) {
      # 	cfg_rem$gms$cm_iteration_max <- 3*double_iterations
    } else {
      # Set negishi iterations back to the value defined in the config file
      cfg_rem$gms$cm_iteration_max <- cm_iteration_max_tmp
    }
    cat("Set Negishi iterations to", cfg_rem$gms$cm_iteration_max, "\n")

    # Switch off generation of needless output for all but the last REMIND iteration
    if (i < max_iterations) {
      cfg_rem$output <- c("reporting", "emulator", "rds_report")
    } else {
      cfg_rem$output <- cfg_rem_original
    }

    ############ DECIDE IF AND HOW TO START REMIND ###################
    outfolder_rem <- NULL
    if (is.null(path_report)) {
      ######### S T A R T   R E M I N D   S T A N D A L O N E ##############
      cfg_rem$gms$cm_MAgPIE_coupling <- "off"
      cat("### COUPLING ### No MAgPIE report for REMIND input provided.\n")
      cat("### COUPLING ### REMIND will be started in stand-alone mode with\n    ", runname, "\n    ", cfg_rem$results_folder, "\n")
      outfolder_rem <- ifelse(debug, debug_coupled(model = "rem", cfg_rem), submit(cfg_rem))
    } else if (grepl(paste0("report.mif"), path_report)) { # if it is a MAgPIE report
      ######### S T A R T   R E M I N D   C O U P L E D ##############
      cfg_rem$gms$cm_MAgPIE_coupling <- "on"
      if (!file.exists(path_report)) stop(paste0("### COUPLING ### Could not find report: ", path_report, "\n"))
      cat("### COUPLING ### Starting REMIND in coupled mode with\n    Report=", path_report, "\n    Folder=", cfg_rem$results_folder, "\n")
      # Keep path to MAgPIE report in mind to have it available after the coupling loop
      mag_report_keep_in_mind <- path_report
      cfg_rem$pathToMagpieReport <- path_report
      outfolder_rem <- ifelse(debug, debug_coupled(model = "rem", cfg_rem), submit(cfg_rem))
      ############################
    } else if (grepl("REMIND_generic_", path_report)) { # if it is a REMIND report
      ############### O M I T   R E M I N D  ###############################
      cat("### COUPLING ### Omitting REMIND in this iteration\n    Report=", path_report, "\n")
      path_report <- path_report
    } else {
      stop(paste0("### COUPLING ### Could not decide whether ", path_report, " is REMIND or MAgPIE output.\n"))
    }

    if (!is.null(outfolder_rem)) {
      path_report <- paste0(path_remind, outfolder_rem, "/REMIND_generic_", cfg_rem$title, ".mif")
      cat("### COUPLING ### REMIND output was stored in ", outfolder_rem, "\n")
      if (file.exists(paste0(outfolder_rem, "/fulldata.gdx"))) {
        modstat <- gdx::readGDX(paste0(outfolder_rem, "/fulldata.gdx"),
                                types = "parameters",
                                format = "raw",
                                c("s80_bool", "o_modelstat"))
        if (cfg_rem$gms$optimization == "negishi") {
          if (as.numeric(modstat$o_modelstat$val) != 2 && as.numeric(modstat$o_modelstat$val) != 7) stop("Iteration stopped! REMIND o_modelstat was ", modstat, " but is required to be 2 or 7.\n")
        } else if (cfg_rem$gms$optimization == "nash") {
          if (as.numeric(modstat$s80_bool$val) != 1) cat("Warning: REMIND s80_bool not 1. Iteration continued though.\n")
        }
      } else if (file.exists(paste0(outfolder_rem, "/non_optimal.gdx"))) {
        stop("### COUPLING ### REMIND didn't find an optimal solution. Coupling iteration stopped!")
      } else if (debug) {
        # continue
      } else {
        stop("### COUPLING ### REMIND didn't produce any gdx. Coupling iteration stopped!")
      }
    }

    if (!file.exists(path_report)) stop(paste0("### COUPLING ### Could not find report: ", path_report, "\n"))

    # If in the last iteration don't run MAgPIE
    if (i == max_iterations) break

    ##################################################################
    #################### M A G P I E #################################
    ##################################################################
    cat("### COUPLING ### Preparing MAgPIE\n")
    cat("### COUPLING ### Set working directory from", getwd())
    setwd(path_magpie)
    cat(" to", getwd(), "\n")
    start_run <- NULL
    source("scripts/start_functions.R", local = TRUE)
    cfg_mag$results_folder <- paste0("output/", runname, "-mag-", i)
    cfg_mag$title <- paste0(runname, "-mag-", i)

    # Increase MAgPIE resolution n600_iterations before final iteration so that REMIND
    # runs n600_iterations iterations using results from MAgPIE with higher resolution
    if (i > (max_iterations - n600_iterations)) {
      cat("Current iteration":i, ". Setting MAgPIE to n600\n")
      cfg_mag <- gms::setScenario(cfg_mag, "n600", scenario_config = paste0("config/scenario_config.csv"))
    }

    # Providing MAgPIE with gdx from last iteration's solution only for time steps >= cfg_rem$gms$cm_startyear
    # For years prior to cfg_rem$gms$cm_startyear MAgPIE output has to be identical across iterations.
    # Because gdxes might slightly lead to a different solution exclude gdxes for the fixing years.
    if (i > 1) {
      cat("### COUPLING ### Copying gdx files from previous iteration\n")
      gdxlist <- paste0("output/", runname, "-mag-", i - 1, "/magpie_y", seq(cfg_rem$gms$cm_startyear, 2150, 5), ".gdx")
      cfg_mag$files2export$start <- .setgdxcopy(".gdx", cfg_mag$files2export$start, gdxlist)
    }

    cat("### COUPLING ### MAgPIE will be started with\n    Report = ", path_report, "\n    Folder=", cfg_mag$results_folder, "\n")
    cfg_mag$path_to_report_bioenergy <- path_report
    # if no different mif was set for GHG prices use the same as for bioenergy
    if (is.na(cfg_mag$path_to_report_ghgprices)) cfg_mag$path_to_report_ghgprices <- path_report
    ########### START MAGPIE #############
    outfolder_mag <- ifelse(debug, debug_coupled(model = "mag", cfg_mag), start_run(cfg_mag, codeCheck = FALSE))
    ######################################
    cat("### COUPLING ### MAgPIE output was stored in ", outfolder_mag, "\n")
    path_report <- paste0(path_magpie, outfolder_mag, "/report.mif")

    # Checking whether MAgPIE is optimal in all years
    file_modstat <- paste0(outfolder_mag, "/glo.magpie_modelstat.csv")
    if (debug) {
      modstat_mag <- 2
    } else if (file.exists(file_modstat)) {
      modstat_mag <- utils::read.csv(file_modstat, stringsAsFactors = FALSE, row.names = 1, na.strings = "")
    } else {
      modstat_mag <- gdx::readGDX(paste0(outfolder_mag, "/fulldata.gdx"), "p80_modelstat", "o_modelstat", format = "first_found")
    }

    if (!all((modstat_mag == 2) | (modstat_mag == 7))) {
      stop("Iteration stopped! MAgPIE modelstat is not 2 or 7 for all years.\n")
    }
  }
  # End of coupling iteration loop

  cat("### COUPLING ### Last coupling iteration completed\n")
  cat("### COUPLING ### Set working directory from", getwd())
  setwd(mainwd)
  cat(" to", getwd(), "\n")

  # for the sbatch command of the subsequent runs below set the number of tasks per node
  # this not clean, because we use the number of regions of the *current* run to set the number of tasks for the *subsequent* runs
  # but it is sufficiently clean, since the number of regions should not differ between current and subsequent
  if (cfg_rem$gms$optimization == "nash" && cfg_rem$gms$cm_nash_mode == "parallel") {
    # for nash: set the number of CPUs per node to number of regions + 1
    nr_of_regions <- length(levels(utils::read.csv2(cfg_rem$regionmapping)$RegionCode)) + 1
  } else {
    # for negishi: use only one CPU
    nr_of_regions <- 1
  }

  # extract subsequent runs from list: take the name of the rows that have the current scenario (= runname without "C_") in the path_gdx_ref column
  subsequent_runs <- rownames(cfg_rem$RunsUsingTHISgdxAsInput[cfg_rem$RunsUsingTHISgdxAsInput[, "path_gdx_ref"] == gsub("^C_", "", runname), ])

  # Start subsequent runs via sbatch
  for (run in subsequent_runs) {
    cat("Submitting subsequent run", run, "\n")
    # load the config of the subsequent run to provide the correct qos setting (use new environmet to not overwrite the cfg_rem of the current run)
    subseq.env <- new.env()
    load(paste0("C_", run, ".RData"), envir = subseq.env)
    system(paste0("sbatch --qos=", subseq.env$qos, " --job-name=C_", run, " --output=C_", run, ".log --mail-type=END --comment=REMIND-MAgPIE --tasks-per-node=", nr_of_regions, " --wrap=\"Rscript -e 'remindStart:::start_coupled()' --args C_", run, ".RData\""))
  }

  # Read runtime of ALL coupled runs (not just the current scenario) and produce comparison pdf
  remindpath <- paste0(path_remind, "output")
  magpiepath <- paste0(path_magpie, "output")

  cat("### COUPLING ### Preparing runtime.pdf\n")
  runs <- lucode2::findCoupledruns(resultsfolder = remindpath)
  ret <- lucode2::findIterations(runs, modelpath = c(remindpath, magpiepath), latest = FALSE)
  lucode2::readRuntime(ret, plot = TRUE, coupled = TRUE)
  unlink(c("runtime.log", "runtime.out", "runtime.rda"))

  # combine REMIND and MAgPIE reports of last coupling iteration (and REMIND water reporting if existing)
  report_rem <- paste0(path_remind, outfolder_rem, "/REMIND_generic_", cfg_rem$title, ".mif")
  if (exists("outfolder_mag")) {
    # If MAgPIE has run use its regular outputfolder
    report_mag <- paste0(path_magpie, outfolder_mag, "/report.mif")
  } else {
    # If MAgPIE did not run, because coupling has been restarted with the last REMIND iteration,
    # use the path to the MAgPIE report REMIND has been restarted with.
    report_mag <- mag_report_keep_in_mind
  }
  cat("Joining to a common reporting file:\n    ", report_rem, "\n    ", report_mag, "\n")
  tmp1 <- read.report(report_rem, as.list = FALSE)
  tmp2 <- read.report(report_mag, as.list = FALSE)[, getYears(tmp1), ]
  tmp3 <- mbind(tmp1, tmp2)
  getNames(tmp3, dim = 1) <- gsub("-(rem|mag)-[0-9]{1,2}", "", getNames(tmp3, dim = 1)) # remove -rem-xx and mag-xx from scenario names
  # only harmonize model names to REMIND-MAgPIE, if there are no variable names that are identical across the models
  if (any(getNames(tmp3[, , "REMIND"], dim = 3) %in% getNames(tmp3[, , "MAgPIE"], dim = 3))) {
    msg <- "Cannot produce common REMIND-MAgPIE reporting because there are identical variable names in both models!\n"
    cat(msg)
    warning(msg)
  } else {
    # Replace REMIND and MAgPIE with REMIND-MAgPIE
    # getNames(tmp3,dim=2) <- gsub("REMIND|MAgPIE","REMIND-MAGPIE",getNames(tmp3,dim=2))
    write.report(tmp3, file = paste0("output/", runname, ".mif"))
  }

  # set required variables and execute script to create convergence plots
  cat("### COUPLING ### Preparing convergence pdf\n")
  source_include <- TRUE
  runs <- runname
  folder <- "./output"
  source("scripts/output/comparison/plot_compare_iterations.R", local = TRUE)
}
