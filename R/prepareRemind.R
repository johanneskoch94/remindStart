prepareRemind <- function(cfg, runCopy) {
  # Work in runCopy directory
  withr::local_dir(runCopy)

  ###########################################################
  ### PROCESSING INPUT DATA ###################### START ####
  ###########################################################
  # Create input file with exogenous CO2 tax using the CO2 price from another run
  if (!is.null(cfg$gms$carbonprice) &&
      (cfg$gms$carbonprice == "exogenous") &&
      (!is.na(cfg$files2export$start["input_carbonprice.gdx"]))) {
    cat(paste(
      "\n Run scripts/input/create_input_for_45_carbonprice_exogenous.R to create",
      "input file with exogenous CO2 tax from another run.\n"
    ))
    create_input_for_45_carbonprice_exogenous(as.character(cfg$files2export$start["input_carbonprice.gdx"]))
  }

  # write name of corresponding CES file to datainput.gms
  gms::replace_in_file(
    file = "./modules/29_CES_parameters/load/datainput.gms",
    content = paste0('$include "./modules/29_CES_parameters/load/input/', cfg$gms$cm_CES_configuration, '.inc"'),
    subject = "CES INPUT"
  )

  # If a path to a MAgPIE report is supplied use it as REMIND intput (used for REMIND-MAgPIE coupling)
  # ATTENTION: modifying gms files
  if (!is.null(cfg$pathToMagpieReport)) {
    getReportData(path_to_report = cfg$pathToMagpieReport, inputpath_mag = cfg$gms$biomass, inputpath_acc = cfg$gms$agCosts)
  }

  # Update module paths in GAMS code
  gms::update_modules_embedding()

  # Check all setglobal settings for consistency
  gms::settingsCheck()

  # Configure main.gms file (or other given by cfg$model) based on settings of cfg$gms file
  lucode2::manipulateConfig(cfg$model, cfg$gms)

  ############ download and distribute input data ########
  # check wheather the regional resolution and input data revision are outdated and update data if needed
  input_old <- if (file.exists("input/source_files.log")) readLines("input/source_files.log")[1] else "no_data"

  input_new <- c(
    paste0("rev", cfg$inputRevision, "_", cfg$regionscode, "_", tolower(cfg$model_name), ".tgz"),
    paste0("rev", cfg$inputRevision, "_", cfg$regionscode, "_", tolower(cfg$validationmodel_name), ".tgz"),
    paste0("CESparametersAndGDX_", cfg$CESandGDXversion, ".tgz")
  )

  if (!setequal(input_new, input_old) | cfg$force_download) {
    cat("Your input data is outdated or in a different regional resolution. New data will be downloaded and distributed.\n")
    gms::download_distribute(
      files = input_new,
      repositories = cfg$repositories, # defined in your local .Rprofile or on the cluster /p/projects/rd3mod/R/.Rprofile
      modelfolder = ".",
      debug = FALSE
    )
  }

  # extract BAU emissions for NDC runs to set up emission goals for region where only some countries have a target
  if ((!is.null(cfg$gms$carbonprice) && (cfg$gms$carbonprice == "NDC")) |
      (!is.null(cfg$gms$carbonpriceRegi) && (cfg$gms$carbonpriceRegi == "NDC")) ){
    prepare_NDC(as.character(cfg$files2export$start["input_bau.gdx"]), cfg)
  }

  ############ update information ########################
  # update_info, which regional resolution and input data revision in cfg$model
  update_info(cfg, cfg$regionscode, cfg$revision)
  # update_sets, which is updating the region-depending sets in core/sets.gms
  #-- load new mapping information
  map <- utils::read.csv(cfg$regionmapping, sep = ";")
  update_sets(cfg, map)

  ########################################################
  ### PROCESSING INPUT DATA ###################### END ###
  ########################################################

  ### ADD MODULE INFO IN SETS  ############# START #######
  content <- NULL
  modification_warning <- c(
    "*** THIS CODE IS CREATED AUTOMATICALLY, DO NOT MODIFY THESE LINES DIRECTLY",
    "*** ANY DIRECT MODIFICATION WILL BE LOST AFTER NEXT MODEL START",
    "*** CHANGES CAN BE DONE USING THE RESPECTIVE LINES IN scripts/start_functions.R"
  )
  content <- c(modification_warning, "", "sets")
  content <- c(content, "", '       modules "all the available modules"')
  content <- c(content, "       /", paste0("       ", gms::getModules("modules/")[, "name"]), "       /")
  content <- c(content, "", 'module2realisation(modules,*) "mapping of modules and active realisations" /')
  content <- c(content, paste0("       ", gms::getModules("modules/")[, "name"], " . %", gms::getModules("modules/")[, "name"], "%"))
  content <- c(content, "      /", ";")
  gms::replace_in_file("core/sets.gms", content, "MODULES", comment = "***")
  ### ADD MODULE INFO IN SETS  ############# END #########

  # Copy right gdx file to the output folder
  gdx_name <- paste0("config/gdx-files/", cfg$gms$cm_CES_configuration, ".gdx")
  if (0 != system(paste("cp", gdx_name, file.path(cfg$results_folder, "input.gdx")))) {
    stop("Could not copy gdx file ", gdx_name)
  }

  # Choose which conopt files to copy
  cfg$files2export$start <- sub("conopt3", cfg$gms$cm_conoptv, cfg$files2export$start)

  # Copy important files into output_folder (before REMIND execution)
  copyFromList(cfg$files2export$start, cfg$results_folder)

  # Save configuration
  save(cfg, file = file.path(cfg$results_folder, "config.Rdata"))
  yaml::write_yaml(cfg, file = file.path(cfg$results_folder, "cfg.txt"))

  # Merge GAMS files
  cat("################\nCreating full.gms ...")
  gms::singleGAMSfile(mainfile = cfg$model, output = file.path(cfg$results_folder, "full.gms"))
  cat(" done.\n################\n\n")

  # Collect run statistics (will be saved to central database in submit.R)
  lucode2::runstatistics(
    file = paste0(cfg$results_folder, "/runstatistics.rda"),
    user = Sys.info()[["user"]],
    date = Sys.time(),
    version_management = "git",
    revision = cfg$gitInfo$commit,
    # revision_date = try(as.POSIXct(system("git show -s --format=%ci", intern=TRUE), silent=TRUE)),
    status = cfg$gitInfo$status
  )
}
