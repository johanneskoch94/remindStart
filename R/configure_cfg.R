configure_cfg <- function(scen,
                          gitInfo,
                          userArgs,
                          slurmConfig = "direct",
                          job_ids = NULL,
                          job_resultFolders = NULL) {

  source("config/default.cfg", local = TRUE)
  cfg$slurmConfig   <- slurmConfig
  cfg$remind_folder <- getwd()
  cfg$gitInfo       <- gitInfo
  cfg$logoption     <- 2 # log output written to file (not to screen)

  if (userArgs$testOneRegi) {
    cfg$title            <- 'testOneRegi'
    cfg$gms$optimization <- 'testOneRegi'
    cfg$output           <- NA
    cfg$results_folder   <- 'output/testOneRegi'
    cfg$force_replace    <- TRUE # delete existing Results directory
  }

  if (!is.null(userArgs$configFile)) cat("\n", row.names(scen), "\n") else cat("\n", cfg$title, "\n")

  # The result folder is still placed in the original remind/ directory.
  # To that end, the cfg$results_folder template is modified:
  cfg$results_folder <- file.path(userArgs$remind, cfg$results_folder)



  # Configure cfg based on settings from csv if provided
  if (!is.null(userArgs$configFile)) {
    # Check if this scen is dependent on other jobs finishing before it can start. If so, then
    # the ids, and the pathways to the folders, of the jobs they depend on have to be determined.

    path_gdx_list <- c("path_gdx", "path_gdx_ref", "path_gdx_refpolicycost", "path_gdx_bau", "path_gdx_carbonprice")

    cond <- path_gdx_list[!is.na(scen[path_gdx_list]) & !grepl("\\.gdx$", scen[["path_gdx_ref"]])]
    depends <- unique(as.character(scen[cond]))

    wait_for_ids <- if (length(depends) == 0) NULL else job_ids[depends]

    # Edit run title
    cfg$title <- row.names(scen)
    message("   Configuring cfg for ", row.names(scen))

    # Edit main model file, region settings and input data revision based on scenarios table, if cell non-empty
    for (switchname in intersect(c("model", "regionmapping", "inputRevision"), names(scen))) {
      if (!is.na(scen[[switchname]] )) {
        cfg[[switchname]] <- scen[[switchname]]
      }
    }

    # Set reporting script
    if ("output" %in% names(scen) && !is.na(scen[["output"]])) {
      cfg$output <- gsub('c\\("|\\)|"', '', strsplit(scen[["output"]], ',')[[1]])
    }

    # Edit switches in default.cfg based on scenarios table, if cell non-empty
    for (switchname in intersect(names(cfg$gms), names(scen))) {
      if (!is.na(scen[[switchname]])) {
        cfg$gms[[switchname]] <- scen[[switchname]]
      }
    }

    # Define path where the GDXs will be taken from
    gdxlist <- c(input.gdx               = file.path(cfg$remind_folder, scen[["path_gdx"]]),
                 input_ref.gdx           = file.path(cfg$remind_folder, scen[["path_gdx_ref"]]),
                 input_refpolicycost.gdx = file.path(cfg$remind_folder, scen[["path_gdx_refpolicycost"]]),
                 input_bau.gdx           = file.path(cfg$remind_folder, scen[["path_gdx_bau"]]),
                 input_carbonprice.gdx   = file.path(cfg$remind_folder, scen[["path_gdx_carbonprice"]]))

    # Drop NAs and add exact path to the fulldata.gdx where necessary
    gdxlistFinal <- NULL
    for (i in seq_along(gdxlist)) {
      if (grepl("NA$",  gdxlist[i])) {
        next
      } else if (grepl("\\.gdx$",  gdxlist[i])){
        gdxlistFinal <- c(gdxlistFinal, gdxlist[i])
      } else {
        h <- file.path(job_resultFolders[basename(gdxlist[i])], "fulldata.gdx")
        names(h) <- names(gdxlist[i])
        gdxlistFinal <- c(gdxlistFinal, h)
      }
    }

    # Add gdxlistFinal to list of files2export
    cfg$files2export$start <- c(cfg$files2export$start, gdxlistFinal)

    cfg
  } else {
    wait_for_ids <- NULL
  }

  if (slurmConfig == "direct") {
    return(cfg)
  } else {
    return(list("cfg" = cfg, "wait_for_ids" = wait_for_ids))
  }
}


create_results_folder <- function(cfg){

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








