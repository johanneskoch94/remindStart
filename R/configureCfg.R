configureCfg <- function(scen,
                         gitInfo,
                         userArgs,
                         prevScenResultFolders,
                         slurmConfig = "direct",
                         job_ids = NULL) {
  source("config/default.cfg", local = TRUE)
  cfg$title <- ifelse(!is.null(row.names(scen)), row.names(scen), "default")
  cfg$slurmConfig <- slurmConfig
  cfg$remind_folder <- getwd()
  cfg$gitInfo <- gitInfo
  cfg$logoption <- 2 # log output written to file


  if (userArgs$testOneRegi) {
    cfg$title <- "testOneRegi"
    cfg$gms$optimization <- "testOneRegi"
    cfg$output <- NA
    cfg$results_folder <- "output/testOneRegi"
    cfg$force_replace <- TRUE # delete existing Results directory
  }

  # To console
  cat("\n", crayon::green(cfg$title), "\n")
  cat("   Configuring cfg for", cfg$title, "\n")

  # The result folder is still placed in the original remind/ directory.
  # To that end, the cfg$results_folder template is modified:
  cfg$results_folder <- file.path(userArgs$remind, cfg$results_folder)

  # Configure cfg based on settings from csv if provided
  if (!is.null(userArgs$configFile)) {
    # Edit main model file, region settings and input data revision based on scenarios table, if cell non-empty
    for (switchname in intersect(c("model", "regionmapping", "inputRevision"), names(scen))) {
      if (!is.na(scen[[switchname]])) {
        cfg[[switchname]] <- scen[[switchname]]
      }
    }

    # Set reporting script
    if ("output" %in% names(scen) && !is.na(scen[["output"]])) {
      cfg$output <- gsub('c\\("|\\)|"', "", strsplit(scen[["output"]], ",")[[1]])
    }

    # Edit switches in default.cfg based on scenarios table, if cell non-empty
    for (switchname in intersect(names(cfg$gms), names(scen))) {
      if (!is.na(scen[[switchname]])) {
        cfg$gms[[switchname]] <- scen[[switchname]]
      }
    }

    gdxlist <- c(
      input.gdx = scen[["path_gdx"]],
      input_ref.gdx = scen[["path_gdx_ref"]],
      input_refpolicycost.gdx = scen[["path_gdx_refpolicycost"]],
      input_bau.gdx = scen[["path_gdx_bau"]],
      input_carbonprice.gdx = scen[["path_gdx_carbonprice"]]
    )

    # Drop NAs and add exact path to the fulldata.gdx where necessary
    gdxlistFinal <- NULL
    for (i in seq_along(gdxlist)) {
      if (is.na(gdxlist[i])) {
        next
      } else if (grepl("\\.gdx$", gdxlist[i])) {
        gdxlistFinal <- c(gdxlistFinal, gdxlist[i])
      } else {
        h <- file.path(prevScenResultFolders[gdxlist[[i]]], "fulldata.gdx")
        names(h) <- names(gdxlist[i])
        gdxlistFinal <- c(gdxlistFinal, h)
      }
    }

    # Add gdxlistFinal to list of files2export
    cfg$files2export$start <- c(cfg$files2export$start, gdxlistFinal)

    cfg
  }

  # Further config settings - set after cfg.csv files are loaded
  cfg$gms$c_expname <- cfg$title
  if (is.null(cfg$model)) cfg$model <- "main.gms"
  cfg$gms$c_GDPpcScen <- gsub("gdp_", "", cfg$gms$cm_GDPscen)
  # Suppress madrat start-up messages
  cfg$regionscode <- suppressMessages(madrat::regionscode(cfg$regionmapping))
  cfg$gms$cm_CES_configuration <- paste0(
    "indu_", cfg$gms$industry, "-",
    "buil_", cfg$gms$buildings, "-",
    "tran_", cfg$gms$transport, "-",
    "POP_", cfg$gms$cm_POPscen, "-",
    "GDP_", cfg$gms$cm_GDPscen, "-",
    "En_", cfg$gms$cm_demScen, "-",
    "Kap_", cfg$gms$capitalMarket, "-",
    if (cfg$gms$cm_calibration_string == "off") "" else paste0(cfg$gms$cm_calibration_string, "-"),
    "Reg_", cfg$regionscode
  )

  if (slurmConfig != "direct" && !is.null(userArgs$configFile)) {
    # Check if this scen is dependent on other jobs finishing before it can start. If so, then
    # the ids, and the pathways to the folders, of the jobs they depend on have to be determined.
    path_gdx_list <- c("path_gdx", "path_gdx_ref", "path_gdx_refpolicycost", "path_gdx_bau", "path_gdx_carbonprice")
    cond <- path_gdx_list[!is.na(scen[path_gdx_list]) & !grepl("\\.gdx$", scen[path_gdx_list])]
    depends <- unique(as.character(scen[cond]))
    if (length(depends) != 0) cfg$waitForIds <- job_ids[depends]
  }

  cfg
}
