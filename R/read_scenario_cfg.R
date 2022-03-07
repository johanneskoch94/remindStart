read_scenario_cfg <- function(configFile, default.cfg = file.path(dirname(configFile), "default.cfg")) {
  # Read-in the switches table, use first column as row names
  settings <- utils::read.csv2(configFile, stringsAsFactors = FALSE, row.names = 1, comment.char = "#", na.strings = "")

  # Add empty path_gdx_... columns if they are missing
  path_gdx_list <- c("path_gdx", "path_gdx_ref", "path_gdx_refpolicycost", "path_gdx_bau", "path_gdx_carbonprice")
  if ("path_gdx_ref" %in% names(settings) && !"path_gdx_refpolicycost" %in% names(settings)) {
    settings$path_gdx_refpolicycost <- settings$path_gdx_ref
    warning("No column path_gdx_refpolicycost for policy cost comparison found, using path_gdx_ref instead.")
  }
  settings[, path_gdx_list[!path_gdx_list %in% names(settings)]] <- NA

  # State if columns are unknown - in which case they will be ignored -, and stop for some outdated parameters.
  source(default.cfg, local = TRUE)
  knownColumnNames <- c(names(cfg$gms), path_gdx_list, "start", "output", "model", "regionmapping", "inputRevision")
  unknownColumnNames <- names(settings)[!names(settings) %in% knownColumnNames]
  if (length(unknownColumnNames) > 0) {
    message("\nAutomated checks did not find counterparts in default.cfg for these config file columns:")
    message("  ", paste(unknownColumnNames, collapse = ", "))
    message("start.R might simply ignore them. Please check if these switches are not deprecated.")
    message("This check was added Jan. 2022. If you find false positives, add them to knownColumnNames in start.R.\n")
    forbiddenColumnNames <- list( # specify forbidden column name and what should be done with it
      "c_budgetCO2" = "Rename to c_budgetCO2from2020, adapt emission budgets, see https://github.com/remindmodel/remind/pull/640",
      "c_budgetCO2FFI" = "Rename to c_budgetCO2from2020FFI, adapt emission budgets, see https://github.com/remindmodel/remind/pull/640"
    )
    for (i in intersect(names(forbiddenColumnNames), unknownColumnNames)) {
      message("Column name ", i, " in ", configFile, " is outdated. ", forbiddenColumnNames[i])
    }
    if (any(names(forbiddenColumnNames) %in% unknownColumnNames)) {
      stop("Outdated column names found that must not be used. Stopped.")
    }
  }

  # Perform checks on scenarios that are flagged to start
  scenarios <- settings[settings$start == 1, ]

  # Make sure scenario names don't includes a "."
  if (length(grep("\\.", rownames(scenarios))) > 0) {
    stop(paste0(
      "These titles contain dots: ",
      paste0(rownames(scenarios)[grep("\\.", rownames(scenarios))], collapse = ", "),
      " GAMS would not tolerate this, and quit working at a point where you least expect it. Stopping now."
    ))
  }
  # Make sure scenario names don't end with a "_"
  if (length(grep("_$", rownames(scenarios))) > 0) {
    stop(paste0(
      "These titles end with _: ",
      paste0(rownames(scenarios)[grep("_$", rownames(scenarios))], collapse = ", "),
      ". This may lead start.R to select wrong gdx files. Stopping now."
    ))
  }
  # Make sure scenario names have less than 75 characters
  if (any(nchar(rownames(scenarios)) > 75)) {
    stop(paste0(
      "These have more than 75 characters ",
      paste0(rownames(scenarios)[any(nchar(rownames(scenarios)) > 75)], collapse = ", "),
      " GAMS would not tolerate this, and quit working at a point where you least expect it. Stopping now."
    ))
  }

  # Perform checks on the path_gdx... column and assign corresponding gdx files when necessary
  for (iscen in row.names(scenarios)) {
    # for columns path_gdx, check whether the cell is non-empty, and not the title of another run with start = 1
    # if not a full path ending with .gdx provided, search for most recent folder with that title
    path_gdx_list <- c("path_gdx", "path_gdx_ref", "path_gdx_refpolicycost", "path_gdx_bau", "path_gdx_carbonprice")
    if (any(iscen %in% scenarios[iscen, path_gdx_list])) {
      stop("Self-reference: ", iscen, " refers to itself in a path_gdx... column.")
    }
    for (path_to_gdx in path_gdx_list) {
      if (!is.na(scenarios[iscen, path_to_gdx]) && !scenarios[iscen, path_to_gdx] %in% row.names(scenarios)) {
        if (!grepl("\\.gdx$", scenarios[iscen, path_to_gdx])) {
          outputDir <- file.path(dirname(dirname(configFile)), "output")
          # search for fulldata.gdx in output directories starting with the path_to_gdx cell content.
          # may include folders that only _start_ with this string. They are sorted out later.
          dirs <- Sys.glob(file.path(outputDir, paste0(scenarios[iscen, path_to_gdx], "*"), "fulldata.gdx"))
          # if path_to_gdx cell content exactly matches folder name, use this one
          h <- file.path(outputDir, scenarios[iscen, path_to_gdx], "fulldata.gdx")
          if (h %in% dirs) {
            message(paste0("   For ", path_to_gdx, " = ", scenarios[iscen, path_to_gdx], ", a folder with fulldata.gdx was found."))
            scenarios[iscen, path_to_gdx] <- h
          } else {

            # sort out unfinished runs and folder names that only _start_ with the path_to_gdx cell content
            # for folder names only allows: cell content, an optional _, datetimepattern
            # the optional _ can be appended in the scenario-config path_to_gdx cell to force using an
            # existing fulldata.gdx instead of queueing as a subsequent run, see tutorial 3.
            datetimepattern <- "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}"
            dirs <- dirs[unlist(lapply(dirs, didremindfinish)) &
              grepl(paste0(scenarios[iscen, path_to_gdx], "_?", datetimepattern, "/fulldata.gdx"), dirs)]
            # if anything found, pick latest
            if (length(dirs) > 0 && !all(is.na(dirs))) {
              latest_fulldata <- lapply(dirs, stringr::str_sub, -32, -14) %>%
                strptime(format = "%Y-%m-%d_%H.%M.%S") %>%
                as.numeric() %>%
                which.max()
              message(paste0(
                "   Use newest normally completed run for ", path_to_gdx,
                " = ", scenarios[iscen, path_to_gdx], ":\n     ",
                dirname(dirs[latest_fulldata])
              ))
              scenarios[iscen, path_to_gdx] <- dirs[latest_fulldata]
            }
          }
        }
        # if the above has not created a path to a valid gdx, stop
        if (!file.exists(scenarios[iscen, path_to_gdx])) {
          stop(paste0(
            "Can't find a gdx specified as ",
            scenarios[iscen, path_to_gdx], " in column ", path_to_gdx,
            ". Please specify full path to gdx or name of output subfolder that contains a fulldata.gdx from a previous normally completed run."
          ))
        }
      }
    }
  }

  # Order scenarios so that runs on which other runs depend get submitted first. The while loop adds scenarios to
  # 'next_up', as soon as all scenarios in path_gdx_list are present in ordered_scenarios. Once added, they are not
  # checked again.
  ordered_scenarios <- NULL
  while (length(ordered_scenarios) < dim(scenarios)[1]) {
    next_up <- scenarios %>%
      dplyr::filter(
        !row.names(.) %in% ordered_scenarios,
        dplyr::if_all(dplyr::all_of(path_gdx_list), ~ is.na(.x) | .x %in% ordered_scenarios | grepl("\\.gdx$", .x))
      ) %>%
      row.names()
    ordered_scenarios <- c(ordered_scenarios, next_up)
  }

  # Return scenarios in new order given by the rownames in ordered_scenarios
  scenarios[order(match(row.names(scenarios), ordered_scenarios)), ]
}
