getGitInfo <- function(remind) {
  # Work in remind directory
  withr::local_dir(remind)

  gitInfo <- list()
  gitInfo$status <- try(system("git status -uno", intern = TRUE), silent = TRUE)
  gitInfo$commit <- try(system("git rev-parse --short HEAD", intern = TRUE), silent = TRUE)
  gitInfo$info_str <- paste0("\n===== git info =====\n Latest commit: ",
                              try(system("git show -s --format='%h %ci %cn'", intern = TRUE), silent = TRUE),
                              "\nChanges since then: ",
                              paste(gitInfo$status, collapse = "\n"),
                              "\n====================\n")
  gitInfo
}


# Gets characters (line) from the terminal of from a connection and stores it in the return object
get_line <- function() {
  if (interactive()) {
    s <- readline()
  } else {
    con <- file("stdin")
    s <- readLines(con, 1, warn=FALSE)
    on.exit(close(con))
  }
  s
}


# Function definition
copyFromList <- function(filelist, destfolder) {
  # Make sure there are names
  if (is.null(names(filelist))) names(filelist) <- rep("", length(filelist))
  # Loop over files and copy
  for (i in 1:length(filelist)) {
    if (!is.na(filelist[i])) {
      to <- file.path(destfolder, names(filelist)[i])
      if (!file.copy(filelist[i],
                     to = to,
                     recursive = dir.exists(to),
                     overwrite = TRUE)) {
        cat(paste0("Could not copy ", filelist[i], " to ", to, "\n"))
      }
    }
  }
}


slurmIsAvailable <- function() {
  suppressWarnings(ifelse(system2("srun", stdout = FALSE, stderr = FALSE) != 127, TRUE, FALSE))
}



# Little function to stop without printing an error
stopQuietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


createTmpBaseCopy <- function(remind, scenarios) {
  base_copy <- paste0(file.path(dirname(remind), "tmp_remind_base"), format(Sys.time(), "_%Y-%m-%d_%H.%M.%S"), "/")

  copyRemind(from = remind,
             to = base_copy,
             exclude = c("output/", "tutorials/", ".git/", "doc/"),
             include = "scripts/output/")

  # If start gdxs are given, then make sure they're copied as well
  if (!identical(row.names(scenarios), "default")) {
    path_gdx_list <- c("path_gdx", "path_gdx_ref", "path_gdx_refpolicycost", "path_gdx_bau", "path_gdx_carbonprice")
    start_gdxs <- unique(grep("\\.gdx$", scenarios[, path_gdx_list], value = TRUE))
    if (length(start_gdxs) != 0) lapply(start_gdxs, function(x) system(paste0("rsync -a -W --inplace -R ", x, " ", base_copy)))
  }

  base_copy
}

# Copy the remind folder using the rsync command, excluding certain directories
copyRemind <- function(from,
                       to,
                       exclude = NULL,
                       include = NULL) {
  # (-a -> copy eveything. -W and --inplace -> do it fast because we're copying locally)
  rsync_cmd <- paste0("rsync -a -W --inplace ", from, "/ ", to, "/ ")

  # Add explicit includes
  if (!is.null(include)) {
    h <- lapply(include, function(x) paste0("--include ", x, " "))
    rsync_cmd <- paste0(rsync_cmd, paste0(h, collapse = " "))
  }

  # Add explicit excludes
  if (!is.null(exclude)) {
    h <- lapply(exclude, function(x) paste0("--exclude ", x, " "))
    rsync_cmd <- paste0(rsync_cmd, paste0(h, collapse = " "))
  }

  system(rsync_cmd)
}


prepare_test <- function() {
  load("config.Rdata")

  # Copy right gdx file to the output folder
  gdx_name <- paste0("config/gdx-files/", cfg$gms$cm_CES_configuration, ".gdx")
  if (0 != system(paste('cp', gdx_name, file.path(cfg$results_folder, 'input.gdx')))) {
    warning('Could not copy gdx file ', gdx_name)
  }

  # Choose which conopt files to copy
  cfg$files2export$start <- sub("conopt3", cfg$gms$cm_conoptv, cfg$files2export$start)

  # Copy important files into output_folder (before REMIND execution)
  copyFromList(cfg$files2export$start, cfg$results_folder)

  # Save configuration
  save(cfg, file = "config.Rdata")
}

run_test <- function() {
  load("config.Rdata")
  system("copy input.gdx fulldata.gdx")
}


gitCloneRemind <- function(from = "git@github.com:remindmodel/remind.git", to = ".") {
  system(paste("git clone", from, to))
}

abort <- function(x) {
  rlang::abort(glue::glue(x))
}


# didremindfinish is TRUE if full.log exists with status: Normal completion
didremindfinish <- function(fulldatapath) {
  logpath <- paste0(stringr::str_sub(fulldatapath, 1, -14), "/full.log")
  normalCompletion <- any(grep("*** Status: Normal completion", readLines(logpath, warn = FALSE), fixed = TRUE))
  file.exists(logpath) && normalCompletion
}
