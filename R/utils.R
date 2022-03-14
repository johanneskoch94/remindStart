checkOptions <- function() {
  if (is.null(getOption("remind_repos"))) {
    abort("No 'remind_repos' option found. Set the option 'remind_repos' in your .Rpofile to be able to download \\
          input data.")
  }
  repos <- getOption("remind_repos")
  # If direct paths are given (only names of NULL objects), then check if they exist
  if (all(sapply(repos, is.null))) {
    if (!all(sapply(names(repos), file.exists))) {
      abort("Can't find remind_repos. Check that the paths in the 'remind_repos' option exist.")
    }
  # If a SCP/SSH details are provided, check if ssh setup works.
  } else {
    for (i in seq_along(repos)) {
      h <- try(curl::new_handle(verbose = FALSE, .list = repos[[i]]), silent = TRUE)
      t <- try(curl::curl_download(file.path(names(repos[i]), "fileForDownloadTest.txt"), "tmpTest.txt", handle = h))
      if ("try-error" %in% class(t)) {
        abort("Could not download test file from repo {names(x)}. Something went wrong")
      } else {
        unlink("tmpTest.txt")
      }
    }
  }
}

getGitInfo <- function(remind) {
  # Work in remind directory
  withr::local_dir(remind)

  gitInfo <- list()
  gitInfo$status <- try(system("git status -uno", intern = TRUE), silent = TRUE)
  gitInfo$commit <- try(system("git rev-parse --short HEAD", intern = TRUE), silent = TRUE)
  gitInfo$info_str <- paste0("\n##### git info #####\n",
                             "Latest commit: ",
                             try(system("git show -s --format='%h %ci %cn'", intern = TRUE), silent = TRUE),
                             "\nChanges since then: ",
                             paste(gitInfo$status, collapse = "\n"),
                             "\n####################\n\n")
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


gitCloneRemind <- function(from = "git@github.com:remindmodel/remind.git", to = "remind") {
  system(paste("git clone", from, to))
}

gitCloneMagpie <- function(from = "git@github.com:magpiemodel/magpie.git", to = "magpie") {
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


# Create the file to be used in the load mode
getLoadFile <- function(cfg, cal_itr) {
  file_name <- paste0(cfg$gms$cm_CES_configuration, "_ITERATION_", cal_itr, ".inc")
  ces_in <- gsub("\"", "", system("gdxdump fulldata.gdx symb=in NoHeader Format=CSV", intern = TRUE))
  expr_ces_in <- paste0("(", paste(ces_in, collapse = "|"), ")")


  tmp <- grep("(quantity|price|eff|effgr|xi|rho|offset_quantity|compl_coef)",
              x = system("gdxdump fulldata.gdx symb=pm_cesdata", intern = TRUE)[-(1:2)],
              value = TRUE)
  tmp <- grep(expr_ces_in, x = tmp, value = TRUE)

  write(sub("'([^']*)'.'([^']*)'.'([^']*)'.'([^']*)' (.*)[ ,][ /];?",
            "pm_cesdata(\"\\1\",\"\\2\",\"\\3\",\"\\4\") = \\5;",
            x = tmp),
        file_name)


  pm_cesdata_putty <- system("gdxdump fulldata.gdx symb=pm_cesdata_putty", intern = TRUE)
  if (length(pm_cesdata_putty) == 2) {
    tmp_putty <- gsub("^Parameter *([A-z_(,)])+cesParameters\\).*$", '\\1"quantity")  =   0;', pm_cesdata_putty[2])
  } else {
    tmp_putty <- grep("quantity", x = pm_cesdata_putty[-(1:2)], value = TRUE)
    tmp_putty <- grep(expr_ces_in, x = tmp_putty, value = TRUE)
  }

  write(sub("'([^']*)'.'([^']*)'.'([^']*)'.'([^']*)' (.*)[ ,][ /];?",
            "pm_cesdata_putty(\"\\1\",\"\\2\",\"\\3\",\"\\4\") = \\5;",
            x = tmp_putty),
        file_name,
        append = TRUE)
}


# delete entries in stack that contain needle and append new
.setgdxcopy <- function(needle, stack, new) {
  matches <- grepl(needle, stack)
  out <- c(stack[!matches], new)
  return(out)
}
