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

### chooseFromList
# @param thelist: list to be selected from
# @param group: list with same dimension as thelist with group names to allow to select whole groups
# @param returnboolean: TRUE: returns list with dimension of thelist with 0 or 1. FALSE: returns selected entries of thelist
# @param multiple: TRUE: allows to select multiple entries. FALSE: no
# @param allowempty: TRUE: allows you not to select anything (returns NA). FALSE: must select something
# @param type: string to be shown to user to understand what he chooses

chooseFromList <- function(thelist, type = "runs", returnboolean = FALSE, multiple = TRUE,
                           allowempty = FALSE, group = FALSE) {
  originallist <- thelist
  booleanlist <- numeric(length(originallist)) # set to zero
  if (! isFALSE(group) && (length(group) != length(originallist) | isFALSE(multiple))) {
    message("group must have same dimension as thelist, or multiple not allowed. Group mode disabled")
    group <- FALSE
  }
  message("\n\nPlease choose ", type,":\n\n")
  if (! isFALSE(group)) {
    groups <- sort(unique(group))
    groupsids <- seq(length(originallist)+2, length(originallist)+length(groups)+1)
    thelist <- c(paste0(str_pad(thelist, max(nchar(originallist)), side = "right"), " ", group), paste("Group:", groups))
    message(str_pad("", max(nchar(originallist)) + nchar(length(thelist)+2)+2, side = "right"), " Group")
  }
  if(multiple)   thelist <- c("all", thelist, "Search by pattern...")
  message(paste(paste(str_pad(1:length(thelist), nchar(length(thelist)), side = "left"), thelist, sep=": " ), collapse="\n"))
  message("\nNumber", ifelse(multiple,"s entered as 2,4:6,9",""),
          ifelse(allowempty, " or leave empty", ""), " (", type, "): ")
  identifier <- strsplit(get_line(), ",")[[1]]
  if (allowempty & length(identifier) == 0) return(NA)
  if (length(identifier) == 0 | ! all(grepl("^[0-9,:]*$", identifier))) {
    message("Try again, you have to choose some numbers.")
    return(chooseFromList(originallist, type, returnboolean, multiple, allowempty, group))
  }
  tmp <- NULL
  for (i in 1:length(identifier)) { # turns 2:5 into 2,3,4,5
    if (length(strsplit(identifier,":")[[i]]) > 1) {
      tmp <- c(tmp,as.numeric(strsplit(identifier,":")[[i]])[1]:as.numeric(strsplit(identifier,":")[[i]])[2])
    }
    else {
      tmp <- c(tmp,as.numeric(identifier[i]))
    }
  }
  identifier <- tmp
  if (! multiple & length(identifier) > 1) {
    message("Try again, not in list or multiple chosen: ", paste(identifier, collapse = ", "))
    return(chooseFromList(originallist, type, returnboolean, multiple, allowempty, group))
  }
  if (any(! identifier %in% seq(length(thelist)))) {
    message("Try again, not all in list: ", paste(identifier, collapse = ", "))
    return(chooseFromList(originallist, type, returnboolean, multiple, allowempty, group))
  }
  if (! isFALSE(group)) {
    selectedgroups <- sub("^Group: ", "", thelist[intersect(identifier, groupsids)])
    identifier <- unique(c(identifier[! identifier %in% groupsids], which(group %in% selectedgroups)+1))
  }
  # PATTERN
  if(multiple && length(identifier == 1) && identifier == length(thelist) ){
    message("\nInsert the search pattern or the regular expression: ")
    pattern <- get_line()
    id <- grep(pattern=pattern, originallist)
    # lists all chosen and ask for the confirmation of the made choice
    message("\n\nYou have chosen the following ", type, ":")
    if (length(id) > 0) message(paste(paste(1:length(id), originallist[id], sep=": "), collapse="\n"))
    message("\nAre you sure these are the right ", type, "? (y/n): ")
    if(get_line() == "y"){
      identifier <- id
      booleanlist[id] <- 1
    } else {
      return(chooseFromList(originallist, type, returnboolean, multiple, allowempty, group))
    }
  } else if(any(thelist[identifier] == "all")){
    booleanlist[] <- 1
    identifier <- 1:length(originallist)
  } else {
    if (multiple) identifier <- identifier - 1
    booleanlist[identifier] <- 1
  }
  if (returnboolean) return(booleanlist) else return(originallist[identifier])
}


slurmIsAvailable <- function() {
  suppressWarnings(ifelse(system2("srun", stdout = FALSE, stderr = FALSE) != 127, TRUE, FALSE))
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

quietly <- function(...) {
  invisible(utils::capture.output(suppressMessages(...)))
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
