handleArgs <- function(remind, configFile, debug, interactive, restart, test, testOneRegi) {
  # Check for command-line arguments
  a <- R.utils::commandArgs(trailingOnly = TRUE)
  if (!(is.null(a) || identical(a, "--args"))) {
    # Overwrite default function arguments with command-line arguments. If non-default function arguments
    # conflict with command-line arguments, throw an error.
    # Remove --args (i.e. the flag to signal that what comes after are the arguments)
    a <- a[a != "--args"]
    for (i in a) {
      if (i == "--testOneRegi") testOneRegi <- TRUE
      else if (i == "--test") test <- TRUE
      else if (i == "--debug") debug <- TRUE
      else if (i == "--interactive") interactive <- TRUE
      else if (i == "--restart") restart <- TRUE
      else if (file.exists(i) && (is.null(configFile) || configFile == i)) configFile <- i
      else abort("User error: unknown command line argument or file path. \\
               Possible arguments are '--debug', '--interactive', '--restart', '--testOneRegi', '--test'.")
    }
  }

  checkUserArguments(remind, configFile, restart, testOneRegi, debug, test, interactive)

  # Make paths absolute
  remind <- normalizePath(remind)
  if (!is.null(configFile)) {
    configFile <- normalizePath(configFile)
  }

  list("remind" = remind,
       "configFile" = configFile,
       "testOneRegi" = testOneRegi,
       "restart" = restart,
       "debug" = debug,
       "interactive" = interactive,
       "test" = test)
}

checkUserArguments <- function(remind, configFile, restart, testOneRegi, debug, test, interactive) {
  if (!all(c("config", "core", "modules", "scripts", "standalone", "tutorials") %in% dir(remind))) {
    abort("Bad remind argument. Does not point to a remind directory.")
  }

  if (!is.null(configFile) && !file.exists(configFile)) {
    abort("Bad configFile argument. File does not exist.")
  }

  if (!is.logical(restart)) {
    abort("Bad restart argument. Must be TRUE or FALSE.")
  }

  if (!is.logical(testOneRegi)) {
    abort("Bad testOneRegi argument. Must be TRUE or FALSE.")
  }

  if (!is.logical(test)) {
    abort("Bad test argument. Must be TRUE or FALSE.")
  }

  if (!is.logical(debug)) {
    abort("Bad debug argument. Must be TRUE or FALSE.")
  }

  if (!is.logical(interactive)) {
    abort("Bad interactive argument. Must be TRUE or FALSE.")
  }

  # Check for incompatibilities
  if (restart && !is.null(configFile)) {
    abort("Arguments are incompatible.")
  }

  invisible(TRUE)
}
