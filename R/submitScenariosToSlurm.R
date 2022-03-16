# Loop over scenarios, configure, and submit jobs
submitScenariosToSlurm <- function(userArgs, scenarios, gitInfo, baseCopy) {
  # Work in baseCopy directory
  withr::local_dir(baseCopy)

  # Choose SLURM configuration
  slurmConfig <- chooseSlurmConfig()

  # For every scenario, a cfg is configured, a prepare_job is submitted and a run_job is submitted.
  # A "run" job is submitted with a dependency on the prepare job.
  # If a scenario depends on another, its prepare job is submitted with a dependency on the base's run_job.
  # Post-processing jobs are submitted with a dependency on the run jobs.
  jobIds <- character(0)
  jobIdsPrepare <- character(0)
  resultFolders <- character(0)

  for (scen in row.names(scenarios)) {
    # Configure cfg
    l <- configureCfg(scenarios[scen, ], gitInfo, userArgs, resultFolders, slurmConfig, jobIds)
    cfg <- l$cfg
    waitForIds <- l$waitForIds

    # Create results folder
    cfg <- createResultsFolder(cfg)

    # Submit remind prepare job
    jobIdPrepare <- submitRemindPrepare(cfg, waitFor = waitForIds)
    waitForIds <- c(waitForIds, jobIdPrepare)

    # Submit remind run job
    jobId <- submitRemindRun(cfg, waitFor = waitForIds)

    # Submit remind post-processing job
    submitRemindPostProc(cfg, waitFor = jobId)

    # Save jobIds and results folders
    jobIds <- c(jobIds, jobId)
    jobIdsPrepare <- c(jobIdsPrepare, jobIdPrepare)

    resultFolder <- cfg$results_folder
    names(resultFolder) <- cfg$title
    resultFolders <- c(resultFolders, resultFolder)
  }

  # Delete the tmp copy of remind, once all prepare jobs are done
  submitCleanUp(baseCopy, jobIdsPrepare)
}


submitRemindPrepare <- function(cfg, waitFor = NULL) {
  # Work in run directory
  withr::local_dir(cfg$results_folder)

  dependency <- if (!is.null(waitFor)) createDependencyString(waitFor) else ""
  qos <- stringr::str_match(cfg$slurmConfig, "--qos=(.*?) ")[2]

  jobId <- system(paste0(
    "sbatch --job-name=prepare_",
    cfg$title,
    dependency,
    " --kill-on-invalid-dep=yes --output=prepare_log.txt --mail-type=FAIL --time=10 --parsable ",
    "--comment=REMIND --wrap=\"Rscript -e 'remindStart:::prepare()' \"  --qos=",
    qos
  ),
  intern = TRUE
  )

  names(jobId) <- paste0("prepare_", cfg$title)

  cat("   Submited remindStart:::prepare() as batch job", crayon::green(names(jobId)), "with id:", jobId, "\n")
  if (!is.null(waitFor)) {
    cat("     Will wait for", crayon::yellow(paste(names(waitFor), collapse = ", ")), "to finish before starting\n")
  }

  jobId
}


submitRemindRun <- function(cfg, waitFor = NULL) {
  # Work in run directory
  withr::local_dir(cfg$results_folder)

  dependency <- if (!is.null(waitFor)) createDependencyString(waitFor) else ""

  jobId <- system(paste0(
    "sbatch --job-name=run_",
    cfg$title,
    dependency,
    " --kill-on-invalid-dep=yes --output=log.txt --mail-type=END --parsable ",
    "--comment=REMIND --wrap=\"Rscript --no-site-file -e 'remindStart:::run()' \" ",
    cfg$slurmConfig
  ),
  intern = TRUE
  )

  names(jobId) <- cfg$title

  cat("   Submited remindStart:::run() as batch job", crayon::green(names(jobId)), "with id:", jobId, "\n")
  if (!is.null(waitFor)) {
    cat("     Will wait for", crayon::yellow(paste(names(waitFor), collapse = ", ")), "to finish before starting\n")
  }

  jobId
}


submitRemindPostProc <- function(cfg, waitFor = NULL) {
  # Work in run directory
  withr::local_dir(cfg$results_folder)

  dependency <- if (!is.null(waitFor)) createDependencyString(waitFor) else ""
  qos <- stringr::str_match(cfg$slurmConfig, "--qos=(.*?) ")[2]

  jobId <- system(paste0(
    "sbatch --job-name=postProc_",
    cfg$title,
    dependency,
    " --kill-on-invalid-dep=yes --output=postProc_log.txt --mail-type=FAIL --time=20 --parsable ",
    "--comment=REMIND --wrap=\"Rscript -e 'remindStart:::postProc()' \"  --qos=",
    qos,
    " --ntasks=1 --cpus-per-task=8"
  ),
  intern = TRUE
  )

  names(jobId) <- paste0("postProc_", cfg$title)

  cat("   Submited remindStart::postProc() as batch job", crayon::green(names(jobId)), "with id:", jobId, "\n")
  if (!is.null(waitFor)) {
    cat("     Will wait for", crayon::yellow(paste(names(waitFor), collapse = ", ")), "to finish before starting\n")
  }

  invisible(jobId)
}


submitCleanUp <- function(copyDir, waitFor) {
  cat("\n Clean up\n")
  dependency <- createDependencyString(waitFor, type = "afterany")
  cmd <- paste0(
    "sbatch --job-name=clean_up",
    dependency,
    " --mail-type=FAIL --time=5 --parsable --comment=REMIND --wrap=\"rm -rf ",
    copyDir,
    "\" --qos=standby"
  )
  jobId <- system(cmd, intern = TRUE)
  cat("   Submited batch job", crayon::green("clean_up"), "with id:", jobId, "\n")
  cat("     Will wait for", crayon::yellow(paste(names(waitFor), collapse = ", ")), "to finish before starting\n")
}


createDependencyString <- function(waitFor, type = "afterok") {
  out <- paste0(" --dependency=", type)
  for (id in unname(waitFor)) {
    out <- paste0(out, ":", id)
  }
  out
}

submit <- function(cfg) {
  cfg <- createResultsFolder(cfg)
  withr::with_dir(cfg$results_folder, {
    load("config.Rdata")
    prepareRemind(cfg, cfg$remind_folder)
    prepareMagicc(cfg, cfg$remind_folder)
    if (cfg$gms$cm_startyear > 2005 && (!file.exists("levs.gms.gz") || !file.exists("levs.gms"))) {
      create_fixing_files(cfg)
    }
    run()
    postProc()
  })
  cfg$results_folder
}
