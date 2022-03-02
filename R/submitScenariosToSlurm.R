# Loop over scenarios, configure, and submit jobs
submitScenariosToSlurm <- function(userArgs, scenarios, gitInfo, base_copy) {
  # Work in base_copy directory
  withr::local_dir(base_copy)

  # Choose SLURM configuration
  slurmConfig <- choose_slurmConfig()

  # For every scenario, a cfg is configured, a prepare_job is submitted and a run_job is submitted.
  # A "run" job is submitted with a dependency on the prepare job.
  # If a scenario depends on another, its prepare job is submitted with a dependency on the base's run_job.
  # Post-processing jobs are submitted with a dependency on the run jobs.
  job_ids <- character(0)
  job_ids_prepare <- character(0)
  job_resultFolders <- character(0)

  for (scen in row.names(scenarios)) {
    # Configure cfg
    l <- configure_cfg(scenarios[scen,], gitInfo, userArgs, slurmConfig, job_ids, job_resultFolders)
    cfg <- l$cfg
    wait_for_ids <- l$wait_for_ids

    # Create results folder
    cfg <- create_results_folder(cfg)

    # Submit remind prepare job
    job_id_prepare <- submit_remind_prepare(cfg, wait_for = wait_for_ids)
    wait_for_ids <- c(wait_for_ids, job_id_prepare)

    # Submit remind run job
    job_id <- submit_remind_run(cfg, wait_for = wait_for_ids)

    # Submit remind post-processing job
    job_id_postProc <- submit_remind_postProc(cfg, wait_for = job_id)

    # Save job_ids and results folders
    job_ids <- c(job_ids, job_id)
    job_ids_prepare <- c(job_ids_prepare, job_id_prepare)

    job_resultFolder <- cfg$results_folder
    names(job_resultFolder) <- cfg$title
    job_resultFolders <- c(job_resultFolders, job_resultFolder)
  }

  # Delete the tmp copy of remind, once all prepare jobs are done
  submit_clean_up(base_copy, job_ids_prepare)
}


submit_remind_prepare <- function(cfg, wait_for = NULL) {
  # Work in run directory
  withr::local_dir(cfg$results_folder)

  dependency <- if (!is.null(wait_for)) create_dependency_string(wait_for) else ""
  qos <- stringr::str_match(cfg$slurmConfig, "--qos=(.*?) ")[2]

  job_id <- system(paste0("sbatch --job-name=prepare_",
                          cfg$title,
                          dependency,
                          " --kill-on-invalid-dep=yes --output=prepare_log.txt --mail-type=FAIL --time=10 --parsable ",
                          "--comment=REMIND --wrap=\"Rscript -e 'remindStart:::prepare()' \"  --qos=",
                          qos),
                   intern = TRUE)

  names(job_id) <- paste0("prepare_",cfg$title)

  cat("   Submited remindStart:::prepare() as batch job", crayon::green(names(job_id)), "with id:", job_id, "\n")
  if (!is.null(wait_for)){
    cat("     Will wait for", crayon::yellow(paste(names(wait_for), collapse = ", ")), "to finish before starting\n")
  }

  job_id
}


submit_remind_run <- function(cfg, wait_for = NULL) {
  # Work in run directory
  withr::local_dir(cfg$results_folder)

  dependency <- if (!is.null(wait_for)) create_dependency_string(wait_for) else ""

  job_id <- system(paste0("sbatch --job-name=run_",
                          cfg$title,
                          dependency,
                          " --kill-on-invalid-dep=yes --output=log.txt --mail-type=END --parsable ",
                          "--comment=REMIND --wrap=\"Rscript --no-site-file -e 'remindStart:::run()' \" ",
                          cfg$slurmConfig),
                   intern = TRUE)

  names(job_id) <- cfg$title

  cat("   Submited remindStart:::run() as batch job", crayon::green(names(job_id)), "with id:", job_id, "\n")
  if (!is.null(wait_for)){
    cat("     Will wait for", crayon::yellow(paste(names(wait_for), collapse = ", ")), "to finish before starting\n")
  }

  job_id
}


submit_remind_postProc <- function(cfg, wait_for = NULL){
  # Work in run directory
  withr::local_dir(cfg$results_folder)

  dependency <- if (!is.null(wait_for)) create_dependency_string(wait_for) else ""
  qos <- stringr::str_match(cfg$slurmConfig,"--qos=(.*?) ")[2]

  job_id <- system(paste0("sbatch --job-name=postProc_",
                          cfg$title,
                          dependency,
                          " --kill-on-invalid-dep=yes --output=postProc_log.txt --mail-type=FAIL --time=20 --parsable ",
                          "--comment=REMIND --wrap=\"Rscript -e 'remindStart:::postProc()' \"  --qos=",
                          qos,
                          " --ntasks=1 --cpus-per-task=8"),
                   intern = TRUE)

  names(job_id) <- paste0("postProc_",cfg$title)

  cat("   Submited remindStart::postProc() as batch job", crayon::green(names(job_id)), "with id:", job_id, "\n")
  if (!is.null(wait_for)){
    cat("     Will wait for", crayon::yellow(paste(names(wait_for), collapse = ", ")), "to finish before starting\n")
  }

  job_id
}


submit_clean_up <- function(copy_dir, wait_for){
  cat("\n Clean up\n")
  dependency <- create_dependency_string(wait_for, type = "afterany")
  cmd <- paste0("sbatch --job-name=clean_up",
                dependency,
                " --mail-type=FAIL --time=5 --parsable --comment=REMIND --wrap=\"rm -rf ",
                copy_dir,
                "\" --qos=standby")
  job_id <- system(cmd, intern = TRUE)
  cat("   Submited batch job", crayon::green("clean_up"),"with id:", job_id,"\n")
  cat("     Will wait for", crayon::yellow(paste(names(wait_for), collapse = ", ")), "to finish before starting\n")
}


create_dependency_string <- function(wait_for, type = "afterok") {
  out <- paste0(" --dependency=", type)
  for (id in unname(wait_for)){
    out <- paste0(out, ":", id)
  }
  out
}



