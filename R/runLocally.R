runLocally <- function(userArgs, scenarios, gitInfo, base_copy) {
  # Work in base_copy directory
  withr::local_dir(base_copy)

  resultFolders <- character(0)

  for (scen in row.names(scenarios)) {
    cfg <- configure_cfg(scenarios[scen, ], gitInfo, userArgs, resultFolders)
    cfg <- create_results_folder(cfg)
    withr::with_dir(cfg$results_folder, {
      prepare()
      run()
      postProc()
    })
    resultFolder <- cfg$results_folder
    names(resultFolder) <- cfg$title
    resultFolders <- c(resultFolders, resultFolder)
  }
}
