runLocally <- function(userArgs, scenarios, gitInfo, baseCopy) {
  # Work in baseCopy directory
  withr::local_dir(baseCopy)

  resultFolders <- character(0)

  for (scen in row.names(scenarios)) {
    cfg <- configureCfg(scenarios[scen, ], gitInfo, userArgs, resultFolders)
    cfg <- createResultsFolder(cfg)
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
