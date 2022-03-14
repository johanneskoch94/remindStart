prepareMagicc <- function(cfg, runCopy) {
  # Copy MAGICC
  if (!file.exists(cfg$magicc_template) & file.exists(path.expand(Sys.getenv("MAGICC")))) {
    cfg$magicc_template <- path.expand(Sys.getenv("MAGICC"))
  }

  if (file.exists(cfg$magicc_template)) {
    cat("Copying MAGICC files from", cfg$magicc_template, "to results folder\n")
    system(paste0("cp -rp ", cfg$magicc_template, " ./"))
    system(paste0("cp -rp ", runCopy, "/core/magicc/* ./magicc/"))
  } else {
    warning(paste0("Could not copy", cfg$magicc_template, "because it does not exist."))
  }

  try_copy <- try(file.copy("magicc/run_magicc.R", "run_magicc.R"))
  try_copy <- try(file.copy("magicc/run_magicc_temperatureImpulseResponse.R", "run_magicc_temperatureImpulseResponse.R"))
  try_copy <- try(file.copy("magicc/read_DAT_TOTAL_ANTHRO_RF.R", "read_DAT_TOTAL_ANTHRO_RF.R"))
  try_copy <- try(file.copy("magicc/read_DAT_SURFACE_TEMP.R", "read_DAT_SURFACE_TEMP.R"))

  # Set MAGCFG file
  magcfgFile <- paste0("./magicc/MAGCFG_STORE/", "MAGCFG_USER_", toupper(cfg$gms$cm_magicc_config), ".CFG")
  if (!file.exists(magcfgFile)) {
    warning(paste("Bad MAGGICC configuration: Could not find file ", magcfgFile))
  } else {
    file.copy(magcfgFile, "./magicc/MAGCFG_USER.CFG")
  }
}
