# Load and check configuration for consistency
loadAndCheckCfg <- function(cfgFile) {
  load(cfgFile)
  cat(crayon::green("Checking configuration settings.\n"))
  cfg <- gms::check_config(
    icfg = cfg,
    modulepath = file.path(cfg$remind_folder, "modules"),
    reference_file = file.path(cfg$remind_folder, "config/default.cfg"),
    settings_config = file.path(cfg$remind_folder, "config/settings_config.csv"),
    extras = c("remind_folder", "gitInfo", "regionscode", "mock")
  )

  # Check for compatibility with subsidizeLearning
  if ((cfg$gms$optimization != "nash") & (cfg$gms$subsidizeLearning == "globallyOptimal")) {
    cat(paste(
      "Only optimization = 'nash' is compatible with subsudizeLearning = 'globallyOptimal'.",
      "Switching subsidizeLearning to 'off' now.\n"
    ))
    cfg$gms$subsidizeLearning <- "off"
  }
  cfg
}

update_info <- function(cfg, regionscode, revision) {
  subject <- "VERSION INFO"
  content <- c(
    "",
    paste("Regionscode:", regionscode),
    "",
    paste("Input data revision:", revision),
    "",
    paste("Last modification (input data):", date()),
    ""
  )
  gms::replace_in_file(cfg$model, paste("*", content), subject)
}

update_sets <- function(cfg, map) {
  .tmp <- function(x, prefix = "", suffix1 = "", suffix2 = " /", collapse = ",", n = 10) {
    content <- NULL
    tmp <- lapply(split(x, ceiling(seq_along(x) / n)), paste, collapse = collapse)
    end <- suffix1
    for (i in 1:length(tmp)) {
      if (i == length(tmp)) end <- suffix2
      content <- c(content, paste0("       ", prefix, tmp[[i]], end))
    }
    return(content)
  }
  modification_warning <- c(
    "*** THIS CODE IS CREATED AUTOMATICALLY, DO NOT MODIFY THESE LINES DIRECTLY",
    "*** ANY DIRECT MODIFICATION WILL BE LOST AFTER NEXT INPUT DOWNLOAD",
    "*** CHANGES CAN BE DONE USING THE RESPECTIVE LINES IN scripts/start/prepare_and_run.R"
  )
  content <- c(modification_warning, "", "sets")
  # write iso set with nice formatting (10 countries per line)
  tmp <- lapply(split(map$CountryCode, ceiling(seq_along(map$CountryCode) / 10)), paste, collapse = ",")
  regions <- as.character(unique(map$RegionCode))
  content <- c(content, "", paste('   all_regi "all regions" /', paste(regions, collapse = ","), "/", sep = ""), "")
  # Creating sets for H12 subregions
  subsets <- remind2::toolRegionSubsets(map = cfg$regionmapping, singleMatches = TRUE, removeDuplicates = FALSE)
  if (grepl("regionmapping_21_EU11", cfg$regionmapping, fixed = TRUE)) { # add EU27 region group
    subsets <- c(subsets, list(
      "EU27" = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "ESW"), # EU27 (without Ireland)
      "NEU_UKI" = c("NES", "NEN", "UKI") # EU27 (without Ireland)
    ))
  }
  # ext_regi
  content <- c(content, paste('   ext_regi "extended regions list (includes subsets of H12 regions)"'))
  content <- c(content, "      /")
  content <- c(content, "        GLO,")
  content <- c(content, "        ", paste(paste0(names(subsets), "_regi"), collapse = ","), ",")
  content <- c(content, "        ", paste(regions, collapse = ","))
  content <- c(content, "      /")
  content <- c(content, " ")
  # regi_group
  content <- c(content, '   regi_group(ext_regi,all_regi) "region groups (regions that together corresponds to a H12 region)"')
  content <- c(content, "      /")
  content <- c(content, "      ", paste("GLO.(", paste(regions, collapse = ","), ")"))
  for (i in 1:length(subsets)) {
    content <- c(content, paste0("        ", paste(c(paste0(names(subsets)[i], "_regi"))), " .(", paste(subsets[[i]], collapse = ","), ")"))
  }
  content <- c(content, "      /")
  content <- c(content, " ")
  # iso countries set
  content <- c(content, '   iso "list of iso countries" /')
  content <- c(content, .tmp(map$CountryCode, suffix1 = ",", suffix2 = " /"), "")
  content <- c(content, '   regi2iso(all_regi,iso) "mapping regions to iso countries"', "      /")
  for (i in as.character(unique(map$RegionCode))) {
    content <- c(content, .tmp(map$CountryCode[map$RegionCode == i], prefix = paste0(i, " . ("), suffix1 = ")", suffix2 = ")"))
  }
  content <- c(content, "      /")
  content <- c(content, 'iso_regi "all iso countries and EU and greater China region" /  EUR,CHA,')
  content <- c(content, .tmp(map$CountryCode, suffix1 = ",", suffix2 = " /"), "")
  content <- c(content, '   map_iso_regi(iso_regi,all_regi) "mapping from iso countries to regions that represent country" ', "         /")
  for (i in regions[regions %in% c("EUR", "CHA", as.character(unique(map$CountryCode)))]) {
    content <- c(content, .tmp(i, prefix = paste0(i, " . "), suffix1 = "", suffix2 = ""))
  }
  content <- c(content, "      /", ";")
  gms::replace_in_file("core/sets.gms", content, "SETS", comment = "***")
}

# Function to create the levs.gms, fixings.gms, and margs.gms files, used in delay scenarios.
create_fixing_files <- function(cfg, input_ref_file = "input_ref.gdx") {

  # Start the clock.
  begin <- Sys.time()

  # Extract data from input_ref.gdx file and store in levs_margs_ref.gms.
  system(paste(
    "gdxdump",
    input_ref_file,
    "Format=gamsbas Delim=comma FilterDef=N Output=levs_margs_ref.gms"
  ))

  # Read data from levs_margs_ref.gms.
  ref_gdx_data <- suppressWarnings(readLines("levs_margs_ref.gms"))

  # Create fixing files.
  cat("\n")
  create_standard_fixings(cfg, ref_gdx_data)

  # Stop the clock.
  cat("Time it took to create the fixing files: ")
  manipulate_runtime <- Sys.time() - begin
  print(manipulate_runtime)
  cat("\n")

  # Delete file.
  file.remove("levs_margs_ref.gms")
}

# Function to create the levs.gms, fixings.gms, and margs.gms files, used in the standard (i.e. the non-macro
# stand-alone) delay scenarios.
create_standard_fixings <- function(cfg, ref_gdx_data) {

  # Declare empty lists to hold the strings for the 'manipulateFile' functions.
  full_manipulateThis <- NULL
  levs_manipulateThis <- NULL
  fixings_manipulateThis <- NULL
  margs_manipulateThis <- NULL

  str_years <- c()
  no_years <- (cfg$gms$cm_startyear - 2005) / 5

  # Write level values to file
  levs <- c()
  for (i in 1:no_years) {
    str_years[i] <- paste("L \\('", 2000 + i * 5, sep = "")
    levs <- c(levs, grep(str_years[i], ref_gdx_data, value = TRUE))
  }

  writeLines(levs, "levs.gms")

  # Replace fixing.gms with level values
  file.copy("levs.gms", "fixings.gms", overwrite = TRUE)

  fixings_manipulateThis <- c(fixings_manipulateThis, list(c(".L ", ".FX ")))
  # cb q_co2eq is only "static" equation to be active before cm_startyear, as multigasscen could be different from a scenario to another that is fixed on the first
  # cb therefore, vm_co2eq cannot be fixed, otherwise infeasibilities would result. vm_co2eq.M is meaningless, is never used in the code (a manipulateFile delete line command would be even better)
  #  manipulateFile("fixings.gms", list(c("vm_co2eq.FX ", "vm_co2eq.M ")))

  # Write marginal values to file
  margs <- c()
  str_years <- c()
  for (i in 1:no_years) {
    str_years[i] <- paste("M \\('", 2000 + i * 5, sep = "")
    margs <- c(margs, grep(str_years[i], ref_gdx_data, value = TRUE))
  }
  writeLines(margs, "margs.gms")
  # temporary fix so that you can use older gdx for fixings - will become obsolete in the future and can be deleted once the next variable name change is done
  margs_manipulateThis <- c(margs_manipulateThis, list(c("q_taxrev", "q21_taxrev")))
  # fixing for SPA runs based on ModPol input data
  margs_manipulateThis <- c(
    margs_manipulateThis,
    list(c("q41_emitrade_restr_mp.M", "!!q41_emitrade_restr_mp.M")),
    list(c("q41_emitrade_restr_mp2.M", "!!q41_emitrade_restr_mp2.M"))
  )

  # AJS this symbol is not known and crashes the run - is it depreciated? TODO
  levs_manipulateThis <- c(
    levs_manipulateThis,
    list(c("vm_pebiolc_price_base.L", "!!vm_pebiolc_price_base.L"))
  )

  # AJS filter out nash marginals in negishi case, as they would lead to a crash when trying to fix on them:
  if (cfg$gms$optimization == "negishi") {
    margs_manipulateThis <- c(margs_manipulateThis, list(c("q80_costAdjNash.M", "!!q80_costAdjNash.M")))
  }
  if (cfg$gms$subsidizeLearning == "off") {
    levs_manipulateThis <- c(
      levs_manipulateThis,
      list(c(
        "v22_costSubsidizeLearningForeign.L",
        "!!v22_costSubsidizeLearningForeign.L"
      ))
    )
    margs_manipulateThis <- c(
      margs_manipulateThis,
      list(c("q22_costSubsidizeLearning.M", "!!q22_costSubsidizeLearning.M")),
      list(c(
        "v22_costSubsidizeLearningForeign.M",
        "!!v22_costSubsidizeLearningForeign.M"
      )),
      list(c(
        "q22_costSubsidizeLearningForeign.M",
        "!!q22_costSubsidizeLearningForeign.M"
      ))
    )
    fixings_manipulateThis <- c(
      fixings_manipulateThis,
      list(c(
        "v22_costSubsidizeLearningForeign.FX",
        "!!v22_costSubsidizeLearningForeign.FX"
      ))
    )
  }

  # JH filter out negishi marginals in nash case, as they would lead to a crash when trying to fix on them:
  if (cfg$gms$optimization == "nash") {
    margs_manipulateThis <- c(
      margs_manipulateThis,
      list(c("q80_balTrade.M", "!!q80_balTrade.M")),
      list(c("q80_budget_helper.M", "!!q80_budget_helper.M"))
    )
  }
  # RP filter out module 40 techpol fixings
  if (cfg$gms$techpol == "none") {
    margs_manipulateThis <- c(
      margs_manipulateThis,
      list(c("q40_NewRenBound.M", "!!q40_NewRenBound.M")),
      list(c("q40_CoalBound.M", "!!q40_CoalBound.M")),
      list(c("q40_LowCarbonBound.M", "!!q40_LowCarbonBound.M")),
      list(c("q40_FE_RenShare.M", "!!q40_FE_RenShare.M")),
      list(c("q40_trp_bound.M", "!!q40_trp_bound.M")),
      list(c("q40_TechBound.M", "!!q40_TechBound.M")),
      list(c("q40_ElecBioBound.M", "!!q40_ElecBioBound.M")),
      list(c("q40_PEBound.M", "!!q40_PEBound.M")),
      list(c("q40_PEcoalBound.M", "!!q40_PEcoalBound.M")),
      list(c("q40_PEgasBound.M", "!!q40_PEgasBound.M")),
      list(c("q40_PElowcarbonBound.M", "!!q40_PElowcarbonBound.M")),
      list(c("q40_EV_share.M", "!!q40_EV_share.M")),
      list(c("q40_TrpEnergyRed.M", "!!q40_TrpEnergyRed.M")),
      list(c("q40_El_RenShare.M", "!!q40_El_RenShare.M")),
      list(c("q40_BioFuelBound.M", "!!q40_BioFuelBound.M"))
    )
  }

  if (cfg$gms$techpol == "NPi2018") {
    margs_manipulateThis <- c(
      margs_manipulateThis,
      list(c("q40_El_RenShare.M", "!!q40_El_RenShare.M")),
      list(c("q40_CoalBound.M", "!!q40_CoalBound.M"))
    )
  }

  levs_manipulateThis <- c(
    levs_manipulateThis,
    list(c("vm_shBioFe.L", "!!vm_shBioFe.L"))
  )
  fixings_manipulateThis <- c(
    fixings_manipulateThis,
    list(c("vm_shBioFe.FX", "!!vm_shBioFe.FX"))
  )
  margs_manipulateThis <- c(
    margs_manipulateThis,
    list(c("vm_shBioFe.M", "!!vm_shBioFe.M"))
  )

  # RP filter out regipol items
  if (grepl("off", cfg$gms$cm_implicitFE, ignore.case = T)) {
    margs_manipulateThis <- c(
      margs_manipulateThis,
      list(c("q47_implFETax.M", "!!q47_implFETax.M"))
    )
  }

  # Include fixings (levels) and marginals in full.gms at predefined position
  # in core/loop.gms.
  full_manipulateThis <- c(
    full_manipulateThis,
    list(c(
      "cb20150605readinpositionforlevelfile",
      paste("first offlisting inclusion of levs.gms so that level value can be accessed",
        "$offlisting",
        "$include \"levs.gms\";",
        "$onlisting",
        sep = "\n"
      )
    ))
  )
  full_manipulateThis <- c(full_manipulateThis, list(c(
    "cb20140305readinpositionforfinxingfiles",
    paste("offlisting inclusion of levs.gms, fixings.gms, and margs.gms",
      "$offlisting",
      "$include \"levs.gms\";",
      "$include \"fixings.gms\";",
      "$include \"margs.gms\";",
      "$onlisting",
      sep = "\n"
    )
  )))


  # Perform actual manipulation on levs.gms, fixings.gms, and margs.gms in
  # single, respective, parses of the texts.
  lucode2::manipulateFile("levs.gms", levs_manipulateThis)
  lucode2::manipulateFile("fixings.gms", fixings_manipulateThis)
  lucode2::manipulateFile("margs.gms", margs_manipulateThis)

  # Perform actual manipulation on full.gms, in single parse of the text.
  lucode2::manipulateFile("full.gms", full_manipulateThis)
}
