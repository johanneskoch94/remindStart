# Load and check configuration for consistency
loadAndCheckCfg <- function(cfgFile) {
  load(cfgFile)
  cat(crayon::green("Checking configuration settings.\n"))
  cfg <- gms::check_config(
    icfg = cfg,
    modulepath = file.path(cfg$remind_folder, "modules"),
    reference_file = file.path(cfg$remind_folder, "config/default.cfg"),
    settings_config = file.path(cfg$remind_folder, "config/settings_config.csv"),
    extras = c("remind_folder", "gitInfo", "regionscode", "waitForIds")
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

  # KK filter out module 39 CCU fixings
  if (cfg$gms$CCU == "off") {
    levs_manipulateThis <- c(
      levs_manipulateThis,
      list(c("v39_shSynTrans.L", "!!v39_shSynTrans.L")),
      list(c("v39_shSynGas.L", "!!v39_shSynGas.L"))
    )

    fixings_manipulateThis <- c(
      fixings_manipulateThis,
      list(c("v39_shSynTrans.FX", "!!v39_shSynTrans.FX")),
      list(c("v39_shSynGas.FX", "!!v39_shSynGas.FX"))
    )

    margs_manipulateThis <- c(
      margs_manipulateThis,
      list(c("v39_shSynTrans.M", "!!v39_shSynTrans.M")),
      list(c("v39_shSynGas.M", "!!v39_shSynGas.M")),
      list(c("q39_emiCCU.M", "!!q39_emiCCU.M")),
      list(c("q39_shSynTrans.M", "!!q39_shSynTrans.M")),
      list(c("q39_shSynGas.M", "!!q39_shSynGas.M"))
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

  # KK CDR module realizations
 if (cfg$gms$CDR == "DAC") {
   fixings_manipulateThis <- c(
     fixings_manipulateThis,
     list(c("v33_emiEW.FX", "!!v33_emiEW.FX")),
     list(c("v33_grindrock_onfield.FX", "!!v33_grindrock_onfield.FX")),
     list(c("v33_grindrock_onfield_tot.FX", "!!v33_grindrock_onfield_tot.FX"))
   )

   levs_manipulateThis <- c(
     levs_manipulateThis,
     list(c("v33_emiEW.L", "!!v33_emiEW.L")),
     list(c("v33_grindrock_onfield.L", "!!v33_grindrock_onfield.L")),
     list(c("v33_grindrock_onfield_tot.L", "!!v33_grindrock_onfield_tot.L"))
   )

   margs_manipulateThis <- c(
     margs_manipulateThis,
     list(c("v33_emiEW.M", "!!v33_emiEW.M")),
     list(c("v33_grindrock_onfield.M", "!!v33_grindrock_onfield.M")),
     list(c("v33_grindrock_onfield_tot.M", "!!v33_grindrock_onfield_tot.M")),
     list(c("q33_capconst_grindrock.M", "!!q33_capconst_grindrock.M")),
     list(c("q33_grindrock_onfield_tot.M", "!!q33_grindrock_onfield_tot.M")),
     list(c("q33_omcosts.M", "!!q33_omcosts.M")),
     list(c("q33_potential.M", "!!q33_potential.M")),
     list(c("q33_emiEW.M", "!!q33_emiEW.M")),
     list(c("q33_LimEmiEW.M", "!!q33_LimEmiEW.M"))
   )
 }

 if (cfg$gms$CDR == "weathering") {
   fixings_manipulateThis <- c(
     fixings_manipulateThis,
     list(c("v33_emiDAC.FX", "!!v33_emiDAC.FX")),
     list(c("v33_DacFEdemand_el.FX", "!!v33_DacFEdemand_el.FX")),
     list(c("v33_DacFEdemand_heat.FX", "!!v33_DacFEdemand_heat.FX"))
   )

   levs_manipulateThis <- c(
     levs_manipulateThis,
     list(c("v33_emiDAC.L", "!!v33_emiDAC.L")),
     list(c("v33_DacFEdemand_el.L", "!!v33_DacFEdemand_el.L")),
     list(c("v33_DacFEdemand_heat.L", "!!v33_DacFEdemand_heat.L"))
   )

   margs_manipulateThis <- c(
     margs_manipulateThis,
     list(c("v33_emiDAC.M", "!!v33_emiDAC.")),
     list(c("v33_DacFEdemand_el.M", "!!v33_DacFEdemand_el.M")),
     list(c("v33_DacFEdemand_heat.M", "!!v33_DacFEdemand_heat.M")),
     list(c("q33_DacFEdemand_heat.M", "!!q33_DacFEdemand_heat.M")),
     list(c("q33_DacFEdemand_el.M", "!!q33_DacFEdemand_el.M")),
     list(c("q33_capconst_dac.M", "!!q33_capconst_dac.M")),
     list(c("q33_ccsbal.M", "!!q33_ccsbal.M")),
     list(c("q33_H2bio_lim.M", "!!q33_H2bio_lim.M"))
   )
 }

 if (cfg$gms$CDR == "off") {
   fixings_manipulateThis <- c(
     fixings_manipulateThis,
     list(c("v33_emiDAC.FX", "!!v33_emiDAC.FX")),
     list(c("v33_emiEW.FX", "!!v33_emiEW.FX")),
     list(c("v33_DacFEdemand_el.FX", "!!v33_DacFEdemand_el.FX")),
     list(c("v33_DacFEdemand_heat.FX", "!!v33_DacFEdemand_heat.FX")),
     list(c("v33_grindrock_onfield.FX", "!!v33_grindrock_onfield.FX")),
     list(c("v33_grindrock_onfield_tot.FX", "!!v33_grindrock_onfield_tot.FX"))
   )

   levs_manipulateThis <- c(
     levs_manipulateThis,
     list(c("v33_emiDAC.L", "!!v33_emiDAC.L")),
     list(c("v33_emiEW.L", "!!v33_emiEW.L")),
     list(c("v33_DacFEdemand_el.L", "!!v33_DacFEdemand_el.L")),
     list(c("v33_DacFEdemand_heat.L", "!!v33_DacFEdemand_heat.L")),
     list(c("v33_grindrock_onfield.L", "!!v33_grindrock_onfield.L")),
     list(c("v33_grindrock_onfield_tot.L", "!!v33_grindrock_onfield_tot.L"))
   )

   margs_manipulateThis <- c(
     margs_manipulateThis,
     list(c("v33_emiDAC.M", "!!v33_emiDAC.M")),
     list(c("v33_emiEW.M", "!!v33_emiEW.M")),
     list(c("v33_grindrock_onfield.M", "!!v33_grindrock_onfield.M")),
     list(c("v33_grindrock_onfield_tot.M", "!!v33_grindrock_onfield_tot.M")),
     list(c("v33_DacFEdemand_el.M", "!!v33_DacFEdemand_el.M")),
     list(c("v33_DacFEdemand_heat.M", "!!v33_DacFEdemand_heat.M")),
     list(c("q33_capconst_grindrock.M", "!!q33_capconst_grindrock.M")),
     list(c("q33_grindrock_onfield_tot.M", "!!q33_grindrock_onfield_tot.M")),
     list(c("q33_omcosts.M", "!!q33_omcosts.M")),
     list(c("q33_potential.M", "!!q33_potential.M")),
     list(c("q33_emiEW.M", "!!q33_emiEW.M")),
     list(c("q33_LimEmiEW.M", "!!q33_LimEmiEW.M")),
     list(c("q33_DacFEdemand_heat.M", "!!q33_DacFEdemand_heat.M")),
     list(c("q33_DacFEdemand_el.M", "!!q33_DacFEdemand_el.M")),
     list(c("q33_capconst_dac.M", "!!q33_capconst_dac.M")),
     list(c("q33_ccsbal.M", "!!q33_ccsbal.M")),
     list(c("q33_H2bio_lim.M", "!!q33_H2bio_lim.M")),
     list(c("q33_demFeCDR.M", "!!q33_demFeCDR.M")),
     list(c("q33_emicdrregi.M", "!!q33_emicdrregi.M")),
     list(c("q33_otherFEdemand.M", "!!q33_otherFEdemand.M"))
   )
 }
 # end of CDR module realizations

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
   list(c("vm_shBioFe.M", "!!vm_shBioFe.M")),
   list(c("q39_EqualSecShare_BioSyn.M", "!!q39_EqualSecShare_BioSyn.M"))
 )

 # OR: renamed for sectoral taxation
 levs_manipulateThis <- c(
   levs_manipulateThis,
   list(c("vm_emiCO2_sector.L", "vm_emiCO2Sector.L")),
   list(c("v21_taxrevCO2_sector.L", "v21_taxrevCO2Sector.L"))
 )
 margs_manipulateThis <- c(
   margs_manipulateThis,
   list(c("vm_emiCO2_sector.M", "vm_emiCO2Sector.M")),
   list(c("v21_taxrevCO2_sector.M", "v21_taxrevCO2Sector.M")),
   list(c("q_emiCO2_sector.M", "q_emiCO2Sector.M")),
   list(c("q21_taxrevCO2_sector.M", "q21_taxrevCO2Sector.M"))
 )
 fixings_manipulateThis <- c(
   fixings_manipulateThis,
   list(c("vm_emiCO2_sector.FX", "vm_emiCO2Sector.FX")),
   list(c("v21_taxrevCO2_sector.FX", "v21_taxrevCO2Sector.FX"))
 )

 # renamed because of https://github.com/remindmodel/remind/pull/796
 manipulate_tradesets <- c(
   list(c("'gas_pipe'", "'pipe_gas'")),
   list(c("'lng_liq'", "'termX_lng'")),
   list(c("'lng_gas'", "'termX_lng'")),
   list(c("'lng_ves'", "'vess_lng'")),
   list(c("'coal_ves'", "'vess_coal'")),
   list(c("vm_budgetTradeX", "!! vm_budgetTradeX")),
   list(c("vm_budgetTradeM", "!! vm_budgetTradeM"))
 )
 levs_manipulateThis <- c(levs_manipulateThis, manipulate_tradesets)
 margs_manipulateThis <- c(margs_manipulateThis, manipulate_tradesets)
 fixings_manipulateThis <- c(fixings_manipulateThis, manipulate_tradesets)

 # because of https://github.com/remindmodel/remind/pull/800
 if (cfg$gms$cm_transpGDPscale != "on") {
   levs_manipulateThis <- c(levs_manipulateThis, list(c("q35_transGDPshare.M", "!! q35_transGDPshare.M")))
   margs_manipulateThis <- c(margs_manipulateThis, list(c("q35_transGDPshare.M", "!! q35_transGDPshare.M")))
   fixings_manipulateThis <- c(fixings_manipulateThis, list(c("q35_transGDPshare.M", "!! q35_transGDPshare.M")))
 }

 # renamed because of https://github.com/remindmodel/remind/pull/848, 1066
 levs_manipulateThis <- c(
   levs_manipulateThis,
   list(c("vm_forcOs.L", "!!vm_forcOs.L")),
   list(c("v32_shSeEl.L", "!!v32_shSeEl.L"))
 )
 margs_manipulateThis <- c(
   margs_manipulateThis,
   list(c("vm_forcOs.M", "!!vm_forcOs.M")),
   list(c("v32_shSeEl.M", "!!v32_shSeEl.M"))
 )
 fixings_manipulateThis <- c(
   fixings_manipulateThis,
   list(c("vm_forcOs.FX", "!!vm_forcOs.FX")),
   list(c("v32_shSeEl.FX", "!!v32_shSeEl.FX"))
 )

 # filter out deprecated regipol items
 levs_manipulateThis <- c(
   levs_manipulateThis,
   list(c("v47_emiTarget.L", "!!v47_emiTarget.L")),
   list(c("v47_emiTargetMkt.L", "!!v47_emiTargetMkt.L")),
   list(c("vm_taxrevimplEnergyBoundTax.L", "!!vm_taxrevimplEnergyBoundTax.L"))
 )
 margs_manipulateThis <- c(
   margs_manipulateThis,
   list(c("v47_emiTarget.M", "!!v47_emiTarget.M")),
   list(c("v47_emiTargetMkt.M", "!!v47_emiTargetMkt.M")),
   list(c("q47_implFETax.M", "!!q47_implFETax.M")),
   list(c("q47_emiTarget_mkt_netCO2.M", "!!q47_emiTarget_mkt_netCO2.M")),
   list(c("q47_emiTarget_mkt_netGHG.M", "!!q47_emiTarget_mkt_netGHG.M")),
   list(c("q47_emiTarget_netCO2.M", "!!q47_emiTarget_netCO2.M")),
   list(c("q47_emiTarget_netCO2_noBunkers.M", "!!q47_emiTarget_netCO2_noBunkers.M")),
   list(c("q47_emiTarget_netCO2_noLULUCF_noBunkers.M", "!!q47_emiTarget_netCO2_noLULUCF_noBunkers.M")),
   list(c("q47_emiTarget_netGHG.M", "!!q47_emiTarget_netGHG.M")),
   list(c("q47_emiTarget_netGHG_noBunkers.M", "!!q47_emiTarget_netGHG_noBunkers.M")),
   list(c("q47_emiTarget_netGHG_noLULUCF_noBunkers.M", "!!q47_emiTarget_netGHG_noLULUCF_noBunkers.M")),
   list(c("q47_emiTarget_netGHG_LULUCFGrassi_noBunkers.M", "!!q47_emiTarget_netGHG_LULUCFGrassi_noBunkers.M")),
   list(c("q47_emiTarget_grossEnCO2.M", "!!q47_emiTarget_grossEnCO2.M")),
   list(c("q47_emiTarget_mkt_netCO2.M", "!!q47_emiTarget_mkt_netCO2.M")),
   list(c("q47_emiTarget_mkt_netCO2_noBunkers.M", "!!q47_emiTarget_mkt_netCO2_noBunkers.M")),
   list(c("q47_emiTarget_mkt_netCO2_noLULUCF_noBunkers.M", "!!q47_emiTarget_mkt_netCO2_noLULUCF_noBunkers.M")),
   list(c("q47_emiTarget_mkt_netGHG.M", "!!q47_emiTarget_mkt_netGHG.M")),
   list(c("q47_emiTarget_mkt_netGHG_noBunkers.M", "!!q47_emiTarget_mkt_netGHG_noBunkers.M")),
   list(c("q47_emiTarget_mkt_netGHG_noLULUCF_noBunkers.M", "!!q47_emiTarget_mkt_netGHG_noLULUCF_noBunkers.M")),
   list(c("q47_emiTarget_mkt_netGHG_LULUCFGrassi_noBunkers.M", "!!q47_emiTarget_mkt_netGHG_LULUCFGrassi_noBunkers.M")),
   list(c("qm_balFeAfterTax.M", "!!qm_balFeAfterTax.M")),
   list(c("q47_implicitQttyTargetTax.M", "!!q47_implicitQttyTargetTax.M")),
   list(c("q47_implEnergyBoundTax.M", "!!q47_implEnergyBoundTax.M")),
   list(c("vm_taxrevimplEnergyBoundTax.M", "!!vm_taxrevimplEnergyBoundTax.M"))
 )

 fixings_manipulateThis <- c(
   fixings_manipulateThis,
   list(c("v47_emiTarget.FX", "!!v47_emiTarget.FX")),
   list(c("v47_emiTargetMkt.FX", "!!v47_emiTargetMkt.FX")),
   list(c("vm_taxrevimplEnergyBoundTax.FX", "!!vm_taxrevimplEnergyBoundTax.FX"))
 )

 # renamed because of https://github.com/remindmodel/remind/pull/1106
 levs_manipulateThis <- c(
   levs_manipulateThis,
   list(c("v21_taxrevBioImport.L", "!!v21_taxrevBioImport.L"))
 )
 margs_manipulateThis <- c(
   margs_manipulateThis,
   list(c("v21_taxrevBioImport.M", "!!v21_taxrevBioImport.M")),
   list(c("q21_taxrevBioImport.M", "!!q21_taxrevBioImport.M")),
   list(c("q30_limitProdtoHist.M", "!!q30_limitProdtoHist.M"))
 )
 fixings_manipulateThis <- c(
   fixings_manipulateThis,
   list(c("v21_taxrevBioImport.FX", "!!v21_taxrevBioImport.FX"))
 )

 # renamed because of https://github.com/remindmodel/remind/pull/1128
 levs_manipulateThis <- c(
   levs_manipulateThis,
   list(c("v_emiTeDetailMkt.L", "!!v_emiTeDetailMkt.L")),
   list(c("v_emiTeMkt.L", "!!v_emiTeMkt.L"))
 )
 margs_manipulateThis <- c(
   margs_manipulateThis,
   list(c("v_emiTeDetailMkt.M", "!!v_emiTeDetailMkt.M")),
   list(c("v_emiTeMkt.M", "!!v_emiTeMkt.M"))
 )
 fixings_manipulateThis <- c(
   fixings_manipulateThis,
   list(c("v_emiTeDetailMkt.FX", "!!v_emiTeDetailMkt.FX")),
   list(c("v_emiTeMkt.FX", "!!v_emiTeMkt.FX"))
 )
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
