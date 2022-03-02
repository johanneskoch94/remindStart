# Load and check configuration for consistency
loadAndCheckCfg <- function(cfgFile) {
  load(cfgFile)
  cat(crayon::green("Checking configuration settings.\n"))
  cfg <- gms::check_config(
    icfg = cfg,
    modulepath = file.path(cfg$remind_folder, "modules"),
    reference_file = file.path(cfg$remind_folder, "config/default.cfg"),
    settings_config = file.path(cfg$remind_folder, "config/settings_config.csv"),
    extras = c("remind_folder", "gitInfo")
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


prepareRemind <- function(cfg, runCopy) {
  # Work in runCopy directory
  withr::local_dir(runCopy)


  # Copy MAGICC
  if (!file.exists(cfg$magicc_template) & file.exists(path.expand(Sys.getenv("MAGICC")))) {
    cfg$magicc_template <- path.expand(Sys.getenv("MAGICC"))
  }

  if (file.exists(cfg$magicc_template)) {
    cat("Copying MAGICC files from", cfg$magicc_template, "to results folder\n")
    system(paste0("cp -rp ", cfg$magicc_template, " ", cfg$results_folder))
    system(paste0("cp -rp core/magicc/* ", cfg$results_folder, "/magicc/"))
  } else {
    cat("Could not copy", cfg$magicc_template, "because it does not exist\n")
  }

  ###########################################################
  ### PROCESSING INPUT DATA ###################### START ####
  ###########################################################
  # adjust GDPpcScen based on GDPscen
  cfg$gms$c_GDPpcScen <- gsub("gdp_", "", cfg$gms$cm_GDPscen)

  # Create input file with exogenous CO2 tax using the CO2 price from another run
  if (!is.null(cfg$gms$carbonprice) &&
      (cfg$gms$carbonprice == "exogenous") &&
      (!is.na(cfg$files2export$start["input_carbonprice.gdx"]))) {
    cat(paste(
      "\n Run scripts/input/create_input_for_45_carbonprice_exogenous.R to create",
      "input file with exogenous CO2 tax from another run.\n"
    ))
    create_input_for_45_carbonprice_exogenous(as.character(cfg$files2export$start["input_carbonprice.gdx"]))
  }

  # Calculate CES configuration string
  cfg$gms$cm_CES_configuration <- paste0(
    "indu_", cfg$gms$industry, "-",
    "buil_", cfg$gms$buildings, "-",
    "tran_", cfg$gms$transport, "-",
    "POP_", cfg$gms$cm_POPscen, "-",
    "GDP_", cfg$gms$cm_GDPscen, "-",
    "En_", cfg$gms$cm_demScen, "-",
    "Kap_", cfg$gms$capitalMarket, "-",
    if (cfg$gms$cm_calibration_string == "off") "" else paste0(cfg$gms$cm_calibration_string, "-"),
    "Reg_", madrat::regionscode(cfg$regionmapping)
  )

  # write name of corresponding CES file to datainput.gms
  gms::replace_in_file(
    file = "./modules/29_CES_parameters/load/datainput.gms",
    content = paste0('$include "./modules/29_CES_parameters/load/input/', cfg$gms$cm_CES_configuration, '.inc"'),
    subject = "CES INPUT"
  )

  # If a path to a MAgPIE report is supplied use it as REMIND intput (used for REMIND-MAgPIE coupling)
  # ATTENTION: modifying gms files
  if (!is.null(cfg$pathToMagpieReport)) {
    getReportData(path_to_report = cfg$pathToMagpieReport, inputpath_mag = cfg$gms$biomass, inputpath_acc = cfg$gms$agCosts)
  }

  # Update module paths in GAMS code
  gms::update_modules_embedding()

  # Check all setglobal settings for consistency
  gms::settingsCheck()

  # configure main model gms file (cfg$model) based on settings of cfg file
  cfg$gms$c_expname <- cfg$title

  # run main.gms if not further specified
  if (is.null(cfg$model)) cfg$model <- "main.gms"
  lucode2::manipulateConfig(cfg$model, cfg$gms)


  ############ download and distribute input data ########
  # check wheather the regional resolution and input data revision are outdated and update data if needed
  input_old <- if (file.exists("input/source_files.log")) readLines("input/source_files.log")[1] else "no_data"

  input_new <- c(
    paste0("rev", cfg$inputRevision, "_", madrat::regionscode(cfg$regionmapping), "_", tolower(cfg$model_name), ".tgz"),
    paste0("rev", cfg$inputRevision, "_", madrat::regionscode(cfg$regionmapping), "_", tolower(cfg$validationmodel_name), ".tgz"),
    paste0("CESparametersAndGDX_", cfg$CESandGDXversion, ".tgz")
  )

  if (!setequal(input_new, input_old) | cfg$force_download) {
    if (is.null(cfg$repositories)) {
      rlang::abort("No remind_repos option defined.")
    }
    cat("Your input data is outdated or in a different regional resolution. New data will be downloaded and distributed.\n")
    gms::download_distribute(
      files = input_new,
      repositories = cfg$repositories, # defined in your local .Rprofile or on the cluster /p/projects/rd3mod/R/.Rprofile
      modelfolder = ".",
      debug = FALSE
    )
  }

  ############ update information ########################
  # update_info, which regional resolution and input data revision in cfg$model
  update_info(cfg, madrat::regionscode(cfg$regionmapping), cfg$revision)
  # update_sets, which is updating the region-depending sets in core/sets.gms
  #-- load new mapping information
  map <- utils::read.csv(cfg$regionmapping, sep = ";")
  update_sets(cfg, map)

  ########################################################
  ### PROCESSING INPUT DATA ###################### END ###
  ########################################################

  ### ADD MODULE INFO IN SETS  ############# START #######
  content <- NULL
  modification_warning <- c(
    "*** THIS CODE IS CREATED AUTOMATICALLY, DO NOT MODIFY THESE LINES DIRECTLY",
    "*** ANY DIRECT MODIFICATION WILL BE LOST AFTER NEXT MODEL START",
    "*** CHANGES CAN BE DONE USING THE RESPECTIVE LINES IN scripts/start_functions.R"
  )
  content <- c(modification_warning, "", "sets")
  content <- c(content, "", '       modules "all the available modules"')
  content <- c(content, "       /", paste0("       ", gms::getModules("modules/")[, "name"]), "       /")
  content <- c(content, "", 'module2realisation(modules,*) "mapping of modules and active realisations" /')
  content <- c(content, paste0("       ", gms::getModules("modules/")[, "name"], " . %", gms::getModules("modules/")[, "name"], "%"))
  content <- c(content, "      /", ";")
  gms::replace_in_file("core/sets.gms", content, "MODULES", comment = "***")
  ### ADD MODULE INFO IN SETS  ############# END #########

  # Copy right gdx file to the output folder
  gdx_name <- paste0("config/gdx-files/", cfg$gms$cm_CES_configuration, ".gdx")
  if (0 != system(paste("cp", gdx_name, file.path(cfg$results_folder, "input.gdx")))) {
    stop("Could not copy gdx file ", gdx_name)
  }

  # Choose which conopt files to copy
  cfg$files2export$start <- sub("conopt3", cfg$gms$cm_conoptv, cfg$files2export$start)

  # Copy important files into output_folder (before REMIND execution)
  copyFromList(cfg$files2export$start, cfg$results_folder)

  # Save configuration
  save(cfg, file = file.path(cfg$results_folder, "config.Rdata"))
  yaml::write_yaml(cfg, file = file.path(cfg$results_folder, "cfg.txt"))

  # Merge GAMS files
  cat("\n################\nCreating full.gms ...")
  gms::singleGAMSfile(mainfile = cfg$model, output = file.path(cfg$results_folder, "full.gms"))
  cat(" done.\n################\n\n\n")

  # Collect run statistics (will be saved to central database in submit.R)
  lucode2::runstatistics(
    file = paste0(cfg$results_folder, "/runstatistics.rda"),
    user = Sys.info()[["user"]],
    date = Sys.time(),
    version_management = "git",
    revision = cfg$gitInfo$commit,
    # revision_date = try(as.POSIXct(system("git show -s --format=%ci", intern=TRUE), silent=TRUE)),
    status = cfg$gitInfo$status
  )
}

prepareMagicc <- function(resultsDir, cm_magicc_config) {
  # Work in base_copy directory
  withr::local_dir(resultsDir)

  try_copy <- try(file.copy("magicc/run_magicc.R", "run_magicc.R"))
  try_copy <- try(file.copy("magicc/run_magicc_temperatureImpulseResponse.R", "run_magicc_temperatureImpulseResponse.R"))
  try_copy <- try(file.copy("magicc/read_DAT_TOTAL_ANTHRO_RF.R", "read_DAT_TOTAL_ANTHRO_RF.R"))
  try_copy <- try(file.copy("magicc/read_DAT_SURFACE_TEMP.R", "read_DAT_SURFACE_TEMP.R"))

  # Set MAGCFG file
  magcfgFile <- paste0("./magicc/MAGCFG_STORE/", "MAGCFG_USER_", toupper(cm_magicc_config), ".CFG")
  if (!file.exists(magcfgFile)) {
    warning(paste("ERROR in MAGGICC configuration: Could not find file ", magcfgFile))
  } else {
    file.copy(magcfgFile, "./magicc/MAGCFG_USER.CFG")
  }
}


getReportData <- function(path_to_report, inputpath_mag = "magpie", inputpath_acc = "costs") {
  .bioenergy_price <- function(mag){
    notGLO <- getRegions(mag)[!(getRegions(mag)=="GLO")]
    if("Demand|Bioenergy|++|2nd generation (EJ/yr)" %in% getNames(mag)) {
      # MAgPIE 4
      out <- mag[,,"Prices|Bioenergy (US$05/GJ)"]*0.0315576 # with transformation factor from US$2005/GJ to US$2005/Wa
    } else {
      # MAgPIE 3
      out <- mag[,,"Price|Primary Energy|Biomass (US$2005/GJ)"]*0.0315576 # with transformation factor from US$2005/GJ to US$2005/Wa
    }
    out["JPN",is.na(out["JPN",,]),] <- 0
    dimnames(out)[[3]] <- NULL #Delete variable name to prevent it from being written into output file
    write.magpie(out[notGLO,,],paste0("./modules/30_biomass/",inputpath_mag,"/input/p30_pebiolc_pricemag_coupling.csv"),file_type="csvr")
  }
  .bioenergy_costs <- function(mag){
    notGLO <- getRegions(mag)[!(getRegions(mag)=="GLO")]
    if ("Production Cost|Agriculture|Biomass|Energy Crops (million US$2005/yr)" %in% getNames(mag)) {
      out <- mag[,,"Production Cost|Agriculture|Biomass|Energy Crops (million US$2005/yr)"]/1000/1000 # with transformation factor from 10E6 US$2005 to 10E12 US$2005
    }
    else {
      # in old MAgPIE reports the unit is reported to be "billion", however the values are in million
      out <- mag[,,"Production Cost|Agriculture|Biomass|Energy Crops (billion US$2005/yr)"]/1000/1000 # with transformation factor from 10E6 US$2005 to 10E12 US$2005
    }
    out["JPN",is.na(out["JPN",,]),] <- 0
    dimnames(out)[[3]] <- NULL
    write.magpie(out[notGLO,,],paste0("./modules/30_biomass/",inputpath_mag,"/input/p30_pebiolc_costsmag.csv"),file_type="csvr")
  }
  .bioenergy_production <- function(mag){
    notGLO <- getRegions(mag)[!(getRegions(mag)=="GLO")]
    if("Demand|Bioenergy|2nd generation|++|Bioenergy crops (EJ/yr)" %in% getNames(mag)) {
      # MAgPIE 4
      out <- mag[,,"Demand|Bioenergy|2nd generation|++|Bioenergy crops (EJ/yr)"]/31.536 # EJ to TWa
    } else {
      # MAgPIE 3
      out <- mag[,,"Primary Energy Production|Biomass|Energy Crops (EJ/yr)"]/31.536 # EJ to TWa
    }
    out[which(out<0)] <- 0 # set negative values to zero since they cause errors in GMAS power function
    out["JPN",is.na(out["JPN",,]),] <- 0
    dimnames(out)[[3]] <- NULL
    write.magpie(out[notGLO,,],paste0("./modules/30_biomass/",inputpath_mag,"/input/p30_pebiolc_demandmag_coupling.csv"),file_type="csvr")
  }
  .emissions_mac <- function(mag) {
    # define three columns of dataframe:
    #   emirem (remind emission names)
    #   emimag (magpie emission names)
    #   factor_mag2rem (factor for converting magpie to remind emissions)
    #   1/1000*28/44, # kt N2O/yr -> Mt N2O/yr -> Mt N/yr
    #   28/44,        # Tg N2O/yr =  Mt N2O/yr -> Mt N/yr
    #   1/1000*12/44, # Mt CO2/yr -> Gt CO2/yr -> Gt C/yr
    map <- data.frame(emirem=NULL,emimag=NULL,factor_mag2rem=NULL,stringsAsFactors=FALSE)
    if("Emissions|N2O|Land|Agriculture|+|Animal Waste Management (Mt N2O/yr)" %in% getNames(mag)) {
      # MAgPIE 4
      map <- rbind(map,data.frame(emimag="Emissions|CO2|Land (Mt CO2/yr)",                                                                 emirem="co2luc",    factor_mag2rem=1/1000*12/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land|Agriculture|+|Animal Waste Management (Mt N2O/yr)",                           emirem="n2oanwstm", factor_mag2rem=28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt N2O/yr)",          emirem="n2ofertin", factor_mag2rem=28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt N2O/yr)",    emirem="n2oanwstc", factor_mag2rem=28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt N2O/yr)",         emirem="n2ofertcr", factor_mag2rem=28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt N2O/yr)",       emirem="n2ofertsom",factor_mag2rem=28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land|Agriculture|Agricultural Soils|+|Pasture (Mt N2O/yr)",                        emirem="n2oanwstp", factor_mag2rem=28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)",                                              emirem="ch4rice",   factor_mag2rem=1,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)",                           emirem="ch4anmlwst",factor_mag2rem=1,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)",                              emirem="ch4animals",factor_mag2rem=1,stringsAsFactors=FALSE))
    } else {
      # MAgPIE 3
      map <- rbind(map,data.frame(emimag="Emissions|CO2|Land Use (Mt CO2/yr)",                                                        emirem="co2luc",    factor_mag2rem=1/1000*12/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Agriculture|AWM (kt N2O/yr)",                                        emirem="n2oanwstm", factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Agriculture|Cropland Soils|Inorganic Fertilizers (kt N2O/yr)",       emirem="n2ofertin", factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Agriculture|Cropland Soils|Manure applied to Croplands (kt N2O/yr)", emirem="n2oanwstc", factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Agriculture|Cropland Soils|Decay of crop residues (kt N2O/yr)",      emirem="n2ofertcr", factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Agriculture|Cropland Soils|Soil organic matter loss (kt N2O/yr)",    emirem="n2ofertsom",factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Agriculture|Cropland Soils|Lower N2O emissions of rice (kt N2O/yr)", emirem="n2ofertrb", factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Agriculture|Pasture (kt N2O/yr)",                                    emirem="n2oanwstp", factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Biomass Burning|Forest Burning (kt N2O/yr)",                         emirem="n2oforest", factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Biomass Burning|Savannah Burning (kt N2O/yr)",                       emirem="n2osavan",  factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|N2O|Land Use|Biomass Burning|Agricultural Waste Burning (kt N2O/yr)",             emirem="n2oagwaste",factor_mag2rem=1/1000*28/44,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land Use|Agriculture|Rice (Mt CH4/yr)",                                       emirem="ch4rice",   factor_mag2rem=1,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land Use|Agriculture|AWM (Mt CH4/yr)",                                        emirem="ch4anmlwst",factor_mag2rem=1,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land Use|Agriculture|Enteric Fermentation (Mt CH4/yr)",                       emirem="ch4animals",factor_mag2rem=1,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land Use|Biomass Burning|Forest Burning (Mt CH4/yr)",                         emirem="ch4forest", factor_mag2rem=1,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land Use|Biomass Burning|Savannah Burning (Mt CH4/yr)",                       emirem="ch4savan",  factor_mag2rem=1,stringsAsFactors=FALSE))
      map <- rbind(map,data.frame(emimag="Emissions|CH4|Land Use|Biomass Burning|Agricultural Waste Burning (Mt CH4/yr)",             emirem="ch4agwaste",factor_mag2rem=1,stringsAsFactors=FALSE))
    }

    # Read data from MAgPIE report and convert to REMIND data, collect in 'out' object
    out<-NULL
    for (i in 1:nrow(map)) {
      tmp<-magclass::setNames(mag[,,map[i,]$emimag],map[i,]$emirem)
      tmp<-tmp*map[i,]$factor_mag2rem
      #tmp["JPN",is.na(tmp["JPN",,]),] <- 0
      # preliminary fix 20160111
      #cat("Preliminary quick fix: filtering out NAs for all and negative values for almost all landuse emissions except for co2luc and n2ofertrb\n")
      #tmp[is.na(tmp)] <- 0
      # preliminary 20160114: filter out negative values except for co2luc and n2ofertrb
      #if (map[i,]$emirem!="co2luc" &&  map[i,]$emirem!="n2ofertrb") {
      # tmp[tmp<0] <- 0
      #}
      out<-mbind(out,tmp)
    }

    # Write REMIND input file
    notGLO   <- getRegions(mag)[!(getRegions(mag)=="GLO")]
    filename <- paste0("./core/input/f_macBaseMagpie_coupling.cs4r")
    write.magpie(out[notGLO],filename)
    write(paste0("*** EOF ",filename," ***"),file=filename,append=TRUE)
  }
  .agriculture_costs <- function(mag){
    notGLO <- getRegions(mag)[!(getRegions(mag)=="GLO")]
    out <- mag[,,"Costs|MainSolve w/o GHG Emissions (million US$05/yr)"]/1000/1000 # with transformation factor from 10E6 US$2005 to 10E12 US$2005
    out["JPN",is.na(out["JPN",,]),] <- 0
    dimnames(out)[[3]] <- NULL #Delete variable name to prevent it from being written into output file
    write.magpie(out[notGLO,,],paste0("./modules/26_agCosts/",inputpath_acc,"/input/p26_totLUcost_coupling.csv"),file_type="csvr")
  }
  .agriculture_tradebal <- function(mag){
    notGLO <- getRegions(mag)[!(getRegions(mag)=="GLO")]
    out <- mag[,,"Trade|Agriculture|Trade Balance (billion US$2005/yr)"]/1000 # with transformation factor from 10E9 US$2005 to 10E12 US$2005
    out["JPN",is.na(out["JPN",,]),] <- 0
    dimnames(out)[[3]] <- NULL
    write.magpie(out[notGLO,,],paste0("./modules/26_agCosts/",inputpath_acc,"/input/trade_bal_reg.rem.csv"),file_type="csvr")
  }

  rep <- read.report(path_to_report,as.list=FALSE)
  if (length(getNames(rep,dim="scenario"))!=1) stop("getReportData: MAgPIE data contains more or less than 1 scenario.")
  rep <- collapseNames(rep) # get rid of scenrio and model dimension if they exist
  years <- 2000+5*(1:30)
  mag <- time_interpolate(rep,years)
  .bioenergy_price(mag)
  #.bioenergy_costs(mag) # Obsolete since bioenergy costs are not calculated by MAgPIE anymore but by integrating the supplycurve
  .bioenergy_production(mag)
  .emissions_mac(mag)
  .agriculture_costs(mag)
  # need to be updated to MAgPIE 4 interface
  #.agriculture_tradebal(mag)
}

update_info <- function(cfg, regionscode, revision) {
  subject <- 'VERSION INFO'
  content <- c('',
               paste('Regionscode:',regionscode),
               '',
               paste('Input data revision:',revision),
               '',
               paste('Last modification (input data):',date()),
               '')
  gms::replace_in_file(cfg$model, paste('*', content), subject)
}

update_sets <- function(cfg, map) {
  .tmp <- function(x,prefix="", suffix1="", suffix2=" /", collapse=",", n=10) {
    content <- NULL
    tmp <- lapply(split(x, ceiling(seq_along(x)/n)),paste,collapse=collapse)
    end <- suffix1
    for(i in 1:length(tmp)) {
      if(i==length(tmp)) end <- suffix2
      content <- c(content,paste0('       ',prefix,tmp[[i]],end))
    }
    return(content)
  }
  modification_warning <- c(
    '*** THIS CODE IS CREATED AUTOMATICALLY, DO NOT MODIFY THESE LINES DIRECTLY',
    '*** ANY DIRECT MODIFICATION WILL BE LOST AFTER NEXT INPUT DOWNLOAD',
    '*** CHANGES CAN BE DONE USING THE RESPECTIVE LINES IN scripts/start/prepare_and_run.R')
  content <- c(modification_warning,'','sets')
  # write iso set with nice formatting (10 countries per line)
  tmp <- lapply(split(map$CountryCode, ceiling(seq_along(map$CountryCode)/10)),paste,collapse=",")
  regions <- as.character(unique(map$RegionCode))
  content <- c(content, '',paste('   all_regi "all regions" /',paste(regions,collapse=','),'/',sep=''),'')
  # Creating sets for H12 subregions
  subsets <- remind2::toolRegionSubsets(map=cfg$regionmapping,singleMatches=TRUE,removeDuplicates=FALSE)
  if(grepl("regionmapping_21_EU11", cfg$regionmapping, fixed = TRUE)){ #add EU27 region group
    subsets <- c(subsets,list(
      "EU27"=c("ENC","EWN","ECS","ESC","ECE","FRA","DEU","ESW"), #EU27 (without Ireland)
      "NEU_UKI"=c("NES", "NEN", "UKI") #EU27 (without Ireland)
    ) )
  }
  # ext_regi
  content <- c(content, paste('   ext_regi "extended regions list (includes subsets of H12 regions)"'))
  content <- c(content, '      /')
  content <- c(content, '        GLO,')
  content <- c(content, '        ', paste(paste0(names(subsets),"_regi"),collapse=','),",")
  content <- c(content, '        ', paste(regions,collapse=','))
  content <- c(content, '      /')
  content <- c(content, ' ')
  # regi_group
  content <- c(content, '   regi_group(ext_regi,all_regi) "region groups (regions that together corresponds to a H12 region)"')
  content <- c(content, '      /')
  content <- c(content, '      ', paste('GLO.(',paste(regions,collapse=','),')'))
  for (i in 1:length(subsets)){
    content <- c(content, paste0('        ', paste(c(paste0(names(subsets)[i],"_regi"))), ' .(',paste(subsets[[i]],collapse=','), ')'))
  }
  content <- c(content, '      /')
  content <- c(content, ' ')
  # iso countries set
  content <- c(content,'   iso "list of iso countries" /')
  content <- c(content, .tmp(map$CountryCode, suffix1=",", suffix2=" /"),'')
  content <- c(content,'   regi2iso(all_regi,iso) "mapping regions to iso countries"','      /')
  for(i in as.character(unique(map$RegionCode))) {
    content <- c(content, .tmp(map$CountryCode[map$RegionCode==i], prefix=paste0(i," . ("), suffix1=")", suffix2=")"))
  }
  content <- c(content,'      /')
  content <- c(content, 'iso_regi "all iso countries and EU and greater China region" /  EUR,CHA,')
  content <- c(content, .tmp(map$CountryCode, suffix1=",", suffix2=" /"),'')
  content <- c(content,'   map_iso_regi(iso_regi,all_regi) "mapping from iso countries to regions that represent country" ','         /')
  for(i in regions[regions %in% c("EUR","CHA",as.character(unique(map$CountryCode)))]) {
    content <- c(content, .tmp(i, prefix=paste0(i," . "), suffix1="", suffix2=""))
  }
  content <- c(content,'      /',';')
  gms::replace_in_file('core/sets.gms',content,"SETS",comment="***")
}

create_fixing_files <- function(cfg, input_ref_file = "input_ref.gdx") {

  # Start the clock.
  begin <- Sys.time()

  # Extract data from input_ref.gdx file and store in levs_margs_ref.gms.
  system(paste("gdxdump",
               input_ref_file,
               "Format=gamsbas Delim=comma FilterDef=N Output=levs_margs_ref.gms",
               sep = " "))

  # Read data from levs_margs_ref.gms.
  ref_gdx_data <- suppressWarnings(readLines("levs_margs_ref.gms"))

  # Create fixing files.
  cat("\n")
  create_standard_fixings(cfg, ref_gdx_data)

  # Stop the clock.
  cat("Time it took to create the fixing files: ")
  manipulate_runtime <- Sys.time()-begin
  print(manipulate_runtime)
  cat("\n")


  # Delete file.
  file.remove("levs_margs_ref.gms")

}

create_standard_fixings <- function(cfg, ref_gdx_data) {

  # Declare empty lists to hold the strings for the 'manipulateFile' functions.
  full_manipulateThis <- NULL
  levs_manipulateThis <- NULL
  fixings_manipulateThis <- NULL
  margs_manipulateThis <- NULL

  str_years <- c()
  no_years  <- (cfg$gms$cm_startyear - 2005) / 5

  # Write level values to file
  levs <- c()
  for (i in 1:no_years) {
    str_years[i] <- paste("L \\('", 2000 + i * 5, sep = "")
    levs         <- c(levs, grep(str_years[i], ref_gdx_data, value = TRUE))
  }

  writeLines(levs, "levs.gms")

  # Replace fixing.gms with level values
  file.copy("levs.gms", "fixings.gms", overwrite = TRUE)

  fixings_manipulateThis <- c(fixings_manipulateThis, list(c(".L ", ".FX ")))
  #cb q_co2eq is only "static" equation to be active before cm_startyear, as multigasscen could be different from a scenario to another that is   fixed on the first
  #cb therefore, vm_co2eq cannot be fixed, otherwise infeasibilities would result. vm_co2eq.M is meaningless, is never used in the code (a   manipulateFile delete line command would be even better)
  #  manipulateFile("fixings.gms", list(c("vm_co2eq.FX ", "vm_co2eq.M ")))

  # Write marginal values to file
  margs <- c()
  str_years    <- c()
  for (i in 1:no_years) {
    str_years[i] <- paste("M \\('", 2000 + i * 5, sep = "")
    margs        <- c(margs, grep(str_years[i], ref_gdx_data, value = TRUE))
  }
  writeLines(margs, "margs.gms")
  # temporary fix so that you can use older gdx for fixings - will become obsolete in the future and can be deleted once the next variable name   change is done
  margs_manipulateThis <- c(margs_manipulateThis, list(c("q_taxrev","q21_taxrev")))
  # fixing for SPA runs based on ModPol input data
  margs_manipulateThis <- c(margs_manipulateThis,
                            list(c("q41_emitrade_restr_mp.M", "!!q41_emitrade_restr_mp.M")),
                            list(c("q41_emitrade_restr_mp2.M", "!!q41_emitrade_restr_mp2.M")))

  #AJS this symbol is not known and crashes the run - is it depreciated? TODO
  levs_manipulateThis <- c(levs_manipulateThis,
                           list(c("vm_pebiolc_price_base.L", "!!vm_pebiolc_price_base.L")))

  #AJS filter out nash marginals in negishi case, as they would lead to a crash when trying to fix on them:
  if(cfg$gms$optimization == 'negishi'){
    margs_manipulateThis <- c(margs_manipulateThis, list(c("q80_costAdjNash.M", "!!q80_costAdjNash.M")))
  }
  if(cfg$gms$subsidizeLearning == 'off'){
    levs_manipulateThis <- c(levs_manipulateThis,
                             list(c("v22_costSubsidizeLearningForeign.L",
                                    "!!v22_costSubsidizeLearningForeign.L")))
    margs_manipulateThis <- c(margs_manipulateThis,
                              list(c("q22_costSubsidizeLearning.M", "!!q22_costSubsidizeLearning.M")),
                              list(c("v22_costSubsidizeLearningForeign.M",
                                     "!!v22_costSubsidizeLearningForeign.M")),
                              list(c("q22_costSubsidizeLearningForeign.M",
                                     "!!q22_costSubsidizeLearningForeign.M")))
    fixings_manipulateThis <- c(fixings_manipulateThis,
                                list(c("v22_costSubsidizeLearningForeign.FX",
                                       "!!v22_costSubsidizeLearningForeign.FX")))

  }

  #JH filter out negishi marginals in nash case, as they would lead to a crash when trying to fix on them:
  if(cfg$gms$optimization == 'nash'){
    margs_manipulateThis <- c(margs_manipulateThis,
                              list(c("q80_balTrade.M", "!!q80_balTrade.M")),
                              list(c("q80_budget_helper.M", "!!q80_budget_helper.M")))
  }
  #RP filter out module 40 techpol fixings
  if(cfg$gms$techpol == 'none'){
    margs_manipulateThis <- c(margs_manipulateThis,
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
                              list(c("q40_BioFuelBound.M", "!!q40_BioFuelBound.M")))

  }

  if(cfg$gms$techpol == 'NPi2018'){
    margs_manipulateThis <- c(margs_manipulateThis,
                              list(c("q40_El_RenShare.M", "!!q40_El_RenShare.M")),
                              list(c("q40_CoalBound.M", "!!q40_CoalBound.M")))
  }

  # Include fixings (levels) and marginals in full.gms at predefined position
  # in core/loop.gms.
  full_manipulateThis <- c(full_manipulateThis,
                           list(c("cb20150605readinpositionforlevelfile",
                                  paste("first offlisting inclusion of levs.gms so that level value can be accessed",
                                        "$offlisting",
                                        "$include \"levs.gms\";",
                                        "$onlisting", sep = "\n"))))
  full_manipulateThis <- c(full_manipulateThis, list(c("cb20140305readinpositionforfinxingfiles",
                                                       paste("offlisting inclusion of levs.gms, fixings.gms, and margs.gms",
                                                             "$offlisting",
                                                             "$include \"levs.gms\";",
                                                             "$include \"fixings.gms\";",
                                                             "$include \"margs.gms\";",
                                                             "$onlisting", sep = "\n"))))


  # Perform actual manipulation on levs.gms, fixings.gms, and margs.gms in
  # single, respective, parses of the texts.
  manipulateFile("levs.gms", levs_manipulateThis)
  manipulateFile("fixings.gms", fixings_manipulateThis)
  manipulateFile("margs.gms", margs_manipulateThis)

  # Perform actual manipulation on full.gms, in single parse of the text.
  manipulateFile("full.gms", full_manipulateThis)
}
# -----------------------------------------------------------------
# -----------------------------------------------------------------
