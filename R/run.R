run <- function() {
  # Save start time
  timeGAMSStart <- Sys.time()

  load("config.Rdata")

  # Display git information in log
  cat(cfg$gitInfo$info_str, "\n")

  # De-compress finxing files if they have already been zipped (only valid if run is restarted)
  if (cfg$gms$cm_startyear > 2005) {
    if (file.exists("levs.gms.gz")) {
      cat("Unzip fixing files\n")
      system("gzip -d -f levs.gms.gz margs.gms.gz fixings.gms.gz")
    } else if (file.exists("levs.gms")) {
      cat("Found unzipped fixing files. Using them.\n")
    } else {
      stop("cm_startyear > 2005 but no fixing files found, neither zipped or unzipped.")
    }
  }

  # Print message
  cat("\n################\nStarting REMIND, time is", format(Sys.time(),"%H:%M:%S"), "...\n")

  # Call GAMS
  if (cfg$gms$CES_parameters == "load") {

    system(paste0(
      cfg$gamsv, " full.gms -errmsg=1 -a=", cfg$action, " -ps=0 -pw=185 -gdxcompress=1 -logoption=", cfg$logoption
    ))

  } else if (cfg$gms$CES_parameters == "calibrate") {

    # Remember file modification time of fulldata.gdx to see if it changed
    fulldata_m_time <- Sys.time();

    # Save original input
    file.copy("input.gdx", "input_00.gdx", overwrite = TRUE)

    # Iterate calibration algorithm
    for (cal_itr in 1:cfg$gms$c_CES_calibration_iterations) {
      cat("CES calibration iteration: ", cal_itr, "\n")

      # Update calibration iteration in GAMS file
      system(paste0("sed -i 's/^\\(\\$setglobal c_CES_calibration_iteration ",
                    "\\).*/\\1", cal_itr, "/' full.gms"))

      system(paste0(cfg$gamsv, " full.gms -errmsg=1 -a=", cfg$action,
                    " -ps=0 -pw=185 -gdxcompress=1 -logoption=", cfg$logoption))

      # If GAMS found a solution
      if (   file.exists("fulldata.gdx")
             && file.info("fulldata.gdx")$mtime > fulldata_m_time) {

        #create the file to be used in the load mode
        getLoadFile <- function(){

          file_name = paste0(cfg$gms$cm_CES_configuration,"_ITERATION_",cal_itr,".inc")
          ces_in = system("gdxdump fulldata.gdx symb=in NoHeader Format=CSV", intern = TRUE) %>% gsub("\"","",.) #" This comment is just to   obtain correct syntax highlighting
          expr_ces_in = paste0("(",paste(ces_in, collapse = "|") ,")")


          tmp = system("gdxdump fulldata.gdx symb=pm_cesdata", intern = TRUE)[-(1:2)] %>%
            grep("(quantity|price|eff|effgr|xi|rho|offset_quantity|compl_coef)", x = ., value = TRUE)
          tmp = tmp %>% grep(expr_ces_in,x = ., value = T)

          tmp %>%
            sub("'([^']*)'.'([^']*)'.'([^']*)'.'([^']*)' (.*)[ ,][ /];?",
                "pm_cesdata(\"\\1\",\"\\2\",\"\\3\",\"\\4\") = \\5;", x = .) %>%
            write(file_name)


          pm_cesdata_putty = system("gdxdump fulldata.gdx symb=pm_cesdata_putty", intern = TRUE)
          if (length(pm_cesdata_putty) == 2){
            tmp_putty =  gsub("^Parameter *([A-z_(,)])+cesParameters\\).*$",'\\1"quantity")  =   0;',  pm_cesdata_putty[2])
          } else {
            tmp_putty = pm_cesdata_putty[-(1:2)] %>%
              grep("quantity", x = ., value = TRUE) %>%
              grep(expr_ces_in,x = ., value = T)
          }
          tmp_putty %>%
            sub("'([^']*)'.'([^']*)'.'([^']*)'.'([^']*)' (.*)[ ,][ /];?",
                "pm_cesdata_putty(\"\\1\",\"\\2\",\"\\3\",\"\\4\") = \\5;", x = .)%>% write(file_name,append =T)
        }

        getLoadFile()

        # Store all the interesting output
        file.copy("full.lst", sprintf("full_%02i.lst", cal_itr), overwrite = TRUE)
        file.copy("full.log", sprintf("full_%02i.log", cal_itr), overwrite = TRUE)
        file.copy("fulldata.gdx", "input.gdx", overwrite = TRUE)
        file.copy("fulldata.gdx", sprintf("input_%02i.gdx", cal_itr),
                  overwrite = TRUE)

        # Update file modification time
        fulldata_m_time <- file.info("fulldata.gdx")$mtime

      } else {
        break
      }
    }
  } else {
    stop("unknown realisation of 29_CES_parameters")
  }



  # Calculate run time statistics, print message and save to runstatistics
  timeGAMSEnd  <- Sys.time()
  gams_runtime <- timeGAMSEnd - timeGAMSStart
  cat("\nEnding REMIND, time is", format(Sys.time(),"%H:%M:%S"),
      "\n################\n\nREMIND run finished in", round(as.numeric(gams_runtime),2), units(gams_runtime),"\n\n")
  lucode2::runstatistics(file            = "runstatistics.rda",
                         timeGAMSStart   = timeGAMSStart,
                         timeGAMSEnd     = timeGAMSEnd)

  # If fulldata.gdx exists, collect and submit run statistics to central data base
  if (file.exists("fulldata.gdx")) {
    lucode2::runstatistics(file       = "runstatistics.rda",
                           modelstat  = gdx::readGDX(gdx = "fulldata.gdx", "o_modelstat", format = "first_found"),
                           config     = cfg,
                           runtime    = gams_runtime,
                           setup_info = lucode2::setup_info(),
                           submit     = cfg$runstatistics)
  } else {
    cat("No fulldata.gdx file found. REMIND did not run correctly.")
  }

  # If REMIND ran, was NASH, and completed successfully, then append nash solution report to lst
  if (cfg$action == "ce" && cfg$gms$c_skip_output != "on" && cfg$gms$optimization == "nash" &&
      cfg$gms$cm_nash_mode != "debug" && file.exists("fulldata.gdx")) {
    system("gdxdump fulldata.gdx Format=gamsbas Delim=comma Output=output_nash.gms")
    file.append("full.lst", "output_nash.gms")
    file.remove("output_nash.gms")
  }

  # Compress files with the fixing-information
  if (cfg$gms$cm_startyear > 2005) system("gzip -f levs.gms margs.gms fixings.gms")
}
