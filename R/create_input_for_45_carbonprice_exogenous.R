create_input_for_45_carbonprice_exogenous <- function(gdx) {
  p_fpath <- "./modules/45_carbonprice/exogenous/input/p45_tau_co2_tax.inc"

  # ---- Read data ----

  if (file.exists(gdx)) {
    pr <- reportPrices(gdx)
  } else {
    stop("No gdx file found to take the carbon price from - please provide gdx from a reference run in path_gdx_carbonprice in scenario_config file.")
  }

  # ---- Convert data ----

  # select right temporal/variable scope
  pr <- pr[, , c("Price|Carbon (US$2005/t CO2)")]
  # convert from $/tCO2 to $/kgC (or T$/GtC)
  pr <- pr / 1000 * 44 / 12
  # remove GLO region if it exists
  if ("GLO" %in% getRegions(pr)) {
    pr <- pr["GLO", , , invert = TRUE]
  }

  # ---- Export data ----

  # Header
  cat("*** SOF ", p_fpath, "\n", file = p_fpath, sep = "", append = FALSE)
  cat("*=============================================================*\n", file = p_fpath, append = TRUE)
  cat("*=              Exogenous CO2 tax level                      =*\n", file = p_fpath, append = TRUE)
  cat("*=============================================================*\n", file = p_fpath, append = TRUE)
  cat("*= author: dklein@pik-potsdam.de                             =*\n", file = p_fpath, append = TRUE)
  cat(paste("*= date  : ", Sys.time(), "                               =*\n", sep = ""), file = p_fpath, append = TRUE)
  cat("*= generated with:                                           =*\n", file = p_fpath, append = TRUE)
  cat("*= scripts/input/create_input_for_45_carbonprice_exogenous.R =*\n", file = p_fpath, append = TRUE)
  cat(paste0("*= from file: ", normalizePath(gdx), " =*\n"), file = p_fpath, append = TRUE)
  cat("*= unit: 10^12 US$(2005)/GtC                                 =*\n", file = p_fpath, append = TRUE)
  cat("*=============================================================*\n", file = p_fpath, append = TRUE)
  cat("\n", file = p_fpath, append = TRUE)

  # Content
  # Loop over time dimension
  for (y in getYears(pr)) {
    for (r in getRegions(pr)) {
      cat("p45_tau_co2_tax(\"", gsub("y", "", y), "\",\"", r, "\")=", pr[r, y, ], ";\n", sep = "", file = p_fpath, append = TRUE)
    }
  }

  cat("*** EOF ", p_fpath, "\n", file = p_fpath, sep = "", append = TRUE)
}
