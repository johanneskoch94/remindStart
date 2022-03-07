prepare_NDC <- function(gdx, cfg) {

  if (file.exists(gdx)) {
    emi <- remind2::reportEmi(gdx)
  } else {
    stop("No gdx file found - please provide gdx from reference BAU run")
  }
  regs <- setdiff(magclass::getRegions(emi), "GLO")
  if ("Emi|GHG|w/o Land-Use Change (Mt CO2eq/yr)" %in% magclass::getItems(emi, 3.1)) {
    pm_BAU_reg_emi_wo_LU_bunkers <- emi[regs, seq(2005, 2050, 5), "Emi|GHG|w/o Land-Use Change (Mt CO2eq/yr)"]
  } else if ("Emi|Kyoto Gases excl Land-Use Change|w/o Bunkers (Mt CO2-equiv/yr)" %in% magclass::getItems(emi, 3.1)) {
    pm_BAU_reg_emi_wo_LU_bunkers <- emi[regs, seq(2005, 2050, 5), "Emi|Kyoto Gases excl Land-Use Change|w/o Bunkers (Mt CO2-equiv/yr)"]
  } else {
    stop("No emissions variable found in the NDC script!")
  }

  magclass::getNames(pm_BAU_reg_emi_wo_LU_bunkers) <- NULL
  magclass::write.magpie(pm_BAU_reg_emi_wo_LU_bunkers, "./modules/45_carbonprice/NDC/input/pm_BAU_reg_emi_wo_LU_bunkers.cs4r",
                         comment = "** description: Regional GHG emi (excl. LU and bunkers) in BAU scenario \n*** unit: Mt CO2eq/yr \n*** file created with scripts/input/prepare_NDC.R")
  magclass::write.magpie(pm_BAU_reg_emi_wo_LU_bunkers, "./modules/46_carbonpriceRegi/NDC/input/pm_BAU_reg_emi_wo_LU_bunkers.cs4r",
                         comment = "** description: Regional GHG emi (excl. LU and bunkers) in BAU scenario \n*** unit: Mt CO2eq/yr \n*** file created with scripts/input/prepare_NDC.R")
}
