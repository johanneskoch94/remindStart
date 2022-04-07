chooseFolder <- function(folder, title = "Please choose a folder") {
  dirs <- dirname(Sys.glob(file.path(folder, "*", "full.gms")))
  # DK: The following outcommented lines are specially made for listing results of coupled runs
  #runs <- findCoupledruns(folder)
  #dirs <- findIterations(runs,modelpath=folder,latest=TRUE)
  #dirs <- sub("./output/","",dirs)
  return(chooseFromList(dirs, type = "folders", returnboolean = FALSE))
}
