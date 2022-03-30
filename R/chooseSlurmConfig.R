chooseSlurmConfig <- function(identifier = FALSE) {
  if (! length(identifier) == 1 || ! identifier %in% paste(seq(1:16))) {
    wasselected <- TRUE
    cat(crayon::yellow("\nPlease choose the SLURM configuration for your submission:\n"))

    cat(crayon::yellow("\nCurrent cluster utilization:\n"))
    system("sclass")
    cat("\n")

    cat(paste0(
      crayon::yellow("Your options:\n"),
      crayon::blue("    QOS             tasks per node   suitable for\n"),
      crayon::blue("=======================================================================\n"),
      crayon::green(" 1:"), " SLURM standby               12   nash H12             [recommended]\n",
      crayon::green(" 2:"), " SLURM standby               13   nash H12 coupled\n",
      crayon::green(" 3:"), " SLURM standby               16   nash H12+\n",
      crayon::green(" 4:"), " SLURM standby                1   nash debug, testOneRegi, reporting\n",
      crayon::blue("--------------------------------------------------------------\n"),
      crayon::green(" 5:"), " SLURM priority              12   nash H12             [recommended]\n",
      crayon::green(" 6:"), " SLURM priority              13   nash H12 coupled\n",
      crayon::green(" 7:"), " SLURM priority              16   nash H12+\n",
      crayon::green(" 8:"), " SLURM priority               1   nash debug, testOneRegi, reporting\n",
      crayon::blue("--------------------------------------------------------------\n"),
      crayon::green(" 9:"), " SLURM short                 12   nash H12\n",
      crayon::green("10:"), " SLURM short                 16   nash H12+\n",
      crayon::green("11:"), " SLURM short                  1   nash debug, testOneRegi, reporting\n",
      crayon::green("12:"), " SLURM medium                 1   negishi\n",
      crayon::green("13:"), " SLURM long                   1   negishi\n",
      crayon::blue("=======================================================================\n"),
      crayon::green("Number: ")
    ))
    identifier <- strsplit(get_line(), ",")[[1]]
  }
  comp <- switch(as.numeric(identifier),
    "1" = "--qos=standby --nodes=1 --tasks-per-node=12", # SLURM standby  - task per node: 12 (nash H12) [recommended]
    "2" = "--qos=standby --nodes=1 --tasks-per-node=13", # SLURM standby  - task per node: 13 (nash H12 coupled)
    "3" = "--qos=standby --nodes=1 --tasks-per-node=16", # SLURM standby  - task per node: 16 (nash H12+)
    "4" = "--qos=standby --nodes=1 --tasks-per-node=1", # SLURM standby  - task per node: 1 (nash debug, test one regi)
    "5" = "--qos=priority --nodes=1 --tasks-per-node=12", # SLURM priority - task per node: 12 (nash H12) [recommended]
    "6" = "--qos=priority --nodes=1 --tasks-per-node=13", # SLURM priority - task per node: 13 (nash H12 coupled)
    "7" = "--qos=priority --nodes=1 --tasks-per-node=16", # SLURM priority - task per node: 16 (nash H12+)
    "8" = "--qos=priority --nodes=1 --tasks-per-node=1", # SLURM priority - task per node:1 (nash debug, test one regi)
    "9" = "--qos=short --nodes=1 --tasks-per-node=12", # SLURM short    - task per node: 12 (nash H12)
    "10" = "--qos=short --nodes=1 --tasks-per-node=16", # SLURM short    - task per node: 16 (nash H12+)
    "11" = "--qos=short --nodes=1 --tasks-per-node=1", # SLURM short    - task per node:  1 (nash debug, test one regi)
    "12" = "--qos=medium --nodes=1 --tasks-per-node=1", # SLURM medium   - task per node:  1 (negishi)
    "13" = "--qos=long --nodes=1 --tasks-per-node=1", # SLURM long     - task per node:  1 (negishi)
    "14" = "--qos=medium --nodes=1 --tasks-per-node=12", # SLURM medium   - task per node: 12 (nash long calibration)
    "15" = "--qos=medium --nodes=1 --tasks-per-node=16", # SLURM medium   - task per node: 16 (nash long calibration)
    "16" = "direct"
  )
  if (is.null(comp)) {
    message("This type is invalid. Please choose a valid type!")
    comp <- chooseSlurmConfig()
  }
  if (! exists("wasselected")) {
    message("   SLURM option ", identifier, " selected: ", gsub("--", "", comp))
  }
  comp
}

# combine_slurmconfig takes two strings with SLURM parameters (e.g. "--qos=priority --time=03:30:00")
# and combines them into one sting of SLURM parameters overwriting the parameters in "original"
# if they also exist in "updateWith".

combineSlurmConfig <- function(original, updateWith) {

  # trim whitespaces
  original <- trimws(original)
  updateWith <- trimws(updateWith)

  # remove double whitespaces
  original <- gsub("\\s+", " ", original)
  updateWith <- gsub("\\s+", " ", updateWith)

  # if user chose "direct" dont update any slurm commands
  if (updateWith == "direct") {
    return(updateWith)
  }

  # ignore original if it is "direct"
  if (original == "direct") original <- ""

  # put RHS strings into vector
  vUpdateWith <- gsub("--.*=(.*)", "\\1", unlist(strsplit(updateWith, split = " ")))
  # name the vector using LHS strings
  names(vUpdateWith) <- gsub("--(.*)=.*", "\\1", unlist(strsplit(updateWith, split = " ")))

  # put RHS strings into vector
  vOriginal <- gsub("--.*=(.*)", "\\1", unlist(strsplit(original, split = " ")))
  # name the vector using LHS strings
  names(vOriginal) <- gsub("--(.*)=.*", "\\1", unlist(strsplit(original, split = " ")))

  # remove elements from "original" that are existing in "updateWith"
  vOriginal <- vOriginal[!names(vOriginal) %in% "qos"]

  combined <- c(vUpdateWith, vOriginal)

  # concatenate SLURM command (insert "--" and "=")
  paste(paste0("--", names(combined), "=", combined), collapse = " ")
}
