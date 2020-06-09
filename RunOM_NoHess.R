### Run OM with -nohess option ####
# NOTE: code is specific to windows computer


### function requirements ####
# library(r4ss)
# OMdir = "R:\\Management Strategy Evaluation\\SB\\TEST_Base\\HCR1\\OM"


RunOM_NoHess = function( OMdir , extras="", intern=FALSE) {
  
  newdir <- file.path(OMdir)
  oldwd <- getwd()              # save working directory
  on.exit(setwd(oldwd))
  setwd(newdir)
  
  system(paste0("ss -nohess"," ",extras), intern=intern)
  
  # setwd(oldwd)
}
