####################
# RUN EM ASSESSMENT
####################


### FROM r4ss ####
# library(r4ss)
#EM_SSdir = "N:\\Documents\\DFA_Simulation\\SB\\SimpleSim_MSE\\SS_mod_SS330_EM"

RunEM = function( EMdir , extras="" , intern=FALSE) {
  # CallType="system"
  
  newdir <- file.path(EMdir)
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(newdir)
  
  system(paste0("ss ",extras), intern=intern)
  
}




