######################################
# ANNUALLY UPDATE OM Data FILE
# June 2020
# CPeterson

## NOTE: This file has been updated to 
# assume OM is updated ANNUALLY, not 
# by FRQ. 
# Unique for Assessment Frequency 
# Update MSE. 
######################################


UpdateOM <- function(OMdir, tt, ...){
  # re-write new OM data file with historical expected values
  OMexpect_dat = SS_readdat(file=paste(OMdir,"\\data.ss_new", sep=""), section=2, version="3.30") 
  OMdat = SS_readdat(file=paste(OMdir,"\\data.ss_new", sep=""), section=1, version="3.30") 
  
  OMdat$CPUE[OMdat$CPUE$year==tt,] <- OMexpect_dat$CPUE[OMexpect_dat$CPUE$year==tt,]
  
  SS_writedat(OMdat, outfile=paste(OMdir,"\\SB.dat", sep=""), version="3.30", overwrite=T)
} # end UpdateOM function
