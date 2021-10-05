######################################
# BUILT EM DATA FILE FROM OM (MSE2)
# Feb 2020
# CPeterson
######################################

# requires r4ss and OMdir and EM dir specified.
# library(r4ss)

### MAKE INTO A FUNCTION:
BuildEM <- function(EMdir, OMdir, tt, ...) {
  # Get EM data file and add results from MSE_Step2
  ### NOTE: Make data Corrections for variance adjustment!!!

  EMdat <- SS_readdat(file = paste(EMdir, "\\SB.dat", sep = ""), version = "3.30")
  OMboot_dat <- SS_readdat(file = paste(OMdir, "\\data.ss_new", sep = ""), section = 3, version = "3.30")


  # get new EM data
  NewEMdat <- EMdat

  # calculate end year of estimation model
  EndYr <- tt - 1
  NewEMdat$endyr <- EndYr

  # Replace catch with bootstrapped catch for existing years
  newcatch <- subset(OMboot_dat$catch, OMboot_dat$catch$year <= EndYr)
  NewEMdat$catch <- newcatch

  # Replace CPUE with bootstrapped CPUE for existing years
  newCPUE <- subset(OMboot_dat$CPUE, OMboot_dat$CPUE$year <= EndYr)
  NewEMdat$CPUE <- newCPUE


  # replace lencomp data with bootstrapped lencomp data
  newlen <- subset(OMboot_dat$lencomp, OMboot_dat$lencomp$Yr <= EndYr) # subset length comps
  newlen$Yr <- ifelse(newlen$Nsamp < 1, newlen$Yr * -1, newlen$Yr) # if Nsamp <=0, make year negative

  NewEMdat$lencomp <- newlen

  # re-write new EM data file with bootstrapped historical data
  SS_writedat(NewEMdat, outfile = paste0(EMdir, "\\SB.dat"), version = "3.30", overwrite = T)
} # end BuildEM function



BuildOM <- function(OMdir, tt, ...) {
  # re-write new OM data file with historical expected values
  OMexpect_dat <- SS_readdat(file = paste0(OMdir, "\\data.ss_new"), section = 2, version = "3.30")
  OMdat <- SS_readdat(file = paste0(OMdir, "\\data.ss_new"), section = 1, version = "3.30")
  newdat <- OMdat

  # Update Data
  newdat$catch[newdat$catch$year < tt, ] <- OMexpect_dat$catch[OMexpect_dat$catch$year < tt, ]
  newdat$CPUE[newdat$CPUE$year < tt, ] <- OMexpect_dat$CPUE[OMexpect_dat$CPUE$year < tt, ]
  newdat$lencomp[newdat$lencomp$Yr > 0 & newdat$lencomp$Yr < tt, ] <- OMexpect_dat$lencomp[OMexpect_dat$lencomp$Yr < tt, ]

  SS_writedat(newdat, outfile = paste(OMdir, "\\SB.dat", sep = ""), version = "3.30", overwrite = T)
} # END BuildOM function
