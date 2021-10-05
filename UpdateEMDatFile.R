######################################
# UPDATE EM DATA FILE FROM OM (MSE2)
# Feb 2021
# CPeterson
######################################

# library(r4ss)
#

### MAKE INTO A FUNCTION:
UpdateEM <- function(EMdir, OMdir, FRQ = 5, tt, ...) {
  # Get EM data file and add results from MSE_Step2
  ### Make data Corrections for variance adjustment!


  #### ----------------------------------------------------------------------------------------------------------
  # get data files
  #### ----------------------------------------------------------------------------------------------------------

  EMdat <- SS_readdat(file = paste(EMdir, "\\SB.dat", sep = ""), version = "3.30")

  OMboot_dat <- SS_readdat(file = paste(OMdir, "\\data.ss_new", sep = ""), section = 3, version = "3.30")
  OMdat <- SS_readdat(file = paste(OMdir, "\\SB.dat", sep = ""), section = 1, version = "3.30")

  # get new EM data
  NewEMdat <- EMdat



  #### ----------------------------------------------------------------------------------------------------------
  # calculate years
  #### ----------------------------------------------------------------------------------------------------------
  StartYr <- tt - FRQ
  EndYr <- tt - 1




  #### ----------------------------------------------------------------------------------------------------------
  # Add new catch years
  #### ----------------------------------------------------------------------------------------------------------
  #     get old and new catch; subset by fleet and reconstruct.
  oldcatch <- EMdat$catch
  newcatch <- subset(OMboot_dat$catch, OMboot_dat$catch$year >= StartYr & OMboot_dat$catch$year <= EndYr) # get new catch data
  catch <- c()
  for (l in levels(as.factor(EMdat$catch$fleet))) { # recombine old+new catch data in order
    assign(paste0("oldcatchF", l), subset(oldcatch, oldcatch$fleet == l))
    assign(paste0("newcatchF", l), subset(newcatch, newcatch$fleet == l))
    catch <- rbind(catch, get(paste0("oldcatchF", l)), get(paste0("newcatchF", l)))
  }
  NewEMdat$catch <- round(catch, digits = 5)



  #### ----------------------------------------------------------------------------------------------------------
  # Add new CPUE years
  #### ----------------------------------------------------------------------------------------------------------
  oldCPUE <- EMdat$CPUE
  newCPUE <- subset(OMboot_dat$CPUE, OMboot_dat$CPUE$year >= StartYr & OMboot_dat$CPUE$year <= EndYr) # get new CPUE data
  CPUE <- c()
  for (m in levels(as.factor(EMdat$CPUE$index))) { # recombine old + new CPUE data in order
    assign(paste0("oldCPUE", m), subset(oldCPUE, oldCPUE$index == m))
    assign(paste0("newCPUE", m), subset(newCPUE, newCPUE$index == m))
    CPUE <- rbind(CPUE, get(paste0("oldCPUE", m)), get(paste0("newCPUE", m)))
  }
  NewEMdat$CPUE <- round(CPUE, digits = 5)



  #### ----------------------------------------------------------------------------------------------------------
  # Add new lencomps
  #### ----------------------------------------------------------------------------------------------------------

  origlen <- EMdat$lencomp
  newlen <- subset(OMdat$lencomp, abs(OMdat$lencomp$Yr) >= StartYr & abs(OMdat$lencomp$Yr) <= EndYr)
  # NOTE: the bootstrapped data ignores negative years; use OMdat to preserve all years

  newlen[newlen$Yr >= StartYr & newlen$Yr <= EndYr, ] <-
    subset(OMboot_dat$lencomp, OMboot_dat$lencomp$Yr >= StartYr & OMboot_dat$lencomp$Yr <= EndYr)
  # insert bootstrapped data into newlen
  newlen$Yr <- abs(newlen$Yr) # take absolute value of year for the purposes of compiling lengthcomps


  lencomps <- c()
  for (n in levels(as.factor(EMdat$lencomp$FltSvy))) {
    if (n %in% levels(as.factor(EMdat$catch$fleet)) == TRUE) { # if n is a fishery fleet

      origl <- subset(origlen, origlen$FltSvy == n) # subset original lencomps
      newl <- subset(newlen, newlen$FltSvy == n) # subset new lencomps for each fleet


      lencomps <- rbind(lencomps, origl, newl) # put lencomps back together
    } # end if n %in% fleet

    if (n %in% levels(as.factor(EMdat$CPUE$index)) == TRUE) { # if n is a survey fleet

      origl <- subset(origlen, origlen$FltSvy == n) # subset orig lens
      newl <- subset(newlen, newlen$FltSvy == n) # subset new lens

      lencomps <- rbind(lencomps, origl, newl) # put lencomps back together
    } # end if n %in% survey
  } # end n loop for lencomps
  lencomps$Yr <- ifelse(lencomps$Nsamp < 1, -1 * abs(lencomps$Yr), lencomps$Yr)
  NewEMdat$lencomp <- round(lencomps, digits = 5)
  NewEMdat$CPUE$obs <- ifelse(NewEMdat$CPUE$obs == 0, 0.00001, NewEMdat$CPUE$obs)

  #### ----------------------------------------------------------------------------------------------------------
  # update endyear
  #### ----------------------------------------------------------------------------------------------------------
  NewEMdat$endyr <- EndYr

  #### ----------------------------------------------------------------------------------------------------------
  # re-write new EM data file
  #### ----------------------------------------------------------------------------------------------------------
  SS_writedat(NewEMdat, outfile = paste(EMdir, "\\SB.dat", sep = ""), version = "3.30", overwrite = T)
} # end UpdateEM function



UpdateOM_annual <- function(OMdir, t, fq = 1, ...) {
  # re-write new OM data file with historical expected values
  OMexpect_dat <- SS_readdat(file = paste(OMdir, "\\data.ss_new", sep = ""), section = 2, version = "3.30")
  OMdat <- SS_readdat(file = paste(OMdir, "\\data.ss_new", sep = ""), section = 1, version = "3.30")

  OMdat$CPUE[OMdat$CPUE$year >= t - fq & OMdat$CPUE$year < t, ] <- OMexpect_dat$CPUE[OMexpect_dat$CPUE$year >= t - fq & OMexpect_dat$CPUE$year < t, ]
  OMdat$CPUE <- round(OMdat$CPUE, digits = 5)
  OMdat$CPUE$obs <- ifelse(OMdat$CPUE$obs == 0, 0.00001, OMdat$CPUE$obs)

  SS_writedat(OMdat, outfile = paste(OMdir, "\\SB.dat", sep = ""), version = "3.30", overwrite = T)
} # end UpdateOM function
