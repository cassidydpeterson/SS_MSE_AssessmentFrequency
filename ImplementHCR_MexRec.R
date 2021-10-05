### Implement HCR # ##
# NOTE: This code will be specific to this example.


# START FUNCTION
implementHCR <- function(hcr, tt, FRQ, modEM, OMdir, i, seed = 430, MaxCatch = NA, ...) {
  set.seed(seed * tt)
  modOM <- SS_output(OMdir)

  #-------------------------------------------------------------------------------------------------------------
  # Implementation uncertainty + allocation
  #-------------------------------------------------------------------------------------------------------------

  ## Corrections for Max Catch ###
  # if max catch present
  if (!is.na(MaxCatch)) {
    hcr$ACL <- ifelse(hcr$ACL > MaxCatch, MaxCatch, hcr$ACL)
  }

  # if ACL > 50% total biomass, limit catch
  if (hcr$ACL >= 0.5 * modOM$timeseries[modOM$timeseries$Yr == (tt - 1), ]$Bio_all) {
    c1 <- vector(length = FRQ)
    for (f in 1:FRQ) {
      c1[f] <- 0.5 * ((1 - 0.5)^(f - 1)) * modOM$timeseries[modOM$timeseries$Yr == tt, ]$Bio_all
    } # end f loop

    hcr$ACL <- c1
  } # end if ACL > 50% total biomass


  # COMMERCIAL CATCH #
  ### calculate expected commercial ACL ###
  # 41.7 mt DW => 58 mt round weight
  comACL <- hcr$ACL / 2

  ### commercial catch w implementation uncertainty!   ###
  actualCatch <- rlnorm(FRQ, -0.6015450, 0.3306523) * comACL


  ### allocate commercial catch to area ###
  # F1 #
  GOMprop <- rbeta(FRQ, 8.533997, 8.731176) # GOM proportion actualCatch
  F1catch <- actualCatch * GOMprop
  F1catch <- ifelse(F1catch < 0, 0, F1catch)

  # F2 #
  Atlprop <- 1 - GOMprop # Atl proportion actualCatch
  F2catch <- actualCatch * Atlprop
  F2catch <- ifelse(F2catch < 0, 0, F2catch)


  # RECREATIONAL AND DISCARDS #
  # F3 #
  # NEW WAY / NEW IMPLEMENT HCR CODE #
  F3exp <- hcr$ACL / 2
  F3catch <- F3exp + rnorm(FRQ, 0, 102.0899 / 5) # actual F3 catch w implementation uncertainty; true sd too hgih
  # # note: chosing to not include uncertainty at this stage, because the data generating process will add appropriate uncertainty;
  #       included b/c more consistent with historical data

  F3catch <- ifelse(F3catch < 0, 0, F3catch)
  ###     NOTE: I am cheating and reducing SD by an order of magnitude; otherwise very crazy results


  #-------------------------------------------------------------------------------------------------------------
  # Calculate Nsamp for LFreqs
  #-------------------------------------------------------------------------------------------------------------
  # SCALE LFreq effNs
  #        comm effN relationship w/ commercial catch
  #        by scaling relationship between survey effN and commercial catch

  # NOTE: parameters estiamted via empirical relationships

  # Commercial Fleets #
  F1c <- ifelse(F1catch == 0, 1e-5, F1catch)
  NsampF1 <- round((10.72071 * log(F1c)) + -36.72194 + rnorm(FRQ, 0, 14.86394))
  NsampF1 <- ifelse(NsampF1 < 0, 0, NsampF1)
  F2c <- ifelse(F2catch == 0, 1e-5, F2catch)
  NsampF2 <- round((2.196819 * log(F2c)) + -7.636586 + rnorm(FRQ, 0, 2.99855))
  NsampF2 <- ifelse(NsampF2 < 0, 0, NsampF2)
  F3c <- ifelse(F3catch == 0, 1e-5, F3catch)
  NsampF3 <- round((1.221945 * log(F3c)) + -3.150376 + rnorm(FRQ, 0, 1.907433))
  NsampF3 <- ifelse(NsampF3 < 0, 0, NsampF3)


  # Return
  return(list(F1catch = F1catch, F2catch = F2catch, F3catch = F3catch, NsampF1 = NsampF1, NsampF2 = NsampF2, NsampF3 = NsampF3))
} # End implementHCR function
