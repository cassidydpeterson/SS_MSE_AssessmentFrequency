### Implement HCR # ##
# NOTE: This code will be specific to this example. 

# # Data Inputs #
# 
# hcr = list(Fset = Fset, ACL = ACL )
# acl = hcr$ACL
# acl = ACL
# FRQ = 5
# modEM = modEM
# modEM = SS_output(EMdir)
# MSEResults = list()
# OMdir = "R:\\Management Strategy Evaluation\\SB\\TEST_Base\\HCR1\\OM" # OM directory

# START FUNCTION
implementHCR = function(hcr, tt, FRQ, modEM, OMdir, i, seed=430, ...){
  set.seed(seed*tt)
  modOM = SS_output(OMdir)
  
  #-------------------------------------------------------------------------------------------------------------
  # Implementation uncertainty + allocation
  #-------------------------------------------------------------------------------------------------------------
  # COMMERCIAL CATCH #
  ### calculate expected commercial ACL ###
  # 41.7 mt DW => 58 mt round weight
  comACL = hcr$ACL - 58
  
  ### commercial catch w implementation uncertainty!   ###
  # actualCatch = rlnorm(FRQ, -0.2722412, 0.3306523) * comACL
  actualCatch = rlnorm(FRQ, -0.6015450, 0.3306523) * comACL
  
  
  ### allocate commercial catch to area ###
  # F1 #
  GOMprop = rbeta(FRQ, 8.533997, 8.731176) # GOM proportion actualCatch
  F1catch = actualCatch * GOMprop
  F1catch = ifelse(F1catch<0, 0, F1catch)
  
  # F2 #
  Atlprop = 1-GOMprop                    # Atl proportion actualCatch
  F2catch = actualCatch * Atlprop
  F2catch = ifelse(F2catch<0, 0, F2catch)
  
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  # Calculate Nsamp for LFreqs
  #-------------------------------------------------------------------------------------------------------------
  # SCALE LFreq effNs
  #        comm effN relationship w/ commercial catch
  #        by scaling relationship between survey effN and commercial catch
  
  # NOTE: parameters estiamted via empirical relationships
  
  # Commercial Fleets #
  F1c = ifelse(F1catch==0, 1e-5, F1catch)
  NsampF1 = round( ( 10.72071 * log(F1c) ) + -36.72194 + rnorm(FRQ, 0, 14.86394 ) )
  NsampF1 = ifelse(NsampF1<0, 0, NsampF1)
  F2c = ifelse(F2catch==0, 1e-5, F2catch)
  NsampF2 = round( ( 2.196819 * log(F2c) ) + -7.636586 + rnorm(FRQ, 0, 2.99855) )
  NsampF2 = ifelse(NsampF2<0, 0, NsampF2)
  
  
  
  # 
  # #-------------------------------------------------------------------------------------------------------------
  # # Save Results
  # #-------------------------------------------------------------------------------------------------------------
  # ### SAVE RESULTS ###
  # 
  # # Create MSEResults$ACL_i (if it doesn't already exist)
  # if(is.null(MSEResults[[paste0("ACL_",i)]]) == TRUE ){
  #   MSEResults[[paste0("ACL_",i)]] = 
  #     data.frame(Year=numeric(), ACL_from_HCR=numeric(), corrected_ACL=numeric(), Implemented_Catch=numeric(), F1Catch = numeric(), 
  #                F2Catch=numeric(), F3Catch=numeric(), F4Catch=numeric(), HCR_Fset=numeric(), Estimated_FMSY=numeric())
  # }
  # 
  # # define endyr of EM assessed year
  # endyr = tt-1
  # 
  # # Save results in MSEResults tagged in the present iteration i
  # MSEResults[[paste0("ACL_",i)]] <- rbind(MSEResults[[paste0("ACL_",i)]], 
  #                                         cbind(Year = endyr + c(1:FRQ), 
  #                                               ACL_from_HCR = rep(hcr$ACL, FRQ), 
  #                                               corrected_ACL = rep(comACL,5), 
  #                                               Implemented_Catch = actualCatch, 
  #                                               F1Catch = F1catch, 
  #                                               F2Catch = F2catch, 
  #                                               # F3Catch = F3catch, 
  #                                               # F4Catch = F4catch, 
  #                                               HCR_Fset = rep(hcr$Fset, FRQ),
  #                                               Estimated_FMSY = rep(modEM$derived_quants["Fstd_MSY","Value"], FRQ) ) )
  # 
  # assign("MSEResults", MSEResults, envir=globalenv())       # save MSEResults in global environment
  # 
  
  # #-------------------------------------------------------------------------------------------------------------
  # # Add new data to OM
  # #-------------------------------------------------------------------------------------------------------------
  # # Put future catches into OM
  # 
  # OMdat = SS_readdat(file=paste0(OMdir,"\\SB.dat"), version="3.30") 
  # newOMdat = OMdat
  # endyr = tt-1
  # for(y in (endyr+1):(endyr+FRQ)){
  #   
  #   # Update catches for each fleet
  #   for( f in levels(as.factor(newOMdat$catch$fleet)) ){
  #     newOMdat$catch[newOMdat$catch$year==y & newOMdat$catch$fleet==f, "catch"] =
  #       get(paste0("F", f, "catch"))[y-endyr]
  #   } # end fishing fleet f loop
  #   
  #   
  #   # update Lfreqs for each fleet/survey
  #   for(fs in levels(as.factor(newOMdat$lencomp$FltSvy)) ){
  #     newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Nsamp"] = get(paste0("NsampF",fs))[y-endyr]
  #     
  #     # IF Nsamp is zero or negative, set year = -year. 
  #     newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Yr"] = 
  #       ifelse(newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Nsamp"]<=0, 
  #              -1*newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Yr"], 
  #              newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Yr"])
  #   } # end Lfreq fs loop
  #   
  #   
  # } # end year loop
  # 
  # 
  # 
  # # Re-write OM data file with additional FRQ years of data. 
  # SS_writedat(newOMdat, outfile=paste0(OMdir,"\\SB.dat"), version="3.30", overwrite=T)
  
  
  return(list(F1catch = F1catch, F2catch = F2catch, NsampF1 = NsampF1, NsampF2 = NsampF2))
} # End implementHCR function




