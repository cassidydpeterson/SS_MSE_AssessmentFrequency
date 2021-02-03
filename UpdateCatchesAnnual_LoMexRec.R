### Update Catches Annual LoMexRec
###############################################
# UPDATE CATCHES ANNUALLY BETWEEN ASSESSMENTS
# Feb 2021
###############################################
# hcr
# tt
# modEM
# OMdir
# i
# seed=430
# F1catch=ComCat$F1catch[yt]
# F2catch=ComCat$F2catch[yt]
# NsampF1=ComCat$NsampF1[yt]
# NsampF2=ComCat$NsampF2[yt]

UpdateCatchesAnnual = function(hcr, tt, yt, modEM, OMdir, i, seed=430, ComCat=ComCat,  ...){
  
  #set seed and get OM output. 
  set.seed(seed*(tt+yt))
  modOM = SS_output(OMdir)
  
  # Get commercial catches from ComCat. 
  F1catch=ComCat$F1catch[yt]
  F2catch=ComCat$F2catch[yt] 
  NsampF1=ComCat$NsampF1[yt]
  NsampF2=ComCat$NsampF2[yt]
  
  
  # RECREATIONAL AND DISCARDS #
  # F3 #
  #     estimate next year's EM forecasted population size
  #     parameters are based on a linear regression between EM observed biomass and F3 catch
  F3catch = rnorm(1, 109, 17.7)
  F3catch = ifelse(F3catch<0, 0, F3catch)
  ###     NOTE: I am cheating and reducing SD by an order of magnitude; otherwise very crazy results
  
  
  # F4 #
  #     estimate next year's EM forecasted population size
  #     parameters are based on a linear regression between EM observed biomass and F4 catch
  lr4 = lm(modOM$timeseries[modOM$timeseries$Yr>1994 & modOM$timeseries$Yr<2016,]$`dead(N):_4` ~
             modOM$timeseries[modOM$timeseries$Yr>1994 & modOM$timeseries$Yr<2016,]$Bio_all)
  F4exp = (  lr4$coefficients[1] ) + ( ( lr4$coefficients[2] )*modOM$timeseries[modOM$timeseries$Yr==tt-1,]$Bio_all )     # expected F4 catch
  F4catch = F4exp + rnorm(1,0,0.04200714/2 )                         # actual F3 catch w implementation uncertainty; true sd too high
  # # note: considered chosing to not include uncertainty at this stage, because the data generating process will add appropriate uncertainty; 
  #       included b/c more consistent with historical data
  
  F4catch = ifelse(F4catch<0, 0, F4catch)
  
  
  
  #### NSamp for LenFreqs
  F3c = ifelse(F3catch==0, 1e-5, F3catch)
  NsampF3 = round( (  1.221945 * log(F3c)  ) + -3.150376 + rnorm(1, 0, 1.907433) )
  NsampF3 = ifelse(NsampF3<0, 0, NsampF3)
  
  # Surveys #
  # S1 #
  #  based on empirically estimated linear relationship between log(Nsamp) and predicted biomass
  NsampF5 = round(exp( -0.7597465 + (4.564626e-05 * modEM$timeseries$Bio_all[ nrow(modEM$timeseries) ] ) + rnorm(1, 0, 0.5243653) + 0.1546644))
  
  # S2 # 
  # sample from a truncated normal distribution (with bounds to restrict observations within realm of observed data)
  NsampF6 = round( rtnorm(1, 8.074, 4.609146 , 0, 25) )
  
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  # Add new data to OM
  #-------------------------------------------------------------------------------------------------------------
  # Put future catches into OM
  
  OMdat = SS_readdat(file=paste0(OMdir,"\\SB.dat"), version="3.30") 
  newOMdat = OMdat
  endyr = tt-1
  
  # Update catches for each fleet
  for( f in levels(as.factor(newOMdat$catch$fleet)) ){
    newOMdat$catch[newOMdat$catch$year==endyr & newOMdat$catch$fleet==f, "catch"] =
      get(paste0("F", f, "catch"))
  } # end fishing fleet f loop
  
  
  # update Lfreqs for each fleet/survey
  for(fs in levels(as.factor(newOMdat$lencomp$FltSvy)) ){
    newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Nsamp"] = get(paste0("NsampF",fs))
    
    # IF Nsamp is zero or negative, set year = -year. 
    newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Yr"] = 
      ifelse(newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Nsamp"]<=0, 
             -1*newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Yr"], 
             newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Yr"])
    
    # IF Nsamp is > 1000, set year = -year. 
    newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Yr"] = 
      ifelse(newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Nsamp"]>1000, 
             -1*newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Yr"], 
             newOMdat$lencomp[newOMdat$lencomp$Yr==endyr & newOMdat$lencomp$FltSvy==fs, "Yr"])
  } # end Lfreq fs loop
  
  
  # limit number of digits
  newOMdat$catch<-round(newOMdat$catch, digits=5)
  newOMdat$lencomp<-round(newOMdat$lencomp, digits=4)
  
  
  # Re-write OM data file with additional FRQ years of data. 
  SS_writedat(newOMdat, outfile=paste0(OMdir,"\\SB.dat"), version="3.30", overwrite=T)

  
  #-------------------------------------------------------------------------------------------------------------
  # Save Results
  #-------------------------------------------------------------------------------------------------------------
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
  #                                               F3Catch = F3catch, 
  #                                               F4Catch = F4catch, 
  #                                               HCR_Fset = rep(hcr$Fset, FRQ),
  #                                               Estimated_FMSY = rep(modEM$derived_quants["Fstd_MSY","Value"], FRQ) ) )
  # 
  # assign("MSEResults", MSEResults, envir=globalenv())       # save MSEResults in global environment
  
  
}
