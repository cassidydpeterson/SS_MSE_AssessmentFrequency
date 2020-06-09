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
  comACL = hcr$ACL / 2
  
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
  
  
  # RECREATIONAL AND DISCARDS #
  # F3 #
  # OLD WAY #
  
  #     estimate next year's EM forecasted population size
  #     parameters are based on a linear regression between EM observed biomass and F3 catch
  # lr3 = lm(modOM$timeseries[modOM$timeseries$Yr>1994 & modOM$timeseries$Yr<2016,]$`dead(B):_3` ~
  #            modEM$timeseries[modEM$timeseries$Yr>1994 & modEM$timeseries$Yr<2016,]$Bio_all)
  # F3exp = (lr3$coefficients[1]) + ( (lr3$coefficients[2] )*modEM$timeseries[modEM$timeseries$Yr==tt-1,]$Bio_all )

  # NEW WAY / NEW IMPLEMENT HCR CODE # 
  F3exp <- hcr$ACL / 2
  F3catch = F3exp + rnorm(FRQ,0,102.0899 / 5 )                     # actual F3 catch w implementation uncertainty; true sd too hgih
  # # note: chosing to not include uncertainty at this stage, because the data generating process will add appropriate uncertainty;
  #       included b/c more consistent with historical data
  
  F3catch = ifelse(F3catch<0, 0, F3catch)
  ###     NOTE: I am cheating and reducing SD by an order of magnitude; otherwise very crazy results
  
  
  # F4 #
  #     estimate next year's EM forecasted population size
  #     parameters are based on a linear regression between EM observed biomass and F4 catch
  # F4exp = (0.2286625) + ((5.37979e-06)*modOM$timeseries[modOM$timeseries$Yr==tt-1,]$Bio_all)     # expected F4 catch
  lr4 = lm(modOM$timeseries[modOM$timeseries$Yr>1994 & modOM$timeseries$Yr<2016,]$`dead(N):_4` ~
             modEM$timeseries[modEM$timeseries$Yr>1994 & modEM$timeseries$Yr<2016,]$Bio_all)
  F4exp = (  lr4$coefficients[1] ) + ( ( lr4$coefficients[2] )*modOM$timeseries[modOM$timeseries$Yr==tt-1,]$Bio_all )     # expected F4 catch
  # F4catch = rep(as.numeric(F4exp), FRQ)
  # F4exp = (  0.1964266 ) + ((6.277744e-06)*modOM$timeseries[modOM$timeseries$Yr==tt-1,]$Bio_all)     # expected F4 catch
  F4catch = F4exp + rnorm(FRQ,0,0.04200714/2 )                         # actual F3 catch w implementation uncertainty; true sd too high
  # # note: considered chosing to not include uncertainty at this stage, because the data generating process will add appropriate uncertainty; 
  #       included b/c more consistent with historical data
  
  F4catch = ifelse(F4catch<0, 0, F4catch)
  
  
  
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
  F3c = ifelse(F3catch==0, 1e-5, F3catch)
  NsampF3 = round( (  1.221945 * log(F3c)  ) + -3.150376 + rnorm(FRQ, 0, 1.907433) )
  NsampF3 = ifelse(NsampF3<0, 0, NsampF3)
  
  # Surveys #
  # S1 #
  #  based on empirically estimated linear relationship between log(Nsamp) and predicted biomass
  NsampF5 = round(exp( -0.7597465 + (4.564626e-05 * modEM$timeseries$Bio_all[ nrow(modEM$timeseries) ] ) + rnorm(FRQ, 0, 0.5243653) + 0.1546644))
  
  # S2 # 
  # sample from a truncated normal distribution (with bounds to restrict observations within realm of observed data)
  NsampF6 = round( rtnorm(FRQ, 8.074, 4.609146 , 0, 25) )
  
  
  #-------------------------------------------------------------------------------------------------------------
  # Save Results
  #-------------------------------------------------------------------------------------------------------------
  ### SAVE RESULTS ###
  
  # Create MSEResults$ACL_i (if it doesn't already exist)
  if(is.null(MSEResults[[paste0("ACL_",i)]]) == TRUE ){
    MSEResults[[paste0("ACL_",i)]] = 
      data.frame(Year=numeric(), ACL_from_HCR=numeric(), corrected_ACL=numeric(), Implemented_Catch=numeric(), F1Catch = numeric(), 
                 F2Catch=numeric(), F3Catch=numeric(), F4Catch=numeric(), HCR_Fset=numeric(), Estimated_FMSY=numeric())
  }
  
  # define endyr of EM assessed year
  endyr = tt-1
  
  # Save results in MSEResults tagged in the present iteration i
  MSEResults[[paste0("ACL_",i)]] <- rbind(MSEResults[[paste0("ACL_",i)]], 
                                          cbind(Year = endyr + c(1:FRQ), 
                                                ACL_from_HCR = rep(hcr$ACL, FRQ), 
                                                corrected_ACL = rep(comACL,5), 
                                                Implemented_Catch = actualCatch, 
                                                F1Catch = F1catch, 
                                                F2Catch = F2catch, 
                                                F3Catch = F3catch, 
                                                F4Catch = F4catch, 
                                                HCR_Fset = rep(hcr$Fset, FRQ),
                                                Estimated_FMSY = rep(modEM$derived_quants["Fstd_MSY","Value"], FRQ) ) )
  
  assign("MSEResults", MSEResults, envir=globalenv())       # save MSEResults in global environment
  
  
  #-------------------------------------------------------------------------------------------------------------
  # Add new data to OM
  #-------------------------------------------------------------------------------------------------------------
  # Put future catches into OM
  
  OMdat = SS_readdat(file=paste0(OMdir,"\\SB.dat"), version="3.30") 
  newOMdat = OMdat
  endyr = tt-1
  for(y in (endyr+1):(endyr+FRQ)){
    
    # Update catches for each fleet
    for( f in levels(as.factor(newOMdat$catch$fleet)) ){
      newOMdat$catch[newOMdat$catch$year==y & newOMdat$catch$fleet==f, "catch"] =
        get(paste0("F", f, "catch"))[y-endyr]
    } # end fishing fleet f loop
    
    
    # update Lfreqs for each fleet/survey
    for(fs in levels(as.factor(newOMdat$lencomp$FltSvy)) ){
      newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Nsamp"] = get(paste0("NsampF",fs))[y-endyr]
      
      # IF Nsamp is zero or negative, set year = -year. 
      newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Yr"] = 
        ifelse(newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Nsamp"]<=0, 
               -1*newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Yr"], 
               newOMdat$lencomp[newOMdat$lencomp$Yr==y & newOMdat$lencomp$FltSvy==fs, "Yr"])
    } # end Lfreq fs loop
    
    
  } # end year loop
  
  
  
  # Re-write OM data file with additional FRQ years of data. 
  SS_writedat(newOMdat, outfile=paste0(OMdir,"\\SB.dat"), version="3.30", overwrite=T)
  
  
  # return(MSEResults = MSEResults)
} # End implementHCR function


