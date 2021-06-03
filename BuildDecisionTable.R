###########################
### COllate MSE Results ###
###  May 2021 #####
###########################


Results = c('Results_Base_HiMexRec', 'Results_BH_HiMexRec', 'Results_Hih_HiMexRec', 'Results_Loh_HiMexRec', 'Results_lnR0_HiMexRec', 'Results_M_BH_HiMexRec')


BuildTable <- function(Results = c('Results_Base_HiMexRec', 'Results_BH_HiMexRec', 'Results_Hih_HiMexRec', 'Results_Loh_HiMexRec', 'Results_lnR0_HiMexRec', 'Results_M_BH_HiMexRec'), stat = "median"){
  ### AAV ###
  # AAV = rbind(Results_BASE$AAV_HCR, Results_BH$AAV_HCR, Results_Hih$AAV_HCR, Results_Loh$AAV_HCR, Results_lnR0$AAV_HCR, Results_M_BH$AAV_HCR) 
  # AAV
  AAV = get(Results[1])$AAV_FRQ
  for(r in 2:length(Results)){
    AAV <- rbind(AAV, get(Results[r])$AAV_FRQ)
  }
  
  
  
  
  ### Tot commercial catch ###
  # TotComCatch = rbind(Results_BASE$Com_Catch_cumulative, Results_BH$Com_Catch_cumulative, Results_Hih$Com_Catch_cumulative, Results_Loh$Com_Catch_cumulative, Results_lnR0$Com_Catch_cumulative, Results_M_BH$Com_Catch_cumulative) 
  # TotComCatch
  TotComCatch = get(Results[1])$Com_Catch_cumulative
  for(r in 2:length(Results)){
    TotComCatch <- rbind(TotComCatch, get(Results[r])$Com_Catch_cumulative)
  }
  
  
  
  
  ### SSB/SSBMSY ###
  # SSB_SSBMSY_2115 = rbind(Results_BASE$SSB_SSBMSY_2115, Results_BH$SSB_SSBMSY_2115, Results_Hih$SSB_SSBMSY_2115, Results_Loh$SSB_SSBMSY_2115, Results_lnR0$SSB_SSBMSY_2115, Results_M_BH$SSB_SSBMSY_2115) 
  # dim(SSB_SSBMSY_2115)
  SSB_SSBMSY_2115 = get(Results[1])$SSB_SSBMSY_2115
  for(r in 2:length(Results)){
    SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
  }
  
  # SSB_SSBMSY_2065 = rbind(Results_BASE$SSB_SSBMSY_2065, Results_BH$SSB_SSBMSY_2065, Results_Hih$SSB_SSBMSY_2065, Results_Loh$SSB_SSBMSY_2065, Results_lnR0$SSB_SSBMSY_2065, Results_M_BH$SSB_SSBMSY_2065) 
  # dim(SSB_SSBMSY_2065)
  SSB_SSBMSY_2065 = get(Results[1])$SSB_SSBMSY_2065
  for(r in 2:length(Results)){
    SSB_SSBMSY_2065 <- rbind(SSB_SSBMSY_2065, get(Results[r])$SSB_SSBMSY_2065)
  }
  
  
  
  
  ### F/FMSY ##
  # F_FMSY_2115 = rbind(Results_BASE$FM_FMMSY_2115, Results_BH$FM_FMMSY_2115, Results_Hih$FM_FMMSY_2115, Results_Loh$FM_FMMSY_2115, Results_lnR0$FM_FMMSY_2115, Results_M_BH$FM_FMMSY_2115) 
  # dim(F_FMSY_2115)
  F_FMSY_2115 = get(Results[1])$FM_FMMSY_2115
  for(r in 2:length(Results)){
    F_FMSY_2115 <- rbind(F_FMSY_2115, get(Results[r])$FM_FMMSY_2115)
  }
  
  
  # F_FMSY_2065 = rbind(Results_BASE$FM_FMMSY_2065, Results_BH$FM_FMMSY_2065, Results_Hih$FM_FMMSY_2065, Results_Loh$FM_FMMSY_2065, Results_lnR0$FM_FMMSY_2065, Results_M_BH$FM_FMMSY_2065) 
  # dim(F_FMSY_2065)
  # View(F_FMSY_2065)
  F_FMSY_2065 = get(Results[1])$FM_FMMSY_2065
  for(r in 2:length(Results)){
    F_FMSY_2065 <- rbind(F_FMSY_2065, get(Results[r])$FM_FMMSY_2065)
  }
  
  
  
  
  
  ### Avg len F ###
  # AvgLen_F_2065 = rbind(Results_BASE$AvgLen_F_2065, Results_BH$AvgLen_F_2065, Results_Hih$AvgLen_F_2065, Results_Loh$AvgLen_F_2065, Results_lnR0$AvgLen_F_2065, Results_M_BH$AvgLen_F_2065) 
  # dim(AvgLen_F_2065)
  AvgLen_F_2065 = get(Results[1])$AvgLen_F_2065
  for(r in 2:length(Results)){
    AvgLen_F_2065 <- rbind(AvgLen_F_2065, get(Results[r])$AvgLen_F_2065)
  }
  
  
  # AvgLen_F_2115 = rbind(Results_BASE$AvgLen_F_2115, Results_BH$AvgLen_F_2115, Results_Hih$AvgLen_F_2115, Results_Loh$AvgLen_F_2115, Results_lnR0$AvgLen_F_2115, Results_M_BH$AvgLen_F_2115) 
  # dim(AvgLen_F_2115)
  # View(AvgLen_F_2115)
  AvgLen_F_2115 = get(Results[1])$AvgLen_F_2115
  for(r in 2:length(Results)){
    AvgLen_F_2115 <- rbind(AvgLen_F_2115, get(Results[r])$AvgLen_F_2115)
  }
  
  
  
  ### prob Recov 2115 ###
  # SSB_SSBMSY_2115
  Recov <- ifelse(SSB_SSBMSY_2115>1, 1, 0) 
  
  
  
  
  ### BUILD TABLE ####
  
  if(stat=="median"){
    table <- rbind( "Prob of recovery by 2115" = round( apply(Recov, 2, sum, na.rm=T) / nrow(Recov), digits=3) ,
                    "total US com. catch" = round( apply(TotComCatch, 2, median, na.rm=T), digits=2) ,
                    'SSB2065 / SSBMSY' = round( apply(SSB_SSBMSY_2065, 2, median, na.rm=T), digits=3) ,
                    'SSB2115 / SSBMSY'  = round( apply(SSB_SSBMSY_2115, 2, median, na.rm=T), digits=3) ,
                    'AAV' =  round( apply(AAV, 2, median, na.rm=T), digits=3) ,
                    'F2065 / FMSY' = round( apply(F_FMSY_2065, 2, median, na.rm=T), digits=3) ,
                    'F2115 / FMSY' = round( apply(F_FMSY_2115, 2, median, na.rm=T), digits=3) ,
                    "Fem Length 2065" = round( apply(AvgLen_F_2065, 2, median, na.rm=T), digits=2) , 
                    "Fem Length 2115" = round( apply(AvgLen_F_2115, 2, median, na.rm=T), digits=2)  )
    # table <- round(table, digits=3)
  } # if stat==median
  
  if(stat=="mean"){
    table <- rbind( "Prob of recovery by 2115" = round( apply(Recov, 2, sum, na.rm=T) / nrow(Recov), digits=3)  ,
                    "total US com. catch" = round( apply(TotComCatch, 2, mean, na.rm=T), digits=2) ,
                    'SSB2065 / SSBMSY' = round( apply(SSB_SSBMSY_2065, 2, mean, na.rm=T), digits=3) ,
                    'SSB2115 / SSBMSY'  = round( apply(SSB_SSBMSY_2115, 2, mean, na.rm=T), digits=3) ,
                    'AAV' =  round( apply(AAV, 2, mean, na.rm=T), digits=3) ,
                    'F2065 / FMSY' = round( apply(F_FMSY_2065, 2, mean, na.rm=T), digits=3) ,
                    'F2115 / FMSY' = round( apply(F_FMSY_2115, 2, mean, na.rm=T), digits=3) ,
                    "Fem Length 2065" = round( apply(AvgLen_F_2065, 2, mean, na.rm=T), digits=2) , 
                    "Fem Length 2115" = round( apply(AvgLen_F_2115, 2, mean, na.rm=T), digits=2)  )
    # table <- round(table, digits=3)
  } # if stat==mean
  
  return(list("table" = table))
  
} # END FUNCTION


BuildTable()



Results_Concept = c('Results_Base_Concept', 'Results_BH_Concept', 'Results_Hih_Concept', 
                    'Results_Loh_Concept', 'Results_lnR0_Concept', 'Results_M_BH_Concept')
BuildTable(Results=Results_Concept)
BuildTable(Results=Results_Concept, stat='mean')



Results_LoMexRec = c('Results_Base_LoMexRec', 'Results_BH_LoMexRec', 'Results_Hih_LoMexRec', 
                     'Results_Loh_LoMexRec', 'Results_lnR0_LoMexRec', 'Results_M_BH_LoMexRec')
BuildTable(Results=Results_LoMexRec)
BuildTable(Results=Results_LoMexRec, stat='mean')


######### BUILD TABLE PLOT ######
### Results are differentiated by a C (conceptual), H (hi), and L (lo)

for(Imp in c('C','H','L')){
  
  if(Imp=='C'){Results = c('Results_Base_Concept', 'Results_BH_Concept', 'Results_Hih_Concept', 
                           'Results_Loh_Concept', 'Results_lnR0_Concept', 'Results_M_BH_Concept')}
  if(Imp=='H'){Results = c('Results_Base_HiMexRec', 'Results_BH_HiMexRec', 'Results_Hih_HiMexRec', 
                           'Results_Loh_HiMexRec', 'Results_lnR0_HiMexRec', 'Results_M_BH_HiMexRec')}
  if(Imp=='L'){Results = c('Results_Base_LoMexRec', 'Results_BH_LoMexRec', 'Results_Hih_LoMexRec', 
                           'Results_Loh_LoMexRec', 'Results_lnR0_LoMexRec', 'Results_M_BH_LoMexRec')}
  
  ### AAV ###
  AAV = get(Results[1])$AAV_FRQ
  for(r in 2:length(Results)){
    AAV <- rbind(AAV, get(Results[r])$AAV_FRQ)
  }
  ### AAV_ALL ###
  AAV_ALL = get(Results[1])$AAV_all
  for(r in 2:length(Results)){
    AAV_ALL <- rbind(AAV_ALL, get(Results[r])$AAV_all)
  }
  
  
  ### Tot commercial catch ###
  TotComCatch = get(Results[1])$Com_Catch_cumulative
  for(r in 2:length(Results)){
    TotComCatch <- rbind(TotComCatch, get(Results[r])$Com_Catch_cumulative)
  }
  
  
  ### SSB/SSBMSY ###
  SSB_SSBMSY_2115 = get(Results[1])$SSB_SSBMSY_2115
  for(r in 2:length(Results)){
    SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
  }
  
  SSB_SSBMSY_2065 = get(Results[1])$SSB_SSBMSY_2065
  for(r in 2:length(Results)){
    SSB_SSBMSY_2065 <- rbind(SSB_SSBMSY_2065, get(Results[r])$SSB_SSBMSY_2065)
  }
  
  
  
  
  ### F/FMSY ##
  F_FMSY_2115 = get(Results[1])$FM_FMMSY_2115
  for(r in 2:length(Results)){
    F_FMSY_2115 <- rbind(F_FMSY_2115, get(Results[r])$FM_FMMSY_2115)
  }
  
  
  F_FMSY_2065 = get(Results[1])$FM_FMMSY_2065
  for(r in 2:length(Results)){
    F_FMSY_2065 <- rbind(F_FMSY_2065, get(Results[r])$FM_FMMSY_2065)
  }
  
  
  
  
  
  ### Avg len F ###
  AvgLen_F_2065 = get(Results[1])$AvgLen_F_2065
  for(r in 2:length(Results)){
    AvgLen_F_2065 <- rbind(AvgLen_F_2065, get(Results[r])$AvgLen_F_2065)
  }
  
  
  AvgLen_F_2115 = get(Results[1])$AvgLen_F_2115
  for(r in 2:length(Results)){
    AvgLen_F_2115 <- rbind(AvgLen_F_2115, get(Results[r])$AvgLen_F_2115)
  }
  
  
  
  ### prob Recov 2115 ###
  SSB_SSBMSY_2115
  Recov <- as.data.frame(ifelse(SSB_SSBMSY_2115>0.9, 1, 0) )
  ######################################## CONSIDER DEFINITION OF RECOVERED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  # ### Prob overfishing ###
  # POF_a = OM_Base_Concept$FRQ_1$FM_FMMSY[as.character(2016:2115),]
  # POF_b = ifelse(POF_a>1, 1, 0)
  # POFapply(POF_b, 2, sum) / nrow(POF_b)
  
  
  
  
  assign(paste0("Recov_",Imp), Recov)
  assign(paste0("PRecov_",Imp), apply(Recov, 2, sum, na.rm=T) / nrow(Recov))
  assign(paste0("POF_",Imp), get(Results[1])$ProbOF )
  assign(paste0("TotComCatch_",Imp), TotComCatch)
  assign(paste0("Med_TotComCatch_",Imp), apply(TotComCatch, 2, median, na.rm=T) )
  assign(paste0("SSB_SSBMSY_2065_",Imp), SSB_SSBMSY_2065)
  assign(paste0("SSB_SSBMSY_2115_",Imp), SSB_SSBMSY_2115)
  assign(paste0("Med_SSB_SSBMSY_2115_",Imp), apply(SSB_SSBMSY_2115, 2, median, na.rm=T) )
  assign(paste0("AAV_",Imp), AAV)
  assign(paste0("AAV_ALL_",Imp), AAV_ALL)
  assign(paste0("Med_AAV_",Imp), apply(AAV, 2, median, na.rm=T) )
  assign(paste0("F_FMSY_2065_",Imp), F_FMSY_2065)
  assign(paste0("F_FMSY_2115_",Imp), F_FMSY_2115)
  assign(paste0("Med_F_FMSY_2115_",Imp), apply(F_FMSY_2115, 2, median, na.rm=T) )
  assign(paste0("AvgLen_F_2065_",Imp), AvgLen_F_2065)
  assign(paste0("AvgLen_F_2115_",Imp), AvgLen_F_2115)
  assign(paste0("Med_AvgLen_F_2115_",Imp), apply(AvgLen_F_2115, 2, median, na.rm=T) )
  
} # end Imp loop 



### GET RESULTS IN DATASET FOR GLM ####
OM_vec = c(rep("Base", 100), rep("BH", 100) , rep("Hih", 100) , rep("Loh", 100) , rep("lnR0", 100) , rep("M_BH", 100) ) 


ResultsC = c('Results_Base_Concept', 'Results_BH_Concept', 'Results_Hih_Concept', 
             'Results_Loh_Concept', 'Results_lnR0_Concept', 'Results_M_BH_Concept')
ResultsH = c('Results_Base_HiMexRec', 'Results_BH_HiMexRec', 'Results_Hih_HiMexRec', 
             'Results_Loh_HiMexRec', 'Results_lnR0_HiMexRec', 'Results_M_BH_HiMexRec')
ResultsL = c('Results_Base_LoMexRec', 'Results_BH_LoMexRec', 'Results_Hih_LoMexRec', 
             'Results_Loh_LoMexRec', 'Results_lnR0_LoMexRec', 'Results_M_BH_LoMexRec')
Lab_vecL = c(rep(ResultsL[1], 100), rep(ResultsL[2], 100) , rep(ResultsL[3], 100) , rep(ResultsL[4], 100) , rep(ResultsL[5], 100) , rep(ResultsL[6], 100) ) 
Lab_vecC = c(rep(ResultsC[1], 100), rep(ResultsC[2], 100) , rep(ResultsC[3], 100) , rep(ResultsC[4], 100) , rep(ResultsC[5], 100) , rep(ResultsC[6], 100) ) 
Lab_vecH = c(rep(ResultsH[1], 100), rep(ResultsH[2], 100) , rep(ResultsH[3], 100) , rep(ResultsH[4], 100) , rep(ResultsH[5], 100) , rep(ResultsH[6], 100) ) 


# FOR LoMexRec
FRQ1L = as.data.frame(cbind(name=Lab_vecL, Imp=rep("LoMexRec", length(Lab_vecL)), OM=OM_vec, FRQ=rep(1, length(Lab_vecL)), TotComCatch = TotComCatch_L$FRQ_1, ProbRec=Recov_L$FRQ_1, SSB2115=SSB_SSBMSY_2115_L$FRQ_1, F2115=F_FMSY_2115_L$FRQ_1, AAV=as.data.frame(AAV_ALL_L)$FRQ_1, POF=POF_L$FRQ_1 ))
FRQ5L = as.data.frame(cbind(name=Lab_vecL, Imp=rep("LoMexRec", length(Lab_vecL)), OM=OM_vec, FRQ=rep(5, length(Lab_vecL)), TotComCatch = TotComCatch_L$FRQ_5, ProbRec=Recov_L$FRQ_5, SSB2115=SSB_SSBMSY_2115_L$FRQ_5, F2115=F_FMSY_2115_L$FRQ_5, AAV=as.data.frame(AAV_ALL_L)$FRQ_5, POF=POF_L$FRQ_5 ))
FRQ10L = as.data.frame(cbind(name=Lab_vecL, Imp=rep("LoMexRec", length(Lab_vecL)), OM=OM_vec, FRQ=rep(10, length(Lab_vecL)), TotComCatch = TotComCatch_L$FRQ_10, ProbRec=Recov_L$FRQ_10, SSB2115=SSB_SSBMSY_2115_L$FRQ_10, F2115=F_FMSY_2115_L$FRQ_10, AAV=as.data.frame(AAV_ALL_L)$FRQ_10,  POF=POF_L$FRQ_15 ))
FRQ15L = as.data.frame(cbind(name=Lab_vecL, Imp=rep("LoMexRec", length(Lab_vecL)), OM=OM_vec, FRQ=rep(15, length(Lab_vecL)), TotComCatch = TotComCatch_L$FRQ_15, ProbRec=Recov_L$FRQ_15, SSB2115=SSB_SSBMSY_2115_L$FRQ_15, F2115=F_FMSY_2115_L$FRQ_15, AAV=as.data.frame(AAV_ALL_L)$FRQ_15,  POF=POF_L$FRQ_15 ))

# FOR HiMexRec
FRQ1H = as.data.frame(cbind(name=Lab_vecH, Imp=rep("HiMexRec", length(Lab_vecH)), OM=OM_vec, FRQ=rep(1, length(Lab_vecH)), TotComCatch = TotComCatch_H$FRQ_1, ProbRec=Recov_H$FRQ_1, SSB2115=SSB_SSBMSY_2115_H$FRQ_1, F2115=F_FMSY_2115_H$FRQ_1, AAV=as.data.frame(AAV_ALL_H)$FRQ_1,  POF=POF_H$FRQ_1 ))
FRQ5H = as.data.frame(cbind(name=Lab_vecH, Imp=rep("HiMexRec", length(Lab_vecH)), OM=OM_vec, FRQ=rep(5, length(Lab_vecH)), TotComCatch = TotComCatch_H$FRQ_5, ProbRec=Recov_H$FRQ_5, SSB2115=SSB_SSBMSY_2115_H$FRQ_5, F2115=F_FMSY_2115_H$FRQ_5, AAV=as.data.frame(AAV_ALL_H)$FRQ_5,  POF=POF_H$FRQ_5 ))
FRQ10H = as.data.frame(cbind(name=Lab_vecH, Imp=rep("HiMexRec", length(Lab_vecH)), OM=OM_vec, FRQ=rep(10, length(Lab_vecH)), TotComCatch = TotComCatch_H$FRQ_10, ProbRec=Recov_H$FRQ_10, SSB2115=SSB_SSBMSY_2115_H$FRQ_10, F2115=F_FMSY_2115_H$FRQ_10, AAV=as.data.frame(AAV_ALL_H)$FRQ_10,  POF=POF_H$FRQ_10 ))
FRQ15H = as.data.frame(cbind(name=Lab_vecH, Imp=rep("HiMexRec", length(Lab_vecH)), OM=OM_vec, FRQ=rep(15, length(Lab_vecH)), TotComCatch = TotComCatch_H$FRQ_15, ProbRec=Recov_H$FRQ_15, SSB2115=SSB_SSBMSY_2115_H$FRQ_15, F2115=F_FMSY_2115_H$FRQ_15, AAV=as.data.frame(AAV_ALL_H)$FRQ_15,  POF=POF_H$FRQ_15 ))

# FOR Concept
FRQ1C = as.data.frame(cbind(name=Lab_vecC, Imp=rep("Concept", length(Lab_vecC)), OM=OM_vec, FRQ=rep(1, length(Lab_vecC)), TotComCatch = TotComCatch_C$FRQ_1, ProbRec=Recov_C$FRQ_1, SSB2115=SSB_SSBMSY_2115_C$FRQ_1, F2115=F_FMSY_2115_C$FRQ_1, AAV=as.data.frame(AAV_ALL_C)$FRQ_1,  POF=POF_C$FRQ_1 ))
FRQ5C = as.data.frame(cbind(name=Lab_vecC, Imp=rep("Concept", length(Lab_vecC)), OM=OM_vec, FRQ=rep(5, length(Lab_vecC)), TotComCatch = TotComCatch_C$FRQ_5, ProbRec=Recov_C$FRQ_5, SSB2115=SSB_SSBMSY_2115_C$FRQ_5, F2115=F_FMSY_2115_C$FRQ_5, AAV=as.data.frame(AAV_ALL_C)$FRQ_5,  POF=POF_C$FRQ_5 ))
FRQ10C = as.data.frame(cbind(name=Lab_vecC, Imp=rep("Concept", length(Lab_vecC)), OM=OM_vec, FRQ=rep(10, length(Lab_vecC)), TotComCatch = TotComCatch_C$FRQ_10, ProbRec=Recov_C$FRQ_10, SSB2115=SSB_SSBMSY_2115_C$FRQ_10, F2115=F_FMSY_2115_C$FRQ_10, AAV=as.data.frame(AAV_ALL_C)$FRQ_10,  POF=POF_C$FRQ_10 ))
FRQ15C = as.data.frame(cbind(name=Lab_vecC, Imp=rep("Concept", length(Lab_vecC)), OM=OM_vec, FRQ=rep(15, length(Lab_vecC)), TotComCatch = TotComCatch_C$FRQ_15, ProbRec=Recov_C$FRQ_15, SSB2115=SSB_SSBMSY_2115_C$FRQ_15, F2115=F_FMSY_2115_C$FRQ_15, AAV=as.data.frame(AAV_ALL_C)$FRQ_15,  POF=POF_C$FRQ_15 ))

FRQ_DATA = rbind(FRQ1C, FRQ5C, FRQ10C, FRQ15C,
                 FRQ1L, FRQ5L, FRQ10L, FRQ15L,
                 FRQ1H, FRQ5H, FRQ10H, FRQ15H)

save(FRQ_DATA, file="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\FRQ_DATA.RData")




# PLOT

png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\Barplot_Decision_Table.png",
    type="cairo",
    units="mm",
    width=100,
    height=250,
    pointsize=22,
    res=300)
#####
par(mfrow=c(7,1),  mar=c(0, 0, 0, 0),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1, oma = c(2.5, 2, 2.5, 0.1))

#Prob of recovery 
barplot(as.table(rbind(PRecov_C[1:4],  PRecov_L[1:4], PRecov_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,1),ylab=NULL, names.arg=NULL, axisnames=F, axes=F, cex.axis = 0.5)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
abline(h=0.7, col='grey35')
box()
mtext("Prob Recover", 2, line=0.75, cex=0.7)
mtext("Graphical Decision Table", 3, line=0.75, cex=1)


# par()
# legend


#Prob of overfishing 
barplot(as.table(rbind(apply(POF_C,2,median)[1:4],  apply(POF_L,2,median)[1:4], apply(POF_H,2,median)[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,1),ylab=NULL, names.arg=NULL, axisnames=F, axes=F, cex.axis = 0.5)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
# abline(h=0.7, col='grey35')
box()
mtext("Prob Overfishing", 2, line=0.75, cex=0.7)


# Total US Commercial catch 
barplot(as.table(rbind(Med_TotComCatch_C[1:4],  Med_TotComCatch_L[1:4], Med_TotComCatch_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,16000),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext("US Catch", 2, line=0.75, cex=0.7)





# SSB / SSB_MSY 
barplot(as.table(rbind(Med_SSB_SSBMSY_2115_C[1:4],  Med_SSB_SSBMSY_2115_L[1:4], Med_SSB_SSBMSY_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0.5,1.5),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1, col='grey35')
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext(expression("SSB"[2115]*"/SSB"["MSY"]), 2, line=0.75, cex=0.7)





# F / F_MSY 
barplot(as.table(rbind(Med_F_FMSY_2115_C[1:4],  Med_F_FMSY_2115_L[1:4], Med_F_FMSY_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,1.05),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1, col='grey35')
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext(expression("F"[2115]*"/F"["MSY"]), 2, line=0.75, cex=0.7)




# AAV
barplot(as.table(rbind(Med_AAV_C[1:4],  Med_AAV_L[1:4], Med_AAV_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,0.5),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1, col='grey35')
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext("AAV", 2, line=0.75, cex=0.7)







# length
barplot(as.table(rbind(Med_AvgLen_F_2115_C[1:4],  Med_AvgLen_F_2115_L[1:4], Med_AvgLen_F_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(100,105),ylab=NULL, names.arg=NULL,  axes=F, cex.names = 0.7)
abline(h=1)
axis(2, cex.axis=0.7)
axis(1, labels=F)
box()
mtext("Avg. Len", 2, line=0.75, cex=0.7)




add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

add_legend("bottom", legend=c("Concept", "LoMexRec","HiMexRec"), pch=15, 
           col=c("grey","dodgerblue","red"),
           horiz=TRUE, bty='n', cex=0.8, pt.cex=2, inset=c(0.2, 0))


##########
dev.off()




