####################################
# Build .par file from posterior.sso
# Data from MCMC OM posterior.sso to
# create .par file for step-2 OM
####################################

#### already done in big function ####
# Load packages #
# library(data.table)
# library(r4ss)
# MCMCdir = "R:\\Management Strategy Evaluation\\SB\\SB_SS_run\\sandbar_330_OM_Base_MCMC_2Surveys_MCMC"
# MCMCdir = "D:\\MSE_COVID19_FILES_BACKUP\\MCMC\\sandbar_330_OM_BH_MCMC"
# mcmc = SSgetMCMC(dir=MCMCdir, writecsv=FALSE) # read in MCMC data
# OMdir = ""
# i=1



BuildParFile = function(MCMCdir, mcmc, i, OMdirs=list()){
  
  # Can optionally put in more than one OM directory in the form of a list to save the ss.par file in each OMdir (for multiple HCRs)
  
  
  ## read-in par file #
  parf = readLines(paste(MCMCdir,"\\ss.par",sep="")) # take par file from mcmc and save as newpar; edit newpar and save in OM 
  newpar = parf
  
  
  #### Biological/ Life History Parameters ####
  # NOTE: Check these on and off as appropriate...
  newpar[which(newpar=="# MGparm[1]:")+1] = mcmc[i,"L_at_Amin_Fem_GP_1"]
  newpar[which(newpar=="# MGparm[2]:")+1] = mcmc[i,"L_at_Amax_Fem_GP_1"]
  newpar[which(newpar=="# MGparm[3]:")+1] = mcmc[i,"VonBert_K_Fem_GP_1"]
  # newpar[which(newpar=="# MGparm[6]:")+1] = mcmc[i,"Wtlen_1_Fem_GP_1"]
  newpar[which(newpar=="# MGparm[7]:")+1] = mcmc[i,"Wtlen_2_Fem_GP_1"]
  # newpar[which(newpar=="# SR_parm[1]:")+1] = mcmc[i,"SR_LN(R0)"]
  #LFSR
  newpar[which(newpar=="# SR_parm[2]:")+1] = mcmc[i,"SR_surv_zfrac"]
  newpar[which(newpar=="# SR_parm[3]:")+1] = mcmc[i,"SR_surv_Beta"]
  # newpar[which(newpar=="# SR_parm[4]:")+1] = mcmc[i,"SR_autocorr"]
  #BH
  # newpar[which(newpar=="# SR_parm[2]:")+1] = mcmc[i,"SR_BH_steep"]
  
  
  #### Recruitment deviations ####
  
  EarlyRecDevs1 = c(as.matrix(mcmc[i,colnames(mcmc) %like% "Early_RecrDev_"]))
  MainRecDevs1 = c(as.matrix(mcmc[i,colnames(mcmc) %like% 'Main_RecrDev_']))
  LateRecDevs1 = c(as.matrix(mcmc[i,colnames(mcmc) %like% 'Late_RecrDev_']))
  ForecastRecDevs1 = c(as.matrix(mcmc[i,colnames(mcmc) %like% 'ForeRecr_']))
  
  newpar[which(newpar=="# recdev_early:")+1] = paste(EarlyRecDevs1, collapse=" ")
  newpar[which(newpar=="# recdev2:")+1] = paste(MainRecDevs1, collapse=" ")
  newpar[which(newpar=="# Fcast_recruitments:")+1] = paste(c(LateRecDevs1, ForecastRecDevs1), collapse=" ")
  
  
  
  
  #### Process error Parameters ####
  # Based on parameters with process-error: q and selectivity
  
  # q #
  newpar[which(newpar=="# Q_parm[1]:")+1] = mcmc[i, "LnQ_base_S1_LPS(5)"] 
  newpar[which(newpar=="# Q_parm[2]:")+1] = mcmc[i, "LnQ_base_S5_NMFS_LLSE(6)"] 
  
  
  # SELECTIVITY #
  #   subset mcmc parameters to just selectivity; based on size-structured selectivity
  
  # Based on 68 selectivity parameters
  #    Note: this if-else approach allows us to skip over values that won't change
  newpar[which(newpar=="# selparm[1]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_peak_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[1]:")+1], 
           mcmc[i,'Size_DblN_peak_F1_COM_GOM(1)'])
  
  newpar[which(newpar=="# selparm[2]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_top_logit_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[2]:")+1], 
           mcmc[i,'Size_DblN_top_logit_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[3]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_ascend_se_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[3]:")+1], 
           mcmc[i,'Size_DblN_ascend_se_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[4]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_descend_se_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[4]:")+1], 
           mcmc[i,'Size_DblN_descend_se_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[5]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_start_logit_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[5]:")+1], 
           mcmc[i,'Size_DblN_start_logit_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[6]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_end_logit_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[6]:")+1], 
           mcmc[i,'Size_DblN_end_logit_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[7]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Peak_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[7]:")+1], 
           mcmc[i,'SzSel_Male_Peak_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[8]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Ascend_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[8]:")+1], 
           mcmc[i,'SzSel_Male_Ascend_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[9]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Descend_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[9]:")+1], 
           mcmc[i,'SzSel_Male_Descend_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[10]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Final_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[10]:")+1], 
           mcmc[i,'SzSel_Male_Final_F1_COM_GOM(1)']) 
  
  newpar[which(newpar=="# selparm[11]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Scale_F1_COM_GOM(1)')==T, newpar[which(newpar=="# selparm[11]:")+1], 
           mcmc[i,'SzSel_Male_Scale_F1_COM_GOM(1)']) 
  
  #
  newpar[which(newpar=="# selparm[12]:") +1] = 
    ifelse(is.null(mcmc$'Size_inflection_F2_COM_SA(2)')==T, newpar[which(newpar=="# selparm[12]:")+1], 
           mcmc[i,'Size_inflection_F2_COM_SA(2)']) 
  
  newpar[which(newpar=="# selparm[13]:") +1] = 
    ifelse(is.null(mcmc$'Size_95%width_F2_COM_SA(2)')==T, newpar[which(newpar=="# selparm[13]:")+1], 
           mcmc[i,'Size_95%width_F2_COM_SA(2)']) 
  
  #
  newpar[which(newpar=="# selparm[14]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_peak_F3_RecMEX(3)')==T, newpar[which(newpar=="# selparm[14]:")+1], 
           mcmc[i,'Size_DblN_peak_F3_RecMEX(3)']) 
  
  newpar[which(newpar=="# selparm[15]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_top_logit_F3_RecMEX(3)')==T, newpar[which(newpar=="# selparm[15]:")+1], 
           mcmc[i,'Size_DblN_top_logit_F3_RecMEX(3)']) 
  
  newpar[which(newpar=="# selparm[16]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_ascend_se_F3_RecMEX(3)')==T, newpar[which(newpar=="# selparm[16]:")+1], 
           mcmc[i,'Size_DblN_ascend_se_F3_RecMEX(3)']) 
  
  newpar[which(newpar=="# selparm[17]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_descend_se_F3_RecMEX(3)')==T, newpar[which(newpar=="# selparm[17]:")+1], 
           mcmc[i,'Size_DblN_descend_se_F3_RecMEX(3)']) 
  
  newpar[which(newpar=="# selparm[18]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_start_logit_F3_RecMEX(3)')==T, newpar[which(newpar=="# selparm[18]:")+1], 
           mcmc[i,'Size_DblN_start_logit_F3_RecMEX(3)']) 
  
  newpar[which(newpar=="# selparm[19]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_end_logit_F3_RecMEX(3)')==T, newpar[which(newpar=="# selparm[19]:")+1], 
           mcmc[i,'Size_DblN_end_logit_F3_RecMEX(3)']) 
  
  #
  newpar[which(newpar=="# selparm[20]:") +1] = 
    ifelse(is.null(mcmc$'Size_inflection_F4_MEN_DSC(4)')==T, newpar[which(newpar=="# selparm[20]:")+1], 
           mcmc[i,'Size_inflection_F4_MEN_DSC(4)']) 
  
  ## NOTE!!! The priorSD was too large so producing wild values. Modify mcmc iterations by reducing SD by half
  mcmc$`Size_95%width_F4_MEN_DSC(4)` = (((mcmc$`Size_95%width_F4_MEN_DSC(4)`)-1)/2.5)+1
  newpar[which(newpar=="# selparm[21]:") +1] = 
    ifelse(is.null(mcmc$'Size_95%width_F4_MEN_DSC(4)')==T, newpar[which(newpar=="# selparm[21]:")+1], 
           mcmc[i,'Size_95%width_F4_MEN_DSC(4)']) 
  
  #
  newpar[which(newpar=="# selparm[22]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_peak_S1_LPS(5)')==T, newpar[which(newpar=="# selparm[22]:")+1], 
           mcmc[i,'Size_DblN_peak_S1_LPS(5)']) 
  
  newpar[which(newpar=="# selparm[23]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_top_logit_S1_LPS(5)')==T, newpar[which(newpar=="# selparm[23]:")+1], 
           mcmc[i,'Size_DblN_top_logit_S1_LPS(5)']) 
  
  newpar[which(newpar=="# selparm[24]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_ascend_se_S1_LPS(5)')==T, newpar[which(newpar=="# selparm[24]:")+1], 
           mcmc[i,'Size_DblN_ascend_se_S1_LPS(5)']) 
  
  newpar[which(newpar=="# selparm[25]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_descend_se_S1_LPS(5)')==T, newpar[which(newpar=="# selparm[25]:")+1], 
           mcmc[i,'Size_DblN_descend_se_S1_LPS(5)']) 
  
  newpar[which(newpar=="# selparm[26]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_start_logit_S1_LPS(5)')==T, newpar[which(newpar=="# selparm[26]:")+1], 
           mcmc[i,'Size_DblN_start_logit_S1_LPS(5)']) 
  
  newpar[which(newpar=="# selparm[27]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_end_logit_S1_LPS(5)')==T, newpar[which(newpar=="# selparm[27]:")+1], 
           mcmc[i,'Size_DblN_end_logit_S1_LPS(5)'])
  
  #
  newpar[which(newpar=="# selparm[28]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_peak_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[28]:")+1], 
           mcmc[i,'Size_DblN_peak_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[29]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_top_logit_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[29]:")+1], 
           mcmc[i,'Size_DblN_top_logit_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[30]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_ascend_se_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[30]:")+1], 
           mcmc[i,'Size_DblN_ascend_se_S5_NMFS_LLSE(6)'])
  
  newpar[which(newpar=="# selparm[31]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_descend_se_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[31]:")+1], 
           mcmc[i,'Size_DblN_descend_se_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[32]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_start_logit_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[32]:")+1], 
           mcmc[i,'Size_DblN_start_logit_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[33]:") +1] = 
    ifelse(is.null(mcmc$'Size_DblN_end_logit_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[33]:")+1], 
           mcmc[i,'Size_DblN_end_logit_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[34]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Peak_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[34]:")+1], 
           mcmc[i,'SzSel_Male_Peak_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[35]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Ascend_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[35]:")+1], 
           mcmc[i,'SzSel_Male_Ascend_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[36]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Descend_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[36]:")+1], 
           mcmc[i,'SzSel_Male_Descend_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[37]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Final_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[37]:")+1], 
           mcmc[i,'SzSel_Male_Final_S5_NMFS_LLSE(6)']) 
  
  newpar[which(newpar=="# selparm[38]:") +1] = 
    ifelse(is.null(mcmc$'SzSel_Male_Scale_S5_NMFS_LLSE(6)')==T, newpar[which(newpar=="# selparm[38]:")+1], 
           mcmc[i,'SzSel_Male_Scale_S5_NMFS_LLSE(6)']) 
  
  
  
  
  
  #### TIMEVARYING PARAMETERS ####
  
  #### NOTE THIS IS SPECIFICALLY FOR WHEN M IS FIXED AND TIME_VARYING Q AND SELECTIVITY PARAMS. 
  ###     THis order of parameters may change when more time-varying parameters are added. 
  # time varying parameters
  # parm_dev  parameter
  # 1	  LnQ_base_S1(5)_dev_se
  # 2	  LnQ_base_S2(6)_dev_se
  # 3	  #SizeSel_P1_F1_COM_GOM(1)_dev_se
  # 4	  #SizeSel_P3_F1_COM_GOM(1)_dev_se
  # 5	  #SizeSel_P4_F1_COM_GOM(1)_dev_se
  # 6	  #SizeSel_P1_F2_COM_SA(2)_dev_se
  # 7	  #SizeSel_P2_F2_COM_SA(2)_dev_se
  # 8	  #SizeSel_P1_F3_RecMEX(3)_dev_se
  # 9	  #SizeSel_P4_F3_RecMEX(3)_dev_se
  # 10	#SizeSel_P1_F4_MEN_DSC(4)_dev_se
  # 11	#SizeSel_P2_F4_MEN_DSC(4)_dev_se
  # 12	#SizeSel_P1_S1_LPS(5)_dev_se
  # 13	#SizeSel_P3_S1_LPS(5)_dev_se
  # 14	#SizeSel_P4_S1_LPS(5)_dev_se
  # 15	#SizeSel_P1_S5_NMFS_LLSE(9)_dev_se
  # 16	#SizeSel_P3_S5_NMFS_LLSE(9)_dev_se
  # 17	#SizeSel_P4_S5_NMFS_LLSE(9)_dev_se
  
  
  #lnq1
  mcmcLnQ1A = mcmc[i, colnames(mcmc) %like% "LnQ_base_S1" ] 
  mcmcLnQ1 = mcmcLnQ1A[colnames(mcmcLnQ1A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[1]:")+1] =  paste(as.vector(unlist(mcmcLnQ1)), collapse=" ")
  #lnq2
  mcmcLnQ2A = mcmc[i, colnames(mcmc) %like% "LnQ_base_S5" ] 
  mcmcLnQ2 = mcmcLnQ2A[colnames(mcmcLnQ2A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[2]:")+1] =  paste(as.vector(unlist(mcmcLnQ2)), collapse=" ")
  
  #Selectivity
  #Size_DblN_peak_F1_COM_GOM(1)
  mcmcSel1A = mcmc[i, colnames(mcmc) %like% "Size_DblN_peak_F1_COM_GOM" ]
  mcmcSel1 = mcmcSel1A[colnames(mcmcSel1A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[3]:")+1] =  paste(as.vector(unlist(mcmcSel1)), collapse=" ")
  
  #Size_DblN_ascend_se_F1_COM_GOM(1)
  mcmcSel2A = mcmc[i, colnames(mcmc) %like% "Size_DblN_ascend_se_F1_COM_GOM" ]
  mcmcSel2 = mcmcSel2A[colnames(mcmcSel2A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[4]:")+1] =  paste(as.vector(unlist(mcmcSel2)), collapse=" ")
  
  #Size_DblN_descend_se_F1_COM_GOM(1)
  mcmcSel3A = mcmc[i, colnames(mcmc) %like% "Size_DblN_descend_se_F1_COM_GOM" ]
  mcmcSel3 = mcmcSel3A[colnames(mcmcSel3A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[5]:")+1] =  paste(as.vector(unlist(mcmcSel3)), collapse=" ")
  
  #Size_inflection_F2_COM_SA(2)
  mcmcSel4A = mcmc[i, colnames(mcmc) %like% "Size_inflection_F2_COM_SA" ]
  mcmcSel4 = mcmcSel4A[colnames(mcmcSel4A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[6]:")+1] =  paste(as.vector(unlist(mcmcSel4)), collapse=" ")
  
  #Size_95%width_F2_COM_SA(2)
  mcmcSel5A = mcmc[i, colnames(mcmc) %like% "Size_95%width_F2_COM_SA" ]
  mcmcSel5 = mcmcSel5A[colnames(mcmcSel5A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[7]:")+1] =  paste(as.vector(unlist(mcmcSel5)), collapse=" ")
  
  #Size_DblN_peak_F3_RecMEX(3)
  mcmcSel6A = mcmc[i, colnames(mcmc) %like% "Size_DblN_peak_F3_RecMEX" ]
  mcmcSel6 = mcmcSel6A[colnames(mcmcSel6A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[8]:")+1] =  paste(as.vector(unlist(mcmcSel6)), collapse=" ")
  
  #Size_DblN_descend_se_F3_RecMEX(3)
  mcmcSel7A = mcmc[i, colnames(mcmc) %like% "Size_DblN_descend_se_F3_RecMEX" ]
  mcmcSel7 = mcmcSel7A[colnames(mcmcSel7A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[9]:")+1] =  paste(as.vector(unlist(mcmcSel7)), collapse=" ")
  
  #Size_inflection_F4_MEN_DSC(4)
  mcmcSel8A = mcmc[i, colnames(mcmc) %like% "Size_inflection_F4_MEN_DSC" ]
  mcmcSel8 = mcmcSel8A[colnames(mcmcSel8A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[10]:")+1] =  paste(as.vector(unlist(mcmcSel8)), collapse=" ")
  
  #Size_95%width_F4_MEN_DSC(4)
  mcmcSel9A = mcmc[i, colnames(mcmc) %like% "Size_95%width_F4_MEN_DSC" ]
  mcmcSel9 = mcmcSel9A[colnames(mcmcSel9A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[11]:")+1] =  paste(as.vector(unlist(mcmcSel9)), collapse=" ")
  
  #Size_DblN_peak_S1_LPS(5)
  mcmcSel10A = mcmc[i, colnames(mcmc) %like% "Size_DblN_peak_S1_LPS" ]
  mcmcSel10 = mcmcSel10A[colnames(mcmcSel10A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[12]:")+1] =  paste(as.vector(unlist(mcmcSel10)), collapse=" ")
  
  #Size_DblN_ascend_se_S1_LPS(5)
  mcmcSel11A = mcmc[i, colnames(mcmc) %like% "Size_DblN_ascend_se_S1_LPS" ]
  mcmcSel11 = mcmcSel11A[colnames(mcmcSel11A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[13]:")+1] =  paste(as.vector(unlist(mcmcSel11)), collapse=" ")
  
  #Size_DblN_descend_se_S1_LPS(5)
  mcmcSel12A = mcmc[i, colnames(mcmc) %like% "Size_DblN_descend_se_S1_LPS" ]
  mcmcSel12 = mcmcSel12A[colnames(mcmcSel12A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[14]:")+1] =  paste(as.vector(unlist(mcmcSel12)), collapse=" ")
  
  #Size_DblN_peak_S5_NMFS_LLSE(6)
  mcmcSel13A = mcmc[i, colnames(mcmc) %like% "Size_DblN_peak_S5_NMFS_LLSE" ]
  mcmcSel13 = mcmcSel13A[colnames(mcmcSel13A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[15]:")+1] =  paste(as.vector(unlist(mcmcSel13)), collapse=" ")
  
  #Size_DblN_ascend_se_S5_NMFS_LLSE(6)
  mcmcSel14A = mcmc[i, colnames(mcmc) %like% "Size_DblN_ascend_se_S5_NMFS_LLSE" ]
  mcmcSel14 = mcmcSel14A[colnames(mcmcSel14A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[16]:")+1] =  paste(as.vector(unlist(mcmcSel14)), collapse=" ")
  
  #Size_DblN_descend_se_S5_NMFS_LLSE(6)
  mcmcSel15A = mcmc[i, colnames(mcmc) %like% "Size_DblN_descend_se_S5_NMFS_LLSE" ]
  mcmcSel15 = mcmcSel15A[colnames(mcmcSel15A) %like% "_DEV_MR_rwalk_"]
  newpar[which(newpar=="# parm_dev[17]:")+1] =  paste(as.vector(unlist(mcmcSel15)), collapse=" ")
  
  
  ###########
  
  #... add other parameters as necessary for each example. 
  
  
  
  for(OMD in OMdirs){
    writeLines(newpar, paste(OMD,"\\ss.par",sep=""))
  }
  
  
  return(paste("Saved new ss.par file(s)"))
} # end BuildParFile function


