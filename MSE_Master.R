####################
# MSE in SS for
# Sandbar shark 
# MASTER
# C.Peterson
# UPDATED: 5/2020
####################

# NOTE: before this code is implemented, the MCMC should be run and checked. The base OM & EM should be basically generated. 
# Build .bat file that sets up folders and copies base files into each folder. See relevant helper .word file for diagram.

# !!!!! think about parallelization? #



#-------------------------------------------------------------------------------------------------------------
# Define relevant parameters and directories
#-------------------------------------------------------------------------------------------------------------
# 
# MCMCdir = "R:\\Management Strategy Evaluation\\SB\\MSE_RUN\\RUN_MCMC\\sandbar_330_OM_Base_MCMC" # OM version that undergoes MCMC directory
# OMdir = "R:\\Management Strategy Evaluation\\SB\\MSE_RUN\\OM_Base\\HCR1\\OM" # OM directory
# EMdir = "R:\\Management Strategy Evaluation\\SB\\MSE_RUN\\OM_Base\\HCR1\\EM" # EM directory
# StoreResults = "R:\\Management Strategy Evaluation\\SB\\MSE_RUN\\OM_Base\\HCR1\\StoreResults" # Directory to store results
# sourcedir = "R:\\Management Strategy Evaluation\\SB\\MSE_RUN\\"
# 
# FRQ=5   # frequency of assessments 
#         # (constant ACL -- less of a big deal for longer-lived species cite: Quang et al. 2020 F&F)
# 
# # HCR parameters -- for threshold control rule
# #         Will change for each HCR iteration
# Btarg="BMSY" # reference Btarg: options include: BMSY, B0, BSPR
# Bconst = 1
# Ftarg="FMSY"   # reference Ftarg: "FMSY" or "F=M"
# Fconst=1
# a=0.1        # a numeric value that will be multiplied by B0 to define threshold below which F=0
# b=1          # a numeric value that will be multiplied to Btarget to define threshold above which F=Ftarget & below which F is ramped down to zero
# 
# BuildPar=T
# OMdirs=list(OMdir)
# simYrs=100
# niters=1
# seed=430
# Fprop averaged for years 1995-2015

#-------------------------------------------------------------------------------------------------------------
# MSE FUNCTION:
#-------------------------------------------------------------------------------------------------------------

MSE_func = function(MCMCdir, OMdir, EMdir, StoreResults, FRQ=5, 
                    Btarg="BMSY", Bconst=1, Ftarg="FMSY", Fconst=1, a=0.1, b=1, 
                    BuildPar=T, OMdirs=list(OMdir), 
                    simYrs=100, niters=NA, seed=430, sourcedir, SR="LFSR", implement="default", ...) {
  # niters=seq(from=47, to=545, by=2)
  
  # WARNING REMINDER # 
  warning("Before running this function, be sure that all starting SS files are ready: \
  
          - MCMC should have ran with all non-process error varying parameters fixed \
          
          - OM files should be ready for bootstrap: \
            <.dat> contains dummy data, \ 
            <.ctl> has priors on all observation error parameters, and \
            <starter.ss> use ss.par=1 and number of datfiles to produce = 3\
            
          - EM files should be ready for assessment: only available data, and \
            fixed and time-invariant parameters as appropriate")
  
  
  #-------------------------------------------------------------------------------------------------------------
  # Read in libraries and source files
  #-------------------------------------------------------------------------------------------------------------
  
  # Use the source() file to read in functions from other r.scripts
  if(SR=="LFSR"){
    source(file.path(sourcedir,"BuildParFile.R")) # BuildParFile
  }
  if(SR=="BH"){
    source(file.path(sourcedir,"BuildParFile_BH.R")) # BuildParFile
  }
  if(SR=="lnR0"){
    source(file.path(sourcedir,"BuildParFile_lnR0.R")) # BuildParFile
  }
  source(file.path(sourcedir,"RunOM_NoHess.R")) # RunOM_NoHess
  source(file.path(sourcedir,"BuildEMDatFile.R")) # BuildEMDatFile
  source(file.path(sourcedir,"UpdateEMDatFile.R")) # UpdateEMDatFile
  source(file.path(sourcedir,"RunEM.R")) # RunEM
  source(file.path(sourcedir,"HCR.R")) # HCR
  if(implement=="default"){
    source(file.path(sourcedir,"ImplementHCR.R")) # ImplementHCR
  }
  if(implement=="MexRec"){
    source(file.path(sourcedir,"ImplementHCR_MexRec.R")) # ImplementHCR
  }
  source(file.path(sourcedir,"EditStarterFile.R")) # ImplementHCR
  # copy files to *save results* folder; maybe save relevant results in .Rdata form
  
  # required libraries
  library(r4ss)
  library(data.table)
  library(msm)
  
  
  #-------------------------------------------------------------------------------------------------------------
  # Housekeeping
  #-------------------------------------------------------------------------------------------------------------
  
  set.seed(seed)
  
  MSEResults <<- list()     # empty list to store MSE results
  # SSgetMCMC(dir="D:\\MSE_Run\\RUN_MCMC\\sandbar_330_OM_BH_MCMC", writecsv=F)
  mcmc = SSgetMCMC(dir=MCMCdir, writecsv=FALSE) # get mcmc file
  OMdat = SS_readdat(file=paste0(OMdir,"\\SB.dat"), version="3.30")
  # modEM = SS_output(EMdir)
  
  StartYear = OMdat$styr
  EndYear = OMdat$endyr
  StartSimYear = EndYear - (simYrs-1)
  
  #get niters as sequence
  if(length(niters)<=1){
    ni = ifelse(is.na(niters), 1000, niters)
    niters = 1:ni
  }
  
  for(i in niters) {
    # i=1
    
    if(BuildPar==T){
      #-------------------------------------------------------------------------------------------------------------
      # Build Par File
      #-------------------------------------------------------------------------------------------------------------
      BuildParFile(MCMCdir,mcmc,i,OMdirs=list(OMdir) )
    } # End if BuildPar==T

    
    for(tt in seq(StartSimYear,(EndYear+1),by=FRQ) ){ # maybe by FRQ!
    # for(tt in seq(StartSimYear,2041,by=FRQ)){ # maybe by FRQ!
      # timet <<- tt
      
      #-------------------------------------------------------------------------------------------------------------
      # RUN OM -nohess (BOOTSTRAP  - DATA GENERATING PROCESS)
      #-------------------------------------------------------------------------------------------------------------
      EditStarterFile(OMdir, seed, tt)
      RunOM_NoHess(OMdir)
      
      
      #-------------------------------------------------------------------------------------------------------------
      # BUILD / UPDATE EM DAT FILE
      #-------------------------------------------------------------------------------------------------------------
      if(tt==StartSimYear){             # run a separate file to include historical observation error; and update OM
        BuildOM(OMdir, tt)
        BuildEM(EMdir, OMdir, tt)
      } #end if tt==1
      
      if(tt>StartSimYear){              # this is to include future observation error
        UpdateOM(OMdir, tt, FRQ)
        UpdateEM(EMdir, OMdir, FRQ, tt)
      } #end if tt>1
      
      
      #-------------------------------------------------------------------------------------------------------------
      # RUN EM
      #-------------------------------------------------------------------------------------------------------------
      RunEM(EMdir)
      
      
      #-------------------------------------------------------------------------------------------------------------
      # FIT HCR
      #-------------------------------------------------------------------------------------------------------------
      modEM <<- SS_output(dir=EMdir)
      
      if(tt < EndYear){                                                     # Only do HCR if tt < EndYear.
        hcr = HCR(Btarg, Bconst=1, Ftarg, Fconst, a, b, modEM, tt, Fprop)
        
        #-------------------------------------------------------------------------------------------------------------
        # UPDATE OM TO APPLY HCR
        #-------------------------------------------------------------------------------------------------------------
        implementHCR(hcr, tt, FRQ, modEM, OMdir, i)
        # MSEResults <<- imphcr$MSEResults
        
      } # End HCR and implementHCR
         
        
    } # end tt loop ; time loop
    
    # save files to StoreResults
    file.copy(from=paste0(OMdir,"\\data.ss_new"),to=paste0(StoreResults,"\\OMdata_",i,".ss_new"))
    file.copy(from=paste0(OMdir,"\\Report.sso"),to=paste0(StoreResults,"\\OMReport_",i,".sso"))
    file.copy(from=paste0(EMdir,"\\data.ss_new"),to=paste0(StoreResults,"\\EMdata_",i,".ss_new"))
    file.copy(from=paste0(EMdir,"\\Report.sso"),to=paste0(StoreResults,"\\EMReport_",i,".sso"))
    OM = SS_output(OMdir)
    EM = SS_output(EMdir)
    MSEResults[[paste0("OM_",i)]] <- OM
    MSEResults[[paste0("EM_",i)]] <- EM
    assign("MSEResults",MSEResults, envir=globalenv())
    # Save MSEResults data in store results
    save(MSEResults, file=paste0(StoreResults,"\\MSEResults.RData"))
    
    #convert data files to starter version. 
    file.copy(from=file.path(OMdir,"SB - START.dat"),to=file.path(OMdir,"SB.dat"), overwrite=T)
    file.copy(from=file.path(EMdir,"SB - START.dat"),to=file.path(EMdir,"SB.dat"), overwrite=T)
    
    
  }  # end for i:nrow(mcmc); iteration loop
  
  return(print("End MSE_func"))
} # end MSE_func



# MSE_func(MCMCdir, OMdir, EMdir, FRQ=5,
#          Btarg="BMSY", Bconst=1, Ftarg="FMSY", Fconst=1, a=0.1, b=1, 
#          BuildPar=T, OMdirs=list(OMdir), 
#          simYrs=100, niters=1, Fprop)

# MSE_func(MCMCdir, OMdir, EMdir, StoreResults, FRQ=5, niters=1, sourcedir=sourcedir, SR="BH")



## only run implementHCR if tt<EndYear{}