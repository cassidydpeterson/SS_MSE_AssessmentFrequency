####################
# MSE in SS for
# Sandbar shark
# MASTER
# C.Peterson
# UPDATED: 5/2020
####################

# NOTE: before this code is implemented, the MCMC should be run and checked. The base OM & EM should be basically generated.
# Build .bat file that sets up folders and copies base files into each folder. See relevant helper .word file for diagram.

# #-------------------------------------------------------------------------------------------------------------
# # Define relevant parameters and directories
# #-------------------------------------------------------------------------------------------------------------
# #
# MCMCdir =  # OM version that undergoes MCMC directory
# OMdir =  # OM directory
# EMdir =  # EM directory
# StoreResults =  # Directory to store results
# sourcedir =
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
# SR="LFSR"
# implement="default"
# # Fprop averaged for years 1995-2015

#-------------------------------------------------------------------------------------------------------------
# MSE FUNCTION:
#-------------------------------------------------------------------------------------------------------------

MSE_func_FRQ <- function(MCMCdir, OMdir, EMdir, StoreResults, FRQ = 5,
                         Btarg = "BMSY", Bconst = 1, Ftarg = "FMSY", Fconst = 1, a = 0.1, b = 1,
                         BuildPar = T, OMdirs = list(OMdir),
                         simYrs = 100, niters = NA, seed = 430, sourcedir, SR = "LFSR", implement = "default", ...) {
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
  if (SR == "LFSR") {
    source(file.path(sourcedir, "BuildParFile.R")) # BuildParFile
  }
  if (SR == "BH") {
    source(file.path(sourcedir, "BuildParFile_BH.R")) # BuildParFile
  }
  if (SR == "lnR0") {
    source(file.path(sourcedir, "BuildParFile_lnR0.R")) # BuildParFile
  }

  source(file.path(sourcedir, "RunOM_NoHess.R")) # RunOM_NoHess
  source(file.path(sourcedir, "BuildEMDatFile.R")) # BuildEMDatFile
  source(file.path(sourcedir, "UpdateEMDatFile.R")) # UpdateEMDatFile
  source(file.path(sourcedir, "UpdateOMDatFile.R")) # UpdateOMDatFile
  source(file.path(sourcedir, "RunEM.R")) # RunEM
  source(file.path(sourcedir, "HCR.R")) # HCR
  source(file.path(sourcedir, "EditStarterFile.R")) # ImplementHCR

  if (implement == "HiMexRec") {
    source(file.path(sourcedir, "ImplementHCR.R")) # ImplementHCR
    source(file.path(sourcedir, "UpdateCatchesAnnual.R")) # Annual
  }
  if (implement == "Concept") {
    source(file.path(sourcedir, "ImplementHCR_MexRec.R")) # ImplementHCR
    source(file.path(sourcedir, "UpdateCatchesAnnual_MexRec.R")) # ImplementHCR
  }
  if (implement == "LoMexRec") {
    source(file.path(sourcedir, "ImplementHCR.R")) # ImplementHCR
    source(file.path(sourcedir, "UpdateCatchesAnnual_LoMexRec.R")) # ImplementHCR
  }
  # copy files to *save results* folder; maybe save relevant results in .Rdata form

  # convert data files to starter version.
  file.copy(from = file.path(OMdir, "SB_START.dat"), to = file.path(OMdir, "SB.dat"), overwrite = T)
  file.copy(from = file.path(EMdir, "SB_START.dat"), to = file.path(EMdir, "SB.dat"), overwrite = T)

  # required libraries
  library(r4ss)
  library(data.table)
  library(msm)


  #-------------------------------------------------------------------------------------------------------------
  # Housekeeping
  #-------------------------------------------------------------------------------------------------------------

  set.seed(seed)

  #### NOTE: Not saving MSEResults in this code variant.
  mcmc <- SSgetMCMC(dir = MCMCdir, writecsv = FALSE) # get mcmc file
  OMdat <- SS_readdat(file = paste0(OMdir, "\\SB.dat"), version = "3.30")

  StartYear <- OMdat$styr
  EndYear <- OMdat$endyr
  StartSimYear <- EndYear - (simYrs - 1)



  for (i in niters) {
    # i=1

    if (BuildPar == T) {
      #-------------------------------------------------------------------------------------------------------------
      # Build Par File
      #-------------------------------------------------------------------------------------------------------------
      BuildParFile(MCMCdir, mcmc, i, OMdirs = list(OMdir))
    } # End if BuildPar==T


    for (tt in seq(StartSimYear, (EndYear + 1), by = FRQ)) { # maybe by FRQ!

      #-------------------------------------------------------------------------------------------------------------
      # RUN OM -nohess (BOOTSTRAP  - DATA GENERATING PROCESS)
      #-------------------------------------------------------------------------------------------------------------
      EditStarterFile(OMdir, seed, tt)
      RunOM_NoHess(OMdir)


      #-------------------------------------------------------------------------------------------------------------
      # BUILD / UPDATE EM DAT FILE
      #-------------------------------------------------------------------------------------------------------------
      if (tt == StartSimYear) { # run a separate file to include historical observation error; and update OM
        BuildOM(OMdir, tt)
        BuildEM(EMdir, OMdir, tt)
      } # end if tt==1

      if (tt > StartSimYear) { # this is to include future observation error
        UpdateOM_annual(OMdir, t = tt) # NOTE: This is only to update annually.
        UpdateEM(EMdir, OMdir, FRQ, tt)
      } # end if tt>1


      #-------------------------------------------------------------------------------------------------------------
      # RUN EM
      #-------------------------------------------------------------------------------------------------------------
      RunEM(EMdir)


      #-------------------------------------------------------------------------------------------------------------
      # FIT HCR
      #-------------------------------------------------------------------------------------------------------------
      modEM <<- SS_output(dir = EMdir)

      if (tt <= EndYear) { # Only do HCR if tt < EndYear.
        # Apply HCR to EM Assessment Results
        hcr <- HCR(Btarg, Bconst = 1, Ftarg, Fconst, a, b, modEM, tt, Fprop)

        #-------------------------------------------------------------------------------------------------------------
        # Get Commercial catch & Nsamp for next FRQ yrs
        #-------------------------------------------------------------------------------------------------------------
        ComCat <- implementHCR(hcr, tt, FRQ, modEM, OMdir, i)

        # ---------------------------------------------------------------------------------------------------------------
        # UPDATE OM ANNUALLY between Assessment years
        #----------------------------------------------------------------------------------------------------------------
        for (yt in 1:FRQ) {
          UpdateCatchesAnnual(hcr, tt, yt, modEM, OMdir, i, seed = 430, ComCat = ComCat)
          if (yt < FRQ) {
            EditStarterFile(OMdir, seed + yt, tt)
            RunOM_NoHess(OMdir)
            UpdateOM_annual(OMdir, t = tt + yt) # NOTE: This is only to update annually.
          } # end yt<FRQ
        } # END ANNUAL yt OM UPDATE
      } # End HCR and implementHCR
    } # end tt by FRQ loop ; time loop

    # save files to StoreResults
    file.copy(from = paste0(OMdir, "\\Report.sso"), to = paste0(StoreResults, "\\OMReport_", i, ".sso"), overwrite = T)
    file.copy(from = paste0(EMdir, "\\Report.sso"), to = paste0(StoreResults, "\\EMReport_", i, ".sso"), overwrite = T)


    # convert data files to starter version.
    file.copy(from = file.path(OMdir, "SB_START.dat"), to = file.path(OMdir, "SB.dat"), overwrite = T)
    file.copy(from = file.path(EMdir, "SB_START.dat"), to = file.path(EMdir, "SB.dat"), overwrite = T)
  } # end for i:nrow(mcmc); iteration loop

  return(print("End MSE_func"))
} # end MSE_func
