## RUN MSE IN PARALLEL

MCMCdir <- "MCMC" # OM version that undergoes MCMC directory


#### Define directories ####
Basedir <- "OM_BH\\"
for (i in c(1, 5, 10, 15)) {
  assign(paste0("OMdirFRQ", i), paste0(Basedir, "FRQ", i, "\\OM"))
  assign(paste0("EMdirFRQ", i), paste0(Basedir, "FRQ", i, "\\EM"))
  assign(paste0("SR_FRQ", i), paste0(Basedir, "FRQ", i, "\\StoreResults"))
}

#### Define HCR trials ####
# HCR iterations:
# list.hcr=c(HCR1, HCR2, HCR3, HCR4, HCR5, HCR6, HCR7, HCR8, HCR9, HCR10, HCR11, HCR12,
#            HCR13, HCR14, HCR15, HCR16, HCR17, HCR18, HCR19, HCR20, HCR21, HCR22, HCR23, HCR24)
list.frq <- list()
list.frq[["FRQ1"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0, b = 1, OMdir = OMdirFRQ1, EMdir = EMdirFRQ1, StoreResults = SR_FRQ1, FRQ = 1)
list.frq[["FRQ5"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0, b = 1, OMdir = OMdirFRQ1, EMdir = EMdirFRQ5, StoreResults = SR_FRQ5, FRQ = 5)
list.frq[["FRQ10"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0, b = 1, OMdir = OMdirFRQ1, EMdir = EMdirFRQ10, StoreResults = SR_FRQ10, FRQ = 10)
list.frq[["FRQ15"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0, b = 1, OMdir = OMdirFRQ1, EMdir = EMdirFRQ15, StoreResults = SR_FRQ15, FRQ = 15)



######

# reminder of inputs to MSE_func
# MSE_func(MCMCdir, OMdir, EMdir, StoreResults, FRQ=5,
#          Btarg="BMSY", Bconst=1, Ftarg="FMSY", Fconst=1, a=0.1, b=1,
#          BuildPar=T, OMdirs=list(OMdir),
#          simYrs=100, niters=NA, ...)
library(foreach)
library(doSNOW)
library(parallel)

NoC <- detectCores() # determine the number of clusters you want to use.
c1 <- makeCluster(4)
registerDoSNOW(c1)

source("R:\\Management Strategy Evaluation\\SB\\MSE_RUN\\MSE_Master.R")
foreach(h = names(list.frq)) %dopar% {
  # GET MSE_func. #--> you need to put functions and packages within loop.
  MSE_func(MCMCdir,
    OMdir = list.frq[[h]]$OMdir, EMdir = list.frq[[h]]$EMdir,
    StoreResults = list.frq[[h]]$StoreResults,
    FRQ = list.frq[[h]]$FRQ,
    Btarg = "BMSY", Bconst = 1, Ftarg = list.frq[[h]]$Ftarg,
    Fconst = list.frq[[h]]$Fconst, a = list.frq[[h]]$a, b = list.frq[[h]]$b,
    sourcedir = sourcedir, SR = "BH"
  )
}


stopCluster(c1)
