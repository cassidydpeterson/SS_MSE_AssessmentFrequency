# SS_MSE_AssessmentFrequency

This is a repository containing wrapper R code for performing a Stock Synthesis-based MSE on the sandbar shark to explore the impact of altered assessment frequency. 

The accompanying manuscript is currently *in revision*: 

>  Peterson CD. Wilberg MJ. Cortes E. Courtney DL. Latour RJ. Effects of altered stock assessment frequency on the management of a large coastal shark.

This code is modified from the [**SS_MSE** repository](https://github.com/cassidydpeterson/SS_MSE) (Peterson et al. 2022). 

A brief description of the methods follows.

A recent SS3 assessment model is used as the base of each operating model (OM) and as the estimating model (EM) for an MSE. The OM was subjected to additional complexity. The basic premise for this wrapper function follows Maunder's (2014) application to Pacific bluefin tuna. An MCMC simulation of the SS3-based OM is run to generate future OM states of nature, while incorporating process uncertainty (including recruitment deviations, and time-invariant and time-varying parameters). We assume that the MCMC steps are completed in accordance with the SS MSE protocol (Maunder 2014) prior to calling this wrapper function. 

This wrapper function serves to modify pre-built SS3 input files (starter.ss, forecast.ss, data.dat, data.ctl) to reflect each new state of nature, as obtained from the MCMC process, and contains the observation model (data-generating process), estimating model (EM), harvest control rule (HCR), and implementation model. Each function is developed specifically for the current example (namely, BuildParFile.R, HCR.R, and ImplementHCR.R). The data-generating process uses SS3's parametric bootstrapping protocol to generate future observed data streams with associated observation uncertainty (e.g., catch stream uncertainty, CPUE observations, length/age compositions, etc.). The EM is a simplified variant of the OM within SS3, reflecting similar assumptions made in the most recent SS3 assessment. The HCR in this example is a threshold constant harvest rate control rule, and the implementation model reflects the recent state of the fishery and thereby includes implementation uncertainty based on recent observations within the fishery. 

This protocol was developed using SS3 v3.30.15. 


## Functions included:
 
### BuildParFile

`BuildParFile()` takes resampled parameter values from the completed MCMC posterior.sso file and inserts one set of parameter estimates into the ss.par file. This .par file will be read into the OM (with no parameter estimation) to generate a unique state of nature. 

Note that this function is written specifically for this sandbar shark example, and the code is somewhat clunky, as it was written before the `SS_readpar()` update to `r4ss`. There is also a second script labeled BuildParFile_BH.R, which includes the `BuildParFile()` designated for the OMs that specify a Beverton-Holt "BH" stock-recruit relationship. 


### EditStarterFile

`EditStarterFile()` sets the random number seed for the data-generating bootstrap process. 


### RunOM_NoHess 

`RunOM_NoHess()` runs the operating model while specifying the `-nohess` option. This is the data-generating bootstrap process. Note that the OM SS files should be pre-specified according to the SS MSE protocol (Maunder 2014), such that dummy data is included, starting values are read from the .par file, and no parameters are estimated. 


### BuildEMDatFile

This script contains the `BuildOM()` and `BuildEM()` functions.These functions take the information obtained from the 1st data-generating bootstrap process and inserts expected historical values in the OM and observed historical values in the EM. These functions are only to build the OM and EM data files in the first time-step. 


### UpdateEMDatFile

This script contains the `UpdateOM()` and `UpdateEM()` functions. These take the new information obtained from the data-generating bootstrap process and inserts expected future values in the OM and observed future data in the EM. These functions are called each time step beyond the first. 


### UpdateCatches_Annual

This script contains the function `UpdateCatchesAnnual()`, which is used to update catches annually in the OM between stock assessments. 


### RunEM

`RunEM()` runs the estimation model after observation uncertainty has been added via the results of the data-generating process. 


### HCR

`HCR()` function applies the harvest control rule. This function is a customizable threshold constant harvest rate control rule. 


### ImplementHCR

`ImplementHCR()` takes the results from the HCR and applies the ACL (quota) to the fishery. This includes allocating commercial catch among commercial fisheries and estimating future catches that are not specified by the commercial quota (in the sandbar shark example: Mexican + Recreational catches, and Menhaden bycatch). Note that this function must be specific to each example and is not generalizeable. Further note that a limitation of this approach is that catch of all fisheries is assumed be constant within a time block; for example, if the stock assessment frequency is 5 years, then catches will be constant (with unique implementation and observation uncertainty) for the next 5 years. 


### MSE_Master 

`MSE_func()` is the wrapper function designed to pull in all the other function scripts and loop over time-steps to complete the MSE simulation. 


### Run_MSE_inParallel

This script contains the code used to run `MSE_func()` in parallel. 


## Additional scripts to analyze results

Additional scripts that were created to analyze MSE results are included in the repository. 


### SS_ReadReportResults

This script contains functions modified from `r4ss` to extract data from saved report files.





## References

* Maunder MN (2014) Management strategy evaluation (MSE) implementation in Stock Synthesis: application to Pacific bluefin tuna. IATTC Stock Assessment Report. 15: 100-117.
* Peterson CD. Wilberg MJ. Cortes E. Courtney DL. Latour RJ. (2022) Effects of unregulated international fishing on recovery potential of the sandbar shark within the southeast United States. Canadian Journal of Fisheries and Aquatic Sciences. DOI: [10.1139/cjfas-2021-0345](10.1139/cjfas-2021-0345)



