### Run OM with -nohess option ####
# NOTE: code is specific to windows computer


### function requirements ####
# library(r4ss)


RunOM_NoHess <- function(OMdir, extras = "", intern = FALSE) {
  newdir <- file.path(OMdir)
  oldwd <- getwd() # save working directory
  on.exit(setwd(oldwd))
  setwd(newdir)

  system(paste0("ss -nohess", " ", extras), intern = intern)
}



# ## Example Linux code
# RunOM_NoHess <- function(OMdir, extras = "", intern = FALSE) {
#   newdir <- file.path(OMdir)
#   oldwd <- getwd() # save working directory
#   on.exit(setwd(oldwd))
#   setwd(newdir)
# 
#   system(paste0("./ss -nohess", " ", extras), intern = intern)
# 
# }
