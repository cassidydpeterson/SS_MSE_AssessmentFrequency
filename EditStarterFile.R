# GET AND EDIT STARTER FILE to update bootstrap #

# library(r4ss)

EditStarterFile <- function(OMdir, seed, tt) {
  starter <- SS_readstarter(file.path(OMdir, "starter.ss"))
  starter$seed <- seed * tt
  SS_writestarter(starter, dir = OMdir, overwrite = T)
}
