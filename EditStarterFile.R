# GET AND EDIT STARTER FILE to update bootstrap #

# library(r4ss) # UPDATE R4SS FOR THIS FUNCTION 
# dir = "C:\\Users\\Cassidy.Peterson\\Desktop\\sandbar_33015_OM_Base_2Surveys\\"

EditStarterFile = function(OMdir, seed, tt){
  starter = SS_readstarter(file.path(OMdir, "starter.ss") )
  starter$seed = seed * tt
  SS_writestarter(starter, dir=OMdir, overwrite=T)
}

