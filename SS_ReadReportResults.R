# dir <- "D:\\MSE_Run\\OM_BH\\HCR1\\StoreResults"
# repfile <- "OMReport_47.sso"
# # repfile <- file.path(dir, report)
# ncols=NULL; forecast=FALSE; warn=FALSE; covar=FALSE; readwt=FALSE;
# checkcor=TRUE; cormax=0.95; cormin=0.01; printhighcor=10; printlowcor=10;
# verbose=TRUE; printstats=TRUE;hidewarn=FALSE; NoCompOK=FALSE;
# aalmaxbinrange=4

SS_output_Report <-
  function(dir="C:/myfiles/mymodels/myrun/", dir.mcmc=NULL,
           repfile="Report.sso", #compfile="CompReport.sso",covarfile="covar.sso",
           #forefile="Forecast-report.sso", wtfile="wtatage.ss_new",
           #warnfile="warning.sso",
           ncols=NULL, forecast=FALSE, warn=FALSE, covar=FALSE, readwt=FALSE,
           checkcor=TRUE, cormax=0.95, cormin=0.01, printhighcor=10, printlowcor=10,
           verbose=TRUE, printstats=TRUE,hidewarn=FALSE, NoCompOK=FALSE,
           aalmaxbinrange=4)
  {
    flush.console()
    
    #################################################################################
    ## embedded functions: emptytest, matchfun and matchfun2
    #################################################################################
    
    emptytest <- function(x){
      # function to help test for empty columns
      sum(!is.na(x) & x=="")/length(x)
    }
    
    matchfun <- function(string, obj=rawrep[,1], substr1=TRUE)
    {
      # return a line number from the report file (or other file)
      # substr1 controls whether to compare subsets or the whole line
      match(string, if(substr1){substring(obj,1,nchar(string))}else{obj} )
    }
    
    matchfun2 <- function(string1,adjust1,string2,adjust2,cols="nonblank",
                          matchcol1=1,matchcol2=1,
                          objmatch=rawrep,objsubset=rawrep,
                          substr1=TRUE,substr2=TRUE,header=FALSE)
    {
      # return a subset of values from the report file (or other file)
      # subset is defined by character strings at the start and end, with integer
      # adjustments of the number of lines to above/below the two strings
      line1 <- match(string1,
                     if(substr1){
                       substring(objmatch[,matchcol1],1,nchar(string1))
                     }else{
                       objmatch[,matchcol1]
                     })
      line2 <- match(string2,
                     if(substr2){
                       substring(objmatch[,matchcol2],1,nchar(string2))
                     }else{
                       objmatch[,matchcol2]
                     })
      if(is.na(line1) | is.na(line2)) return("absent")
      
      if(is.numeric(cols))    out <- objsubset[(line1+adjust1):(line2+adjust2),cols]
      if(cols[1]=="all")      out <- objsubset[(line1+adjust1):(line2+adjust2),]
      if(cols[1]=="nonblank"){
        # returns only columns that contain at least one non-empty value
        out <- objsubset[(line1+adjust1):(line2+adjust2),]
        out <- out[,apply(out,2,emptytest) < 1]
      }
      if(header && nrow(out)>0){
        out[1,out[1,]==""] <- "NoName"
        names(out) <- out[1,]
        out <- out[-1,]
      }
      return(out)
    }
    
    df.rename <- function(df, oldnames, newnames){
      # function to replace names in dataframes
      # added to clean up adaptation to more consistent
      # syntax in Report.sso as of SS version 3.30.01.15.
      for(iname in 1:length(oldnames)){
        names(df)[names(df)==oldnames[iname]] <- newnames[iname]
      }
      return(df)
    }
    
    get_ncol <- function(file) {
      numcol <- list("yes")
      initial <- 100
      while (!all(numcol[length(numcol)] == "")) {
        numcol <- utils::read.table(file,
                                    col.names = 1:initial, fill = TRUE, quote = "",
                                    colClasses = "character", nrows = -1, comment.char = "")
        initial <- initial + 100
      }
      nummax <- max(which(
        apply(numcol, 2, function(x) all(x == "")) == FALSE)) + 1
      return(nummax)
    }
    
    #####
    
    # get info on output files created by Stock Synthesis
    shortrepfile <- repfile
    repfile <- file.path(dir,repfile)
    
  
    # read three rows to get start time and version number from rep file
    rephead <- readLines(con=repfile,n=15)
    
    # warn if SS version used to create rep file is too old or too new for this code
    # note: SS_versionCode is new with V3.20
    # perhaps in the future we will use it to replace SS_versionshort throughout r4ss?
    SS_versionCode <- rephead[grep("#V",rephead)]
    SS_version <- rephead[grep("Stock_Synthesis",rephead)]
    SS_version <- SS_version[substring(SS_version,1,2)!="#C"] # remove any version numbering in the comments
    if(substring(SS_version,1,2)=="#V"){
      SS_version <- substring(SS_version,3)
    }
    if(substring(SS_version,1,4)=="3.30"){
      SS_versionshort <- "3.30"
      SS_versionNumeric <- as.numeric(SS_versionshort)
    }
    

    
    findtime <- function(lines){
      # quick function to get model start time from SS output files
      time <- strsplit(lines[grep("ime",lines)],"ime: ")[[1]]
      if(length(time)<2) return() else return(time[2])
    }
    # repfiletime <- findtime(rephead)
    
    
    ##############################
    #read report file
    ##############################
    # ncols <- get_ncol(repfile)
    ncols <- 134 # to save time in my specific example. this step takes a long time ~ 10-12 seconds. 
    rawrep <- read.table(file=repfile,col.names=1:ncols,fill=TRUE,quote="",
                         colClasses="character",nrows=-1,comment.char="")

    
    # Ian T.: if the read.table command above had "blank.lines.skip=TRUE" then blank lines could play a role in parsing the report file
    
    # check empty columns
    nonblanks <- apply(rawrep,2,emptytest) < 1
    maxnonblank = max(0,(1:ncols)[nonblanks==TRUE])
    if(maxnonblank==ncols){
      stop("all columns are used and some data may been missed,\n",
           "  increase 'ncols' input above current value (ncols=",ncols,")")
    }
    flush.console()
    
    ###################
    # SKIP FORECAST 
    ####################
    # read forecast report file and get equilibrium yeild (for older versions)
    yielddat <- NA
    sprtarg <- -999
    btarg <- -999
    minbthresh <- -999

    # get equilibrium yield for newer versions of SS (3.30),
    # which have SPR/YPR profile in Report.sso
      yieldraw <- matchfun2("SPR/YPR_Profile",1,"Finish",-2)
    # note: section with "Dynamic_Bzero" is missing before Hessian is run or skipped

    if(!is.na(yieldraw[[1]][1])){
      names <- yieldraw[1,]
      names[names=="SSB/Bzero"] <- "Depletion"
      yielddat <- yieldraw[c(2:(as.numeric(length(yieldraw[,1])-1))),]
      yielddat[yielddat=="-nan(ind)"] <- NA # this value sometimes occurs in 3.30 models
      names(yielddat) <- names
      yielddat <- type.convert(yielddat, as.is = TRUE)
      yielddat <- yielddat[order(yielddat$Depletion,decreasing = FALSE),]
    }
    
    flush.console()

    
    ######################
    # temporary files 
    ######################
    # # check for use of temporary files
    logfile <- NA
    nwarn <- NA
    
    #######################
    # Selectivity
    #######################
    # selectivity read first because it was used to get fleet info
    # this can be moved to join rest of selex stuff after SSv3.11 not supported any more
    selex <- matchfun2("LEN_SELEX",6,"AGE_SELEX",-1,header=TRUE)
    # update to naming convention associated with 3.30.01.15
    selex <- df.rename(selex,
                       oldnames=c("fleet", "year", "seas", "gender", "morph", "label"),
                       newnames=c("Fleet", "Yr", "Seas", "Sex", "Morph", "Label"))
    selex <- type.convert(selex, as.is = TRUE)
    
    
    ############################
    ## DEFINITIONS section (new in SSv3.20)
    ###########################
    rawdefs <- matchfun2("DEFINITIONS",1,"LIKELIHOOD",-1)
    # new format for definitions (starting with 3.30.12)
    
    get.def <- function(string){
      # function to grab numeric value from 2nd column matching string in 1st column
      row <- grep(string, rawdefs$X1)[1]
      if(length(row) > 0){
        return(as.numeric(rawdefs[row, 2]))
      }else{
        return(NULL)
      }
    }
    # apply function above to get a bunch of things
    # in some cases, duplicate names are used for backward compatibility
    N_seasons         <- nseasons       <- get.def("N_seasons")
    N_sub_seasons                       <- get.def("N_sub_seasons")
    Season_Durations  <- seasdurations  <- as.numeric(rawdefs[grep("Season_Durations",
                                                                   rawdefs$X1),
                                                              1+1:nseasons])
    Spawn_month       <- spawnmonth     <- get.def("Spawn_month")
    Spawn_seas        <- spawnseas      <- get.def("Spawn_seas")
    Spawn_timing_in_season              <- get.def("Spawn_timing_in_season")
    N_areas           <- nareas         <- get.def("N_areas")
    Start_year        <- startyr        <- get.def("Start_year")
    End_year          <- endyr          <- get.def("End_year")
    Retro_year                          <- get.def("Retro_year")
    N_forecast_yrs                      <- get.def("N_forecast_yrs")
    N_sexes           <- nsexes         <- get.def("N_sexes")
    Max_age           <- accuage        <- get.def("Max_age")
    Empirical_wt_at_age                 <- get.def("Empirical_wt_at_age")
    N_bio_patterns                      <- get.def("N_bio_patterns")
    N_platoons                          <- get.def("N_platoons")
    # following quants added in 3.30.13
    NatMort_option                      <- get.def("NatMort")
    GrowthModel_option                  <- get.def("GrowthModel")
    Maturity_option                     <- get.def("Maturity")
    Fecundity_option                    <- get.def("Fecundity")
    # end quants added in 3.30.13
    Start_from_par                      <- get.def("Start_from_par")
    Do_all_priors                       <- get.def("Do_all_priors")
    Use_softbound                       <- get.def("Use_softbound")
    N_nudata                            <- get.def("N_nudata")
    Max_phase                           <- get.def("Max_phase")
    Current_phase                       <- get.def("Current_phase")
    Jitter                              <- get.def("Jitter")
    ALK_tolerance                       <- get.def("ALK_tolerance")
    # table starting with final occurrence of "Fleet" in column 1
    fleetdefs <- rawdefs[tail(grep("Fleet", rawdefs$X1),1):nrow(rawdefs),]
    names(fleetdefs) <- fleetdefs[1,] # set names equal to first row
    fleetdefs <- fleetdefs[-1,] # remove first row
    # remove any blank columns beyond Fleet_name
    fleetdefs <- fleetdefs[,1:grep("fleet_name", tolower(names(fleetdefs)))]
    # make values numeric (other than Fleet_name)
    fleetdefs <- type.convert(fleetdefs, as.is = TRUE)
    
    fleetdefs <- df.rename(fleetdefs,
                           oldnames=c("fleet_name"),
                           newnames=c("Fleet_name"))
    # fleet_type definitions from TPL:
    # 1=fleet with catch; 2=discard only fleet with F;
    # 3=survey(ignore catch); 4=ignore completely
    fleet_type   <- fleetdefs$fleet_type
    fleet_timing <- fleetdefs$timing
    fleet_area   <- fleetdefs$area
    catch_units  <- fleetdefs$catch_units
    ## equ_catch_se <- fleetdefs$equ_catch_se
    ## catch_se     <- fleetdefs$catch_se
    survey_units <- fleetdefs$survey_units
    survey_error <- fleetdefs$survey_error
    fleet_ID     <- fleetdefs$Fleet
    IsFishFleet  <- fleet_type <= 2 # based on definitions above
    nfishfleets  <- sum(IsFishFleet)
    FleetNames   <- fleetdefs$Fleet_name
    nfleets <- max(fleet_ID)
    
    # process some season info
    seasfracs <- round(12*cumsum(seasdurations))/12
    seasfracs <- seasfracs - seasdurations/2 # should be mid-point of each season as a fraction of the year
    
    # end new DEFINITIONS format (starting with 3.30.12)
      
    
    ###################
    # CPUE
    ###################
    # which column of INDEX_1 has number of CPUE values (used in reading INDEX_2)
    
    ncpue_column <- 11
    INDEX_1 <- matchfun2("INDEX_1",1,"INDEX_3",-4, header=TRUE)
    # remove any comments at the bottom of table
    INDEX_1 <- INDEX_1[substr(INDEX_1$Fleet, 1, 1) != "#",]
    # count of observations per index
    ncpue <- sum(as.numeric(INDEX_1$N), na.rm=TRUE)
    
    
    #################
    # Compositions
    #################
    
    lbins <- NA
    nlbins <- NA
    #### need to get length bins from somewhere
    temp <- rawrep[grep("NUMBERS_AT_LENGTH",rawrep[,1])+1,]
    lbinspop <- as.numeric(temp[temp!=""][-(1:12)])
    nlbinspop <- length(lbinspop)
    agebins <- NA
    nagebins <- NA
    Lbin_method <- 2
    sizebinlist <- NA
    
    # info on growth morphs (see also section setting mainmorphs below)
    endcode <- "SIZEFREQ_TRANSLATION" #(this section heading not present in all models)
    #if(SS_versionshort=="SS-V3.11") shift <- -1 else shift <- -2
    # shift <- -1
    if(is.na(matchfun(endcode))){
      endcode <- "MOVEMENT"
      shift <- -2
    }
    morph_indexing <- matchfun2("MORPH_INDEXING",1,endcode,shift,cols=1:9,header=TRUE)
    morph_indexing <- type.convert(morph_indexing, as.is = TRUE)
    morph_indexing <- df.rename(morph_indexing,
                                oldnames=c("Gpattern", "Bseas", "Gender"),
                                newnames=c("GP", "BirthSeas", "Sex"))
    ngpatterns <- max(morph_indexing$GP)
    
    #################
    # Forecast
    #################
    nforecastyears <- NA
   
    #######################
    # Stats List
    #######################
    # stats list: items that are output to the GUI (if printstats==T) for a quick summary of results
    stats <- list()
    stats$SS_version <- SS_version
    stats$SS_versionshort <- SS_versionshort
    stats$SS_versionNumeric <- SS_versionNumeric
    
    stats$StartTime <- paste(as.character(matchfun2("StartTime",0,"StartTime",0,cols=1:6)),collapse=" ")
    stats$RunTime <- paste(as.character(matchfun2("StartTime",2,"StartTime",2,cols=4:9)),collapse=" ")
    
    # data return object to fill in various things
    returndat <- list()
    
 
    
    # likelihoods
    rawlike <- matchfun2("LIKELIHOOD",2,"Fleet:",-2)
    # check for new section added in SS version 3.30.13.04 (2019-05-31)
    laplace_line <- which(rawlike[,1] == "#_info_for_Laplace_calculations")
    if(length(laplace_line) > 0){
      rawlike <- rawlike[-laplace_line,]
    }
    # make numeric, clean up blank values
    like <- data.frame(signif(as.numeric(rawlike[,2]),digits=7))
    names(like) <- "values"
    rownames(like) <- rawlike[,1]
    lambdas <- rawlike[,3]
    lambdas[lambdas==""] <- NA
    lambdas <- as.numeric(lambdas)
    like$lambdas <- lambdas
    # separate new section added in SS version 3.30.13.04 (2019-05-31)
    if(length(laplace_line) > 0){
      stats$likelihoods_used <- like[1:(laplace_line - 1),]
      stats$likelihoods_laplace <- like[laplace_line:nrow(like),]
    }else{
      stats$likelihoods_used <- like
      stats$likelihoods_laplace <- NULL
    }
    
    # read fleet-specific likelihoods
    likelihoods_by_fleet <-
      matchfun2("Fleet:",0,"Input_Variance_Adjustment",-1,header=TRUE)
    Parm_devs_detail <- NA
    # read detail on parameters devs (if present, 3.30 only)
    if(length(grep("Parm_devs_detail", likelihoods_by_fleet[,1]))>0){
      likelihoods_by_fleet <-
        matchfun2("Fleet:",0,"Parm_devs_detail",-1,header=TRUE)
      Parm_devs_detail <-
        matchfun2("Parm_devs_detail",1,"Input_Variance_Adjustment",-1,header=TRUE)
    }
    
    
    
    # clean up fleet-specific likelihoods
    likelihoods_by_fleet[likelihoods_by_fleet=="_"] <- NA
    likelihoods_by_fleet <- type.convert(likelihoods_by_fleet, as.is = TRUE)
    
    # replace numeric column names with fleet names
    names(likelihoods_by_fleet) <- c("Label","ALL",FleetNames)
    labs <- likelihoods_by_fleet$Label
    
    # removing ":" at the end of likelihood components
    for(irow in 1:length(labs)) labs[irow] <- substr(labs[irow],1,nchar(labs[irow])-1)
    likelihoods_by_fleet$Label <- labs
    
    stats$likelihoods_by_fleet <- likelihoods_by_fleet
    stats$Parm_devs_detail <- Parm_devs_detail
    
    ##############
    # parameters
    #############
    shift <- -1
    parameters <- matchfun2("PARAMETERS",1,"DERIVED_QUANTITIES",shift,header=TRUE)
    

    temp <- tail(parameters,2)[,1:3]
    parameters <- parameters[1:(nrow(parameters)-2),]
    
    parameters <- df.rename(parameters,
                            oldnames=c("PR_type","Prior_Like"),
                            newnames=c("Pr_type","Pr_Like"))
    parameters[parameters=="_"] <- NA
    parameters[parameters==" "] <- NA
    parameters[parameters=="1.#INF"] <- Inf # set infinite values equal to R's infinity
    
    # make values numeric
    parameters <- type.convert(parameters, as.is = TRUE)
    
    
    # fix for duplicate parameter labels in 3.30.03.03,
    # not robust to more than 2 growth patterns but probably will be fixed soon
    ParmLabels <- parameters$Label
    # ParmLabels[duplicated(ParmLabels)] <- paste0(ParmLabels[duplicated(ParmLabels)], "_2")
    # end fix
    rownames(parameters) <- ParmLabels
    
    # names of active parameters
    activepars <- parameters$Label[!is.na(parameters$Active_Cnt)]
    
    # if(!is.na(parfile)){
    #   parline <- read.table(parfile,fill=TRUE,comment.char="",nrows=1)
    # }else{
      parline <- matrix(NA,1,16)
    # }
    stats$N_estimated_parameters <- parline[1,6]
    
    # subset to active parameters only
    pars <- parameters[!is.na(parameters$Active_Cnt),]
    
    if(nrow(pars)>0){
      pars$Afterbound <- ""
      pars$checkdiff <- pars$Value - pars$Min
      pars$checkdiff2 <- pars$Max - pars$Value
      pars$checkdiff3 <- abs(pars$Value-(pars$Max-(pars$Max-pars$Min)/2))
      pars$Afterbound[pars$checkdiff < 0.001 | pars$checkdiff2 < 0.001 | pars$checkdiff2 < 0.001] <- "CHECK"
      pars$Afterbound[!pars$Afterbound %in% "CHECK"] <- "OK"
    }
    stats$table_of_phases <- table(parameters$Phase)
    # subset columns for printed table of estimated parameters
    estimated_non_dev_parameters <- pars[,names(pars) %in%
                                           c("Value", "Phase", "Min", "Max", "Init", "Prior", "Gradient", "Pr_type",
                                             "Pr_SD", "Pr_Like", "Parm_StDev", "Status", "Afterbound")]
    # exclude parameters that represent recdevs or other deviations
    devnames <- c("RecrDev", "InitAge", "ForeRecr",
                  "DEVadd", "DEVmult", "DEVrwalk", "DEV_MR_rwalk", "ARDEV")
    # look for rows in table of parameters that have label indicating deviation
    devrows <- NULL
    for(iname in 1:length(devnames)){
      devrows <- unique(c(devrows, grep(devnames[iname],
                                        rownames(estimated_non_dev_parameters))))
    }
    
    # add table to stats that get printed in console
    stats$estimated_non_dev_parameters <- estimated_non_dev_parameters
    
    # Semi-parametric (2D-AR1) selectivity parameters
    seldev_pars <- parameters[grep("ARDEV", parameters$Label, fixed=TRUE),
                              names(parameters) %in% c("Label", "Value")]

      # if semi-parametric selectivity IS NOT used
      seldev_pars <- NULL
      seldev_matrix <- NULL
  
    
    # # Dirichlet-Multinomial parameters
    # # (new option for comp likelihood that uses these parameters for automated
    # #  data weighting)
    # DM_pars <- parameters[grep("ln(EffN_mult)", parameters$Label, fixed=TRUE),
    #                       names(parameters)%in% c("Value","Phase","Min","Max")]
    # DM_pars$Theta <- exp(DM_pars$Value)
    # DM_pars$"Theta/(1+Theta)" <- DM_pars$Theta / (1 + DM_pars$Theta)
    # # if D-M parameters are present, then do some extra processing steps
    # age_data_info <- NULL
    # len_data_info <- NULL
    
    
     
    
    
    
    
    # # read weight-at-age file
    wtatage <- NULL

    # # read MCMC output
    mcmc <- NULL
    
    
    #####################
    # derived quantities
    #####################
    der <- matchfun2("DERIVED_QUANTITIES",4,"MGparm_By_Year_after_adjustments",-1,
                     header=TRUE)
    # make older SS output names match current SS output conventions
    der <- df.rename(der, oldnames="LABEL", newnames="Label")
    
    der <- der[der$Label!="Bzero_again",]
    der[der=="_"] <- NA
    der[der==""] <- NA
    
    # remove bad rows that may go away in future versions of SS 3.30
    test <- grep("Parm_dev_details", der$Label)
    # if(length(test)>0){
    #   der <- der[1:(min(test)-1),]
    # }
    # convert columns to numeric
    der <- type.convert(der, as.is = TRUE)
    
    # replace SPB with SSB as changed in SS version 3.30.10.00 (29 Nov. 2017)
    der$Label <- gsub("SPB_", "SSB_", der$Label, fixed=TRUE)
    # set rownames equal to Label column
    # (skipping any duplicates, such as ln(SPB)_YYYY for models with limited year range)
    rownames(der)[!duplicated(der$Label)] <- der$Label[!duplicated(der$Label)]
    
    managementratiolabels <- matchfun2("DERIVED_QUANTITIES",1,"DERIVED_QUANTITIES",3,cols=1:2)
    names(managementratiolabels) <- c("Ratio","Label")
    
    # new message about how forecast selectivity is modeled added in 3.30.06
    # (has impact on read of time-varying parameters below)
    forecast_selectivity <- grep("forecast_selectivity", rawrep[,1], value=TRUE)
    # if(length(forecast_selectivity)==0){
    #   forecast_selectivity <- NA
    #   offset <- -1
    # }else{
      offset <- -2
    # }
    
    ###########################
    # time-varying parameters
    ###########################
    MGparmAdj <- matchfun2("MGparm_By_Year_after_adjustments",1,
                           "selparm(Size)_By_Year_after_adjustments",
                           offset, header=TRUE)
    # make older SS output names match current SS output conventions
    MGparmAdj <- df.rename(MGparmAdj, oldnames="Year", newnames="Yr")
    # make values numeric
    # if(nrow(MGparmAdj)>0){
      MGparmAdj <- type.convert(MGparmAdj, as.is = TRUE)
    # }else{
    #   MGparmAdj <- NA
    # }
    
    # time-varying size-selectivity parameters
    SelSizeAdj <- matchfun2("selparm(Size)_By_Year_after_adjustments",2,
                            "selparm(Age)_By_Year_after_adjustments",-1)
    # if(nrow(SelSizeAdj)>2){
      SelSizeAdj <- SelSizeAdj[,apply(SelSizeAdj,2,emptytest)<1]
      SelSizeAdj[SelSizeAdj==""] <- NA
      # make values numeric
      SelSizeAdj <- type.convert(SelSizeAdj, as.is = TRUE)
      
      # provide rownames (after testing for extra column added in 3.30.06.02)
      # if(rawrep[matchfun("selparm(Size)_By_Year_after_adjustments")+1, 3] == "Change?"){
        names(SelSizeAdj) <- c("Fleet","Yr","Change?",
                               paste("Par",1:(ncol(SelSizeAdj)-3),sep=""))
      # }else{
      #   names(SelSizeAdj) <- c("Fleet","Yr",
      #                          paste("Par",1:(ncol(SelSizeAdj)-2),sep=""))
      # }
    # }else{
    #   SelSizeAdj <- NA
    # }
    
    # time-varying age-selectivity parameters
    SelAgeAdj <- matchfun2("selparm(Age)_By_Year_after_adjustments",2,"RECRUITMENT_DIST",-1)
    
      SelAgeAdj <- NA

    
    ########## recruitment distribution ################
    recruitment_dist <- matchfun2("RECRUITMENT_DIST",1,"MORPH_INDEXING",-1,header=TRUE)
    # models prior to SSv3.24Q have no additional outputs
    
      # names were changed in SSv3.30
      # if(length(grep("RECRUITMENT_DIST_Bmark",recruitment_dist[,1]))>0){
        recruitment_dist <- matchfun2("RECRUITMENT_DIST",0,"MORPH_INDEXING",-1,header=FALSE)
        # start empty list
        rd <- list()
        # find break points in table
        rd.line.top   <- 1
        rd.line.Bmark <- grep("RECRUITMENT_DIST_Bmark", recruitment_dist[,1])
        rd.line.endyr <- grep("RECRUITMENT_DIST_endyr", recruitment_dist[,1])
        rd.line.end   <- nrow(recruitment_dist)
        # split apart table
        rd$recruit_dist       <- recruitment_dist[(rd.line.top+1):(rd.line.Bmark-1),]
        rd$recruit_dist_Bmark <- recruitment_dist[(rd.line.Bmark+1):(rd.line.endyr-1),]
        rd$recruit_dist_endyr <- recruitment_dist[(rd.line.endyr+1):(rd.line.end),]
       #}
      
      for(i in 1:length(rd)){
        # convert first row to header
        tmp <- rd[[i]]
        names(tmp) <- tmp[1,]
        tmp <- type.convert(tmp[-1,], as.is = TRUE)
        rd[[i]] <- tmp
      }
      # provide as same name
      recruitment_dist <- rd
    # }
    
    ######### gradient ###############
    # if(covar & !is.na(corfile)){
    #   stats$log_det_hessian <- read.table(corfile,nrows=1)[1,10]
    # }
    stats$maximum_gradient_component <-
      as.numeric(matchfun2("Convergence_Level",0,"Convergence_Level",0,cols=2))
    
   
    
    # sigma_R

      last_row_index <- 11

    
    srhead <- matchfun2("SPAWN_RECRUIT",0,"SPAWN_RECRUIT",last_row_index,cols=1:6)
    rmse_table <- as.data.frame(srhead[-(1:(last_row_index-1)),1:5])
    rmse_table <- rmse_table[!grepl("SpawnBio", rmse_table[, 2]), ]
    rmse_table <- type.convert(rmse_table, as.is = TRUE)
    names(rmse_table) <- srhead[last_row_index-1,1:5]
    names(rmse_table)[4] <- "RMSE_over_sigmaR"
    sigma_R_in <- as.numeric(srhead[last_row_index-6,1])
    rmse_table <- rmse_table
    row.names(rmse_table) <- NULL
    
    ############# Bias adjustment ramp ################ --------------------------------------------------------------- REMOVE
    # biascol <- grep("breakpoints_for_bias", srhead)
    # breakpoints_for_bias_adjustment_ramp <- srhead[
    #   grep("breakpoints_for_bias", srhead[, biascol]), 1:5]
    # colnames(breakpoints_for_bias_adjustment_ramp) <- c("last_yr_early",
    #                                                     "first_yr_full", "last_yr_full", "first_yr_recent", "max_bias_adj")
    # rownames(breakpoints_for_bias_adjustment_ramp) <- NULL
    # -------------------------------------------------------------------------------------------------------------------------- END REMOVE
    
    ############## Spawner-recruit curve #####################
    # read SPAWN_RECRUIT table
    raw_recruit <- matchfun2("SPAWN_RECRUIT", last_row_index+1, "INDEX_2", -1)

    
    # starting in 3.30.11.00, a new section with the full spawn recr curve was added
    spawn_recruit_end <- grep("Full_Spawn_Recr_Curve", raw_recruit[,1])
    # if(length(spawn_recruit_end) > 0){
      # split the two pieces into separate tables
      Full_Spawn_Recr_Curve <- raw_recruit[(spawn_recruit_end+1):nrow(raw_recruit), 1:2]
      raw_recruit <- raw_recruit[1:(spawn_recruit_end-2),]
      # make numeric
      names(Full_Spawn_Recr_Curve) <- Full_Spawn_Recr_Curve[1, ]
      Full_Spawn_Recr_Curve <- Full_Spawn_Recr_Curve[-1, ]
      Full_Spawn_Recr_Curve[, 1:2] <- lapply(Full_Spawn_Recr_Curve[, 1:2], as.numeric)
    # }else{
    #   Full_Spawn_Recr_Curve <- NULL
    # }
    
    # process SPAWN_RECRUIT table
    names(raw_recruit) <- raw_recruit[1,]
    raw_recruit[raw_recruit=="_"] <- NA
    raw_recruit <- raw_recruit[-(1:2),] # remove header rows
    recruit <- raw_recruit[-(1:2),] # remove rows for Virg and Init
    # temporary change for model that has bad values in dev column
    recruit$dev[recruit$dev=="-nan(ind)"] <- NA
    
    # make values numeric
    recruit <- type.convert(recruit, as.is = TRUE)
    
    # make older SS output names match current SS output conventions
    recruit <- df.rename(recruit,
                         oldnames=c("year", "spawn_bio", "adjusted"),
                         newnames=c("Yr", "SpawnBio", "bias_adjusted"))
    
    ## variance and sample size tuning information
    vartune <- matchfun2("INDEX_1", 1, "INDEX_1", (nfleets+1), header=TRUE)
    # fill in column name that was missing in SS 3.24 (and perhaps other versions)
    # and replace inconsistent name in some 3.30 versions with standard name
    vartune <- df.rename(vartune,
                         oldnames=c("NoName", "fleetname"),
                         newnames=c("Name", "Name"))
    
    ############# FIT_LEN_COMPS #######################
    # if(SS_versionNumeric >= 3.3){
      # This section hasn't been read by SS_output in the past,
      # not bother adding to models prior to 3.30
      fit_len_comps <- matchfun2("FIT_LEN_COMPS",1,"Length_Comp_Fit_Summary",-1,
                                 header=TRUE)
    # }else{
    #   fit_len_comps <- NULL
    # }
    # if(!is.null(dim(fit_len_comps)) && nrow(fit_len_comps)>0){
      # replace underscores with NA
      fit_len_comps[fit_len_comps=="_"] <- NA
      # make columns numeric (except "Used", which may contain "skip")
      fit_len_comps <- type.convert(fit_len_comps, as.is = TRUE)
    # }else{
    #   fit_len_comps <- NULL
    # }
    
    # Length comp effective N tuning check

      lenntune <- matchfun2("Length_Comp_Fit_Summary",1,"FIT_AGE_COMPS",-1,header=TRUE)
      lenntune <- df.rename(lenntune,
                            oldnames=c("FleetName"),
                            newnames=c("Fleet_name"))
      
      # if("Factor" %in% names(lenntune)){
        # format starting with 3.30.12 doesn't need adjustment, just convert to numeric
        lenntune <- type.convert(lenntune, as.is = TRUE)
      
    stats$Length_Comp_Fit_Summary <- lenntune
    
    ############### FIT_AGE_COMPS ################### ----------------------------------------------------------------------- REMOVE

      # fit_age_comps <- matchfun2("FIT_AGE_COMPS",1,"Age_Comp_Fit_Summary",-1,
      #                            header=TRUE)

      fit_age_comps <- NULL

    
    # # Age comp effective N tuning check
    # 
    #   agentune <- matchfun2("Age_Comp_Fit_Summary",1,"FIT_SIZE_COMPS",-1,
    #                         header=TRUE)
    # agentune <- df.rename(agentune,
    #                       oldnames=c("FleetName"),
    #                       newnames=c("Fleet_name"))
    # 
    # 
    #   # format starting with 3.30.12 doesn't need adjustment, just convert to numeric
    #   agentune <- type.convert(agentune, as.is = TRUE)
    # 
    # stats$Age_Comp_Fit_Summary <- agentune
  
    ################## FIT_SIZE_COMPS ######################################
    fit_size_comps <- NULL

        fit_size_comps <- matchfun2("FIT_SIZE_COMPS",1,"OVERALL_COMPS",-1,
                                    header=FALSE)

          sizentune <- NULL

      stats$Size_comp_Eff_N_tuning_check <- sizentune
      # ---------------------------------------------------------------------------------------------------------------------- END REMOVE
    

    flush.console()
    
    # add stuff to list to return
    
      returndat$definitions  <- fleetdefs
      returndat$fleet_ID     <- fleet_ID
      returndat$fleet_type   <- fleet_type
      returndat$fleet_timing <- fleet_timing
      returndat$fleet_area   <- fleet_area
      returndat$catch_units  <- catch_units
      # if(exists("catch_se")){
      #   returndat$catch_se     <- catch_se
      #   returndat$equ_catch_se <- equ_catch_se
      # }else{
        returndat$catch_se     <- NA
        returndat$equ_catch_se <- NA
      # }

    
    ################################ simple function to return additional things from the DEFINITIONS ######################################
    # section that were added with SS version 3.30.12
    return.def <- function(x){
      if(exists(x)){
        get(x)
      }else{
        NULL
      }
    }
    
    returndat$mcmc         <- mcmc
    returndat$survey_units <- survey_units
    returndat$survey_error <- survey_error
    returndat$index_variance_tuning_check <- vartune
    returndat$IsFishFleet  <- IsFishFleet
    returndat$nfishfleets  <- nfishfleets
    
    returndat$nfleets     <- nfleets
    returndat$nsexes      <- nsexes
    # returndat$ngpatterns  <- ngpatterns
    returndat$lbins       <- lbins
    returndat$Lbin_method <- Lbin_method
    returndat$nlbins      <- nlbins
    returndat$lbinspop    <- lbinspop
    returndat$nlbinspop   <- nlbinspop
    returndat$sizebinlist <- sizebinlist
    # returndat$age_data_info <- age_data_info
    # returndat$len_data_info <- len_data_info
    returndat$agebins     <- agebins
    returndat$nagebins    <- nagebins
    returndat$accuage     <- accuage
    returndat$nareas      <- nareas
    returndat$startyr     <- startyr
    returndat$endyr       <- endyr
    returndat$nseasons    <- nseasons
    returndat$seasfracs   <- seasfracs
    returndat$seasdurations <- seasdurations
    returndat$N_sub_seasons <- return.def("N_sub_seasons")
    returndat$Spawn_month   <- return.def("Spawn_month")
    returndat$Spawn_seas    <- return.def("Spawn_seas")
    returndat$Spawn_timing_in_season <- return.def("Spawn_timing_in_season")
    returndat$Retro_year     <- return.def("Retro_year")
    returndat$N_forecast_yrs <- return.def("N_forecast_yrs")
    returndat$Empirical_wt_at_age <- return.def("Empirical_wt_at_age")
    returndat$N_bio_patterns <- return.def("N_bio_patterns")
    returndat$N_platoons     <- return.def("N_platoons")
    returndat$NatMort_option <- return.def("NatMort_option")
    returndat$GrowthModel_option <- return.def("GrowthModel_option")
    returndat$Maturity_option  <- return.def("Maturity_option")
    returndat$Fecundity_option <- return.def("Fecundity_option")
    returndat$Start_from_par <- return.def("Start_from_par")
    returndat$Do_all_priors  <- return.def("Do_all_priors")
    returndat$Use_softbound  <- return.def("Use_softbound")
    returndat$N_nudata       <- return.def("N_nudata")
    returndat$Max_phase      <- return.def("Max_phase")
    returndat$Current_phase  <- return.def("Current_phase")
    returndat$Jitter         <- return.def("Jitter")
    returndat$ALK_tolerance  <- return.def("ALK_tolerance")
    returndat$nforecastyears <- nforecastyears
    returndat$morph_indexing <- morph_indexing
    #  returndat$MGParm_dev_details <- MGParm_dev_details
    returndat$MGparmAdj   <- MGparmAdj
    returndat$forecast_selectivity <- forecast_selectivity
    returndat$SelSizeAdj  <- SelSizeAdj
    returndat$SelAgeAdj   <- SelAgeAdj
    returndat$recruitment_dist <- recruitment_dist
    returndat$recruit     <- recruit
    returndat$Full_Spawn_Recr_Curve <- Full_Spawn_Recr_Curve
    # returndat$breakpoints_for_bias_adjustment_ramp <- breakpoints_for_bias_adjustment_ramp
    
    # Static growth
    begin <- matchfun("N_Used_morphs",rawrep[,6])+1 # keyword "BIOLOGY" not unique enough
    rawbio <- rawrep[begin:(begin+nlbinspop),1:10]
    rawbio <- rawbio[,apply(rawbio,2,emptytest) < 1]
    names(rawbio) <- rawbio[1,]
    biology <- type.convert(rawbio[-1,], as.is = TRUE)
    
    # determine fecundity type
    FecType <- 0
    pl <- parameters$Label
    # FecGrep1 <- grep("Eggs/kg_slope_wt_Fem", pl)
    # FecGrep2 <- grep("Eggs_exp_len_Fem", pl)
    # FecGrep3 <- grep("Eggs_exp_wt_Fem", pl)
    FecGrep4 <- grep("Eggs_slope_len_Fem", pl)
    # FecGrep5 <- grep("Eggs_slope_Wt_Fem", pl)
    
    # if(length(FecGrep1) > 0){
    #   FecType <- 1
    #   FecPar1name <- grep("Eggs/kg_inter_Fem", pl, value=TRUE)[1]
    #   FecPar2name <- pl[FecGrep1[1]]
    # }
    # if(length(FecGrep2) > 0){
    #   FecType <- 2
    #   FecPar1name <- grep("Eggs_scalar_Fem", pl, value=TRUE)[1]
    #   FecPar2name <- pl[FecGrep2[1]]
    # }
    # if(length(FecGrep3) > 0){
    #   FecType <- 3
    #   FecPar1name <- grep("Eggs_scalar_Fem", pl, value=TRUE)[1]
    #   FecPar2name <- pl[FecGrep3[1]]
    # }
    if(length(FecGrep4) > 0){
      FecType <- 4
      FecPar1name <- grep("Eggs_intercept_Fem", pl, value=TRUE)[1]
      FecPar2name <- pl[FecGrep4[1]]
    }
    # if(length(FecGrep5) > 0){
    #   FecType <- 5
    #   FecPar1name <- grep("Eggs_intercept_Fem", pl, value=TRUE)[1]
    #   FecPar2name <- pl[FecGrep5[1]]
    # }
    if(is.na(lbinspop[1])){
      lbinspop <- biology$Low[biology$GP==1]
    }
    
    returndat$biology <- biology
    returndat$FecType <- FecType
    returndat$FecPar1name <- FecPar1name
    returndat$FecPar2name <- FecPar2name
    
    returndat$FecPar1 <- parameters$Value[parameters$Label==FecPar1name]
    returndat$FecPar2 <- parameters$Value[parameters$Label==FecPar2name]
    

    
    # simple test to figure out if fecundity is proportional to spawning biomass:
    returndat$SpawnOutputUnits <- ifelse(!is.na(biology$Fecundity[1]) &&
                                           all(biology$Wt_len_F==biology$Fecundity),
                                         "biomass", "numbers")
    
    # get natural mortality type and vectors of M by age
    M_type <- as.numeric(gsub(".*([0-9]+)", "\\1",
                              rawrep[matchfun("Natural_Mortality"),2]))
    # in SS 3.30 the number of rows of Natural_Mortality is the product of
    # the number of sexes, growth patterns, settlement events but settlement
    # events didn't exist in 3.24, so it's easier to just use the following
    # keyword (also version dependent)
    endcode <- "Natural_Mortality_Bmark" # next keyword in 3.30 models
    # if(is.na(matchfun(endcode))){
    #   endcode <- "Growth_Parameters" # next keyword in 3.24
    # }
    M_Parameters <- matchfun2("Natural_Mortality",1,
                              endcode,-1,
                              header=TRUE)
    returndat$M_type <- M_type
    returndat$M_Parameters <- type.convert(M_Parameters, as.is = TRUE)
    
    # get growth parameters
    Growth_Parameters <- matchfun2("Growth_Parameters", 1,
                                   "Growth_Parameters", 1 + max(morph_indexing$GP),
                                   header=TRUE)
    Growth_Parameters <- type.convert(Growth_Parameters, as.is = TRUE)
    returndat$Growth_Parameters <- Growth_Parameters
    
    # Seas_Effects <- matchfun2("Seas_Effects",1,"Biology_at_age_in_endyr",-1,header=TRUE)
    # if(Seas_Effects[[1]][1]!="absent"){
    #   Seas_Effects <- type.convert(Seas_Effects, as.is = TRUE)
    # }else{
      Seas_Effects <- NA
    # }
    returndat$Seas_Effects <- Seas_Effects
    
    # ending year growth, including pattern for the CV (added in SSv3.22b_Aug3)
    growthCVtype <- matchfun2("Biology_at_age",0,"Biology_at_age",0,header=FALSE)
    # if(nchar(growthCVtype)>31){
      returndat$growthCVtype <- substring(growthCVtype,30)
    # }else{
    #   returndat$growthCVtype <- "unknown"
    # }
    growdat <- matchfun2("Biology_at_age",1,"MEAN_BODY_WT(begin)",-1,header=TRUE)
    # make older SS output names match current SS output conventions
    growdat <- df.rename(growdat,
                         oldnames=c("Gender"),
                         newnames=c("Sex"))
    growdat <- type.convert(growdat, as.is = TRUE)
    nmorphs <- max(growdat$Morph)
    midmorphs <- c(c(0,nmorphs/nsexes)+ceiling(nmorphs/nsexes/2))
    returndat$endgrowth <- growdat
    
    # test for use of empirical weight-at-age input file (wtatage.ss)
    test <- matchfun2("MEAN_BODY_WT(begin)",0,"MEAN_BODY_WT(begin)",0,header=FALSE)
    wtatage_switch <- length(grep("wtatage.ss",test))>0
    returndat$wtatage_switch <- wtatage_switch
    
    # mean body weight
    mean_body_wt <- matchfun2("MEAN_BODY_WT(begin)",1,"MEAN_SIZE_TIMESERIES",-1,header=TRUE)
    mean_body_wt <- type.convert(mean_body_wt, as.is = TRUE)
    returndat$mean_body_wt <- mean_body_wt
    
    # Time-varying growth
    rawgrow <- matchfun2("MEAN_SIZE_TIMESERIES",1,"mean_size_Jan_1",-1,cols=1:(4+accuage+1))
    growthvaries <- FALSE
    # if(length(rawgrow)>1){
      names(rawgrow) <- rawgrow[1,]
      growdat <- type.convert(rawgrow[-1,], as.is = TRUE)
 
        growdat <- growdat[growdat$SubSeas==1 &
                             growdat$Yr >= startyr &
                             growdat$Yr < endyr,]
      
      returndat$growthseries <- growdat
      returndat$growthvaries <- growthvaries

    
    # Length selex and retention
    # if(!forecast){
      selex <- selex[selex$Yr <= endyr,]
    # }
    returndat$sizeselex <- selex
    
    # Age based selex <--------------------------------------------------------------------------------------DON'T NEED
    # # # determine which keyword follows the AGE_SELEX section
    # 
    #   # a numbers-at-age section occurs here if detailed age-structured reports are
    #   # requested in the starter file, otherwise, a similar section occurs after the
    #   # biology section
    #   ageselex <- matchfun2("AGE_SELEX",4,"NUMBERS_AT_AGE",-1,header=TRUE)
    # 
    # ageselex <- df.rename(ageselex,
    #                       oldnames=c("fleet", "year", "seas", "gender",
    #                                  "morph", "label", "factor"),
    #                       newnames=c("Fleet", "Yr",   "Seas", "Sex",
    #                                  "Morph", "Label", "Factor"))
    # # filter forecast years from selectivity if no forecast
    # # NOTE: maybe refine this in 3.30
    # 
    #   ageselex <- ageselex[ageselex$Yr <= endyr,]
    # 
    # ageselex <- type.convert(ageselex, as.is = TRUE)
    # returndat$ageselex <- ageselex
    # -------------------------------------------------------------------------------------------------------END DON'T NEED
    
    ################ exploitation ######################
    exploitation_head <- matchfun2("EXPLOITATION", 1, "EXPLOITATION", 20,
                                   header=FALSE)
    # check for new header info added in 3.30.13_beta (14 Feb. 2019)
    # if(exploitation_head[1,1] == "Info:"){
      # NOTE: add read of additional header info here
      exploitation <- matchfun2("EXPLOITATION",
                                which(exploitation_head[,1] == "Yr"),
                                "CATCH",-1,header=TRUE)
      # remove meta-data about fleets (filtered by color in 1st column):
      # "Catchunits:","FleetType:","FleetArea:","FleetID:"
      exploitation <- exploitation[-grep(":", exploitation[,1]),]
      # find line with F_method like this "Info: F_Method:=3;.Continuous_F;..."
      # F_method info contains additional information that might be useful elsewhere
      F_method_info <- exploitation_head[grep("F_Method:",
                                              exploitation_head[,2]), 2]
      F_method_info <- gsub(pattern=".", replacement=" ", x=F_method_info, fixed=TRUE)
      F_method_info <- strsplit(F_method_info, split=";", fixed=TRUE)[[1]]
      # get numeric value for F_method
      F_method <- as.numeric(strsplit(F_method_info[[1]], split="=",
                                      fixed=TRUE)[[1]][2])
   
    returndat$F_method <- F_method
    
      # more processing of exploitation
      exploitation[exploitation=="_"] <- NA
      # make text numeric
      # "init_yr" not used as of 3.30.13, but must have been in the past
      # "INIT" appears to be used in 3.30.13 and beyond
      exploitation$Yr[exploitation$Yr %in% c("INIT", "init_yr")] <- startyr-1
      # make columns numeric
      exploitation <- type.convert(exploitation, as.is = TRUE)
      returndat$exploitation <- exploitation

    
    ###########
    #catch
    ###########
    catch <- matchfun2("CATCH",1,"TIME_SERIES",-1,substr1=FALSE,header=TRUE)
    # if table is present, then do processing of it

      # update to new column names used starting with 3.30.13
      catch <- df.rename(catch,
                         oldnames=c("Name",       "Yr.frac"),
                         newnames=c("Fleet_Name", "Time"))
      # fix likelihood associated with 0 catch
      catch$Like[catch$Like=="-1.#IND"] <- NA
      # change "INIT" or "init" to year value following convention used elsewhere
      catch$Yr[tolower(catch$Yr)=="init"] <- startyr-1
      # make columns numeric
      catch <- type.convert(catch, as.is = TRUE)
   
    returndat$catch <- catch
    
    # age associated with summary biomass
    summary_age <- rawrep[matchfun("TIME_SERIES"),2]
    summary_age <- as.numeric(substring(summary_age, nchar("BioSmry_age:_") + 1))
    returndat$summary_age <- summary_age
    
    ###############
    # time series
    ###############
    timeseries <- matchfun2("TIME_SERIES",1,"SPR_series",-1,header=TRUE)
    # temporary fix for 3.30.03.06
    timeseries <- timeseries[timeseries$Seas != "recruits",]
    
    timeseries[timeseries=="_"] <- NA
    timeseries <- type.convert(timeseries, as.is = TRUE)
    
    returndat$timeseries <- timeseries
    
    ############# get spawning season ##################
    # currently (v3.20b), Spawning Biomass is only calculated in a unique spawning season within the year

    returndat$spawnseas <- spawnseas
    
    # distribution of recruitment
      # from SSv3.24Q onward, recruitment_dist is a list of tables, not a single table
      rd <- recruitment_dist$recruit_dist_endyr
  
    
    # set mainmorphs as those morphs born in the first season with recruitment
    # and the largest fraction of the platoons (should equal middle platoon when present)
    # if(SS_versionNumeric >= 3.3){
      # new "platoon" label
      temp <- morph_indexing[morph_indexing$BirthSeas==min(rd$Seas[rd$"Frac/sex">0]) &
                               morph_indexing$Platoon_Dist==max(morph_indexing$Platoon_Dist),]
      mainmorphs <- min(temp$Index[temp$Sex==1])
      # if(nsexes==2){
        mainmorphs <- c(mainmorphs, min(temp$Index[temp$Sex==2]))
      # }
    # }
   
    returndat$mainmorphs  <- mainmorphs
    
    # get birth seasons as vector of seasons with non-zero recruitment
    birthseas <- sort(unique(timeseries$Seas[timeseries$Recruit_0 > 0]))
    # temporary fix for model with missing Recruit_0 values
    # (so far this has only been seen in one 3.30 model with 2 GPs)
    # if(length(birthseas)==0){
    #   birthseas <- sort(unique(morph_indexing$BirthSeas))
    # }
    returndat$birthseas <- birthseas
    
    # stats and dimensions
    timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
    ts <- timeseries[timeseries$Yr <= endyr+1,]
    tsyears <- ts$Yr[ts$Seas==1]
    
    ########### Depletion ############
    tsspaw_bio <- ts$SpawnBio[ts$Seas==spawnseas & ts$Area==1]

    depletionseries <- tsspaw_bio/tsspaw_bio[1]
    stats$SBzero <- tsspaw_bio[1]
    stats$current_depletion <- depletionseries[length(depletionseries)]
    
    ################### total landings #################
    # (in the future, this section should be cleaned up to take advantage of
    # new columns that are in process of being added above, such as $dead_B_sum
    ls <- nrow(ts)-1
    totretainedmat <- as.matrix(ts[,substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    ts$totretained <- 0
    ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]
    
    ########## total catch #############
    totcatchmat <- as.matrix(ts[,substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
    ts$totcatch <- 0
    ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]
    
    ########### harvest rates #############
    # if(F_method==1){
    #   stringmatch <- "Hrate:_"
    # }else{
      stringmatch <- "F:_" #}
    Hrates <- as.matrix(ts[,substr(names(ts),1,nchar(stringmatch))==stringmatch])
    fmax <- max(Hrates)
    #stats$fmax <- fmax
    #stats$endyrcatch <- ts$totcatch[ls]
    #stats$endyrlandings <- ts$totretained[ls]
    
    #######  depletion ###############
    depletion_method <- as.numeric(rawrep[matchfun("Depletion_method"),2])
    depletion_basis <- rawrep[matchfun("B_ratio_denominator"),2]
    # if(depletion_basis=="no_depletion_basis"){
    #   depletion_basis <- "none"
    # }else{
      depletion_basis <- as.numeric(strsplit(depletion_basis,"%*",fixed=TRUE)[[1]][1])/100
    # }
    returndat$depletion_method <- depletion_method
    returndat$depletion_basis <- depletion_basis
    
    ####### discard fractions #########
    
    # degrees of freedom for T-distribution
    # (or indicator 0, -1, -2 for other distributions)

      DF_discard <- NA
      shift <- 1
      discard_spec <- matchfun2("DISCARD_SPECIFICATION",9,"DISCARD_OUTPUT",-2,
                                cols=1:3,header=TRUE)
      # test for Robbie Emmet's experimental new discard option
      # if(length(grep("trunc_normal", names(discard_spec)))>0){
        discard_spec <- matchfun2("DISCARD_SPECIFICATION",10,"DISCARD_OUTPUT",-2,
                                  cols=1:3,header=TRUE)
      # }
      discard_spec <- type.convert(discard_spec, as.is = TRUE)
      names(discard_spec)[1] <- "Fleet"
    # }
    # read DISCARD_OUTPUT table
    # discard <- matchfun2("DISCARD_OUTPUT",shift,"MEAN_BODY_WT_OUTPUT",-1,header=TRUE)
    # 
    # 
    # # rename columns to standard used with 3.30.13 (starting Feb 14, 2019)
    # discard <- df.rename(discard,
    #                      oldnames=c("Name",       "Yr.frac"),
    #                      newnames=c("Fleet_Name", "Time"))
    
    # process discard info if table was present
    discard_type <- NA
   
      discard <- NA
    returndat$discard <- discard
    returndat$discard_type <- discard_type
    returndat$DF_discard <- DF_discard
    returndat$discard_spec <- discard_spec
    
    ############ Average body weight observations #############
    # degrees of freedom for T-distribution
    # DF_mnwgt <- rawrep[matchfun("log(L)_based_on_T_distribution"),1]
    
      DF_mnwgt <- NA
      mnwgt <- NA

    # returndat$mnwgt <- mnwgt
    # returndat$DF_mnwgt <- DF_mnwgt
    
    #########  Yield and SPR time-series #####################
    spr <- matchfun2("SPR_series",5,"SPAWN_RECRUIT",-1,header=TRUE)
    # read Kobe plot
    # if(length(grep("Kobe_Plot",rawrep[,1]))!=0){
      shift <- -3
      # if(SS_versionNumeric < 3.23) shift <- -1
      spr <- matchfun2("SPR_series",5,"Kobe_Plot",shift,header=TRUE)
      
      # head of Kobe_Plot section differs by SS version,
      # but I haven't kept track of which is which
      Kobe_head <- matchfun2("Kobe_Plot",0,"Kobe_Plot",5,header=TRUE)
      shift <- grep("^Y", Kobe_head[,1]) # may be "Year" or "Yr"
      Kobe_warn <- NA
      Kobe_MSY_basis <- NA

      Kobe <- matchfun2("Kobe_Plot",shift,"SPAWN_RECRUIT",-1,header=TRUE)
      Kobe[Kobe=="_"] <- NA
      Kobe[Kobe=="1.#INF"] <- NA
      Kobe[Kobe=="-1.#IND"] <- NA
      names(Kobe) <- gsub("/", ".", names(Kobe), fixed=TRUE)
      Kobe[, 1:3] <- lapply(Kobe[, 1:3], as.numeric)
    

    returndat$Kobe_warn <- Kobe_warn
    returndat$Kobe_MSY_basis <- Kobe_MSY_basis
    returndat$Kobe <- Kobe
    
    # clean up SPR output
    # make older SS output names match current SS output conventions
    names(spr) <- gsub(pattern="SPB", replacement="SSB", names(spr))
    spr <- df.rename(spr,
                     oldnames=c("Year", "spawn_bio", "SPR_std", "Y/R", "F_std"),
                     newnames=c("Yr", "SpawnBio", "SPR_report", "YPR", "F_report"))
    spr[spr=="_"] <- NA
    spr[spr=="&"] <- NA
    spr[spr=="-1.#IND"] <- NA
    spr <- type.convert(spr, as.is = TRUE)
    #spr <- spr[spr$Year <= endyr,]
    spr$spr <- spr$SPR
    returndat$sprseries <- spr
    stats$last_years_SPR <- spr$spr[nrow(spr)]
    stats$SPRratioLabel <- managementratiolabels[1,2]
    stats$last_years_SPRratio <- spr$SPR_std[nrow(spr)]
    
    returndat$managementratiolabels <- managementratiolabels
    returndat$F_report_basis <- managementratiolabels$Label[2]
    # if(length(grep("%", managementratiolabels$Label[3])) > 0 ) {
      returndat$B_ratio_denominator <-
        as.numeric(strsplit(managementratiolabels$Label[3],"%")[[1]][1])/100
    # } else {
    #   returndat$B_ratio_denominator <- NA
    # }
    returndat$sprtarg <- sprtarg
    returndat$btarg <- btarg
    

    returndat$minbthresh <- minbthresh
    
   
      returndat$equil_yield <- yielddat

    flush.console()
    

      ############## CPUE/Survey series #######################
      cpue <- matchfun2("INDEX_2",1,"INDEX_2",ncpue+1,header=TRUE)
      cpue[cpue=="_"] <- NA
      # make older SS output names match current SS output conventions
      # note: "Fleet_name" (formerly "Name") introduced in 3.30.12
      #       and might change as result of discussion on inconsistent use of
      #       similar column names.
      cpue <- df.rename(cpue,
                        oldnames=c("Yr.S", "Yr.frac", "Supr_Per", "Name"),
                        newnames=c("Time", "Time",    "SuprPer",  "Fleet_name"))
      
      # make columns numeric
      cpue <- type.convert(cpue, as.is = TRUE)
    

    returndat$cpue <- cpue
    
    ################## Numbers at age ###################

      rawnatage <- matchfun2("NUMBERS_AT_AGE",1,"BIOMASS_AT_AGE",-1,
                             cols=1:(13+accuage),substr1=FALSE)

      names(rawnatage) <- rawnatage[1,]
      rawnatage <- rawnatage[-1,]
      # make older SS output names match current SS output conventions
      rawnatage <- df.rename(rawnatage,
                             oldnames=c("Gender", "SubMorph"),
                             newnames=c("Sex", "Platoon"))
      rawnatage <- type.convert(rawnatage, as.is = TRUE)
      returndat$natage <- rawnatage
 
    
    # NUMBERS_AT_AGE_Annual with and without fishery
    natage_annual_1_no_fishery <- matchfun2("NUMBERS_AT_AGE_Annual_1", 1,
                                            "Z_AT_AGE_Annual_1", -1, header=TRUE)
    natage_annual_2_with_fishery <- matchfun2("NUMBERS_AT_AGE_Annual_2", 1,
                                              "Z_AT_AGE_Annual_2", -1, header=TRUE)
    # if(natage_annual_1_no_fishery[[1]][1] != "absent"){
      natage_annual_1_no_fishery <- type.convert(natage_annual_1_no_fishery,
                                                 as.is = TRUE)
      natage_annual_2_with_fishery <- type.convert(natage_annual_2_with_fishery,
                                                   as.is = TRUE)
    # }
    returndat$natage_annual_1_no_fishery <- natage_annual_1_no_fishery
    returndat$natage_annual_2_with_fishery <- natage_annual_2_with_fishery
    
    ################ Biomass at age ##################
    # if(SS_versionNumeric >= 3.3){
      batage <- matchfun2("BIOMASS_AT_AGE", 1, "NUMBERS_AT_LENGTH", -1,
                          cols=1:(13+accuage), substr1=FALSE)

    # if(length(batage)>1){
      names(batage) <- batage[1,]
      batage <- type.convert(batage[-1,], as.is = TRUE)
      returndat$batage <- batage
    # }
    
    ############### Numbers at length ###################
    col.adjust <- 12

      rawnatlen <- matchfun2("NUMBERS_AT_LENGTH",1,"BIOMASS_AT_LENGTH",-1,
                             cols=1:(col.adjust+nlbinspop),substr1=FALSE)
    # if(length(rawnatlen)>1){
      names(rawnatlen) <- rawnatlen[1,]
      rawnatlen <- rawnatlen[-1,]
      # make older SS output names match current SS output conventions
      rawnatlen <- df.rename(rawnatlen,
                             oldnames=c("Gender", "SubMorph"),
                             newnames=c("Sex", "Platoon"))
      rawnatlen <- type.convert(rawnatlen, as.is = TRUE)
      returndat$natlen <- rawnatlen
    # }
    
    # test ending based on text because sections changed within 3.30 series
    # if(!is.na(matchfun("F_AT_AGE"))){
      end.keyword <- "F_AT_AGE"
    # }else{
    #   end.keyword <- "CATCH_AT_AGE"
    # }
    
    # Biomass at length (first appeared in version 3.24l, 12-5-2012)
    # if(length(grep("BIOMASS_AT_LENGTH",rawrep[,1]))>0){
      rawbatlen <- matchfun2("BIOMASS_AT_LENGTH",1,end.keyword,-1,
                             cols=1:(col.adjust+nlbinspop),substr1=FALSE)
      # if(length(rawbatlen)>1){
        names(rawbatlen) <- rawbatlen[1,]
        rawbatlen <- type.convert(rawbatlen[-1,], as.is = TRUE)
        returndat$batlen <- rawbatlen
      # }
    # }
    
    ########## Movement ###############
    # movement <- matchfun2("MOVEMENT",1,"EXPLOITATION",-1,cols=1:(7+accuage),substr1=FALSE)
    # names(movement) <- c(movement[1,1:6],paste("age",movement[1,-(1:6)],sep=""))
    # movement <- df.rename(movement,
    #                       oldnames=c("Gpattern"),
    #                       newnames=c("GP"))
    # 
    # movement <- movement[-1,]
    # for(i in 1:ncol(movement)){
    #   movement[,i] <- as.numeric(movement[,i])
    # }
    # returndat$movement <- movement
    
    ######## reporting rates #############
    # tagreportrates <- matchfun2("Reporting_Rates_by_Fishery",1,
    #                             "See_composition_data_output_for_tag_recapture_details",-1,
    #                             cols=1:3)
    # 
    #   returndat$tagreportrates <- NA
    # 
    # 
    # # tag release table
    # tagrelease <- matchfun2("TAG_Recapture",1,
    #                         "Tags_Alive",-1,
    #                         cols=1:10)
    # 
    #   returndat$tagrelease <- NA
    #   returndat$tagfirstperiod <- NA
    #   returndat$tagaccumperiod <- NA
    # 
    # 
    # # tags alive
    # tagsalive <- matchfun2("Tags_Alive",1,
    #                        "Total_recaptures",-1,
    #                        cols=1:ncols)
    # 
    #   returndat$tagsalive <- NA
    # 
    # 
    # # total recaptures
    # tagtotrecap <- matchfun2("Total_recaptures",1,
    #                          "Reporting_Rates_by_Fishery",-1,
    #                          cols=1:ncols)
    # 
    #   returndat$tagtotrecap <- NA
    # 
    
    # age-length matrix
    # because of rows like " Seas: 12 Sub_Seas: 2   Morph: 12", the number of columns
    # needs to be at least 6 even if there are fewer ages
    rawALK <- matchfun2("AGE_LENGTH_KEY",4,"AGE_AGE_KEY",-1,cols=1:max(6, accuage+2))
    # if(length(rawALK)>1 & rawALK[[1]][1]!="absent" &&
    #    length(grep("AGE_AGE_KEY", rawALK[,1]))==0){
      morph_col <- 5

      starts <- grep("Morph:",rawALK[,morph_col])+2
      ends <- grep("mean",rawALK[,1])-1
      N_ALKs <- length(starts)
      # 3rd dimension should be either nmorphs or nmorphs*(number of Sub_Seas)
      ALK <- array(NA, c(nlbinspop, accuage+1, N_ALKs))
      dimnames(ALK) <- list(Length=rev(lbinspop), TrueAge=0:accuage, Matrix=1:N_ALKs)
      
      for(i in 1:N_ALKs){
        # get matrix of values
        ALKtemp <- rawALK[starts[i]:ends[i], 2 + 0:accuage]
        # loop over ages to convert values to numeric
        ALKtemp <- type.convert(ALKtemp, as.is = TRUE)
        # fill in appropriate slice of array
        ALK[,,i] <- as.matrix(ALKtemp)
        # get info on each matrix (such as "Seas: 1 Sub_Seas: 1 Morph: 1")
        Matrix.Info <- rawALK[starts[i]-2,]
        # filter out empty elements
        Matrix.Info <- Matrix.Info[Matrix.Info!=""]
        # combine elements to form a label in the dimnames
        dimnames(ALK)$Matrix[i] <- paste(Matrix.Info, collapse=" ")
      }
      returndat$ALK <- ALK
    # }
    
    # ageing error matrices -------------------------------------------------------------------------------------------- REMOVE
    # rawAAK <- matchfun2("AGE_AGE_KEY",1,"SELEX_database",-1,cols=1:(accuage+2))
    # # if(length(rawAAK)>1){
    #   starts <- grep("KEY:",rawAAK[,1])
    #   returndat$N_ageerror_defs <- N_ageerror_defs <- length(starts) ------------------------------------------------- END REMOVE
    #   
    
    # F at age (first appeared in version 3.30.13, 8-Mar-2019)
    # if(!is.na(matchfun("F_AT_AGE"))){
      fatage <- matchfun2("F_AT_AGE", 1, "CATCH_AT_AGE", -1, header=TRUE)
      fatage <- type.convert(fatage, as.is = TRUE)
    # }else{
    #   fatage <- NA
    # }
    
    # test for discard at age section (added with 3.30.12, 29-Aug-2018)
    # if(!is.na(matchfun("DISCARD_AT_AGE"))){
      # read discard at age
      # discard_at_age <- matchfun2("DISCARD_AT_AGE", 1, "BIOLOGY", -1)
      # read catch at age
      catage <- matchfun2("CATCH_AT_AGE", 1, "DISCARD_AT_AGE", -1)

        # discard_at_age <- discard_at_age[,apply(discard_at_age,2,emptytest)<1]
        # names(discard_at_age) <- discard_at_age[1,]
        # discard_at_age <- type.convert(discard_at_age[-1,], as.is = TRUE)

      catage <- catage[,apply(catage,2,emptytest)<1]
      names(catage) <- catage[1,]
      catage <- type.convert(catage[-1,], as.is = TRUE)

    returndat$fatage <- fatage
    returndat$catage <- catage
    # returndat$discard_at_age <- discard_at_age
    
    # if(!is.na(matchfun("Z_AT_AGE"))){
      # Z at age
      #With_fishery
      #No_fishery_for_Z=M_and_dynamic_Bzero
      Z_at_age <- matchfun2("Z_AT_AGE_Annual_2",1,"Spawning_Biomass_Report_1",-2,header=TRUE)
      M_at_age <- matchfun2("Z_AT_AGE_Annual_1",1,"-ln(Nt+1",-1,matchcol2=5, header=TRUE)
      # if(nrow(Z_at_age)>0){
        Z_at_age[Z_at_age=="_"] <- NA
        M_at_age[M_at_age=="_"] <- NA
        # if birth season is not season 1, you can get infinite values
        Z_at_age[Z_at_age=="-1.#INF"] <- NA
        M_at_age[M_at_age=="-1.#INF"] <- NA
        # if(Z_at_age[[1]][1]!="absent" && nrow(Z_at_age>0)){
          Z_at_age <- type.convert(Z_at_age, as.is = TRUE)
          M_at_age <- type.convert(M_at_age, as.is = TRUE)

    returndat$Z_at_age <- Z_at_age
    returndat$M_at_age <- M_at_age
    
    # Dynamic_Bzero output "with fishery"
    Dynamic_Bzero1 <- matchfun2("Spawning_Biomass_Report_2",1,"NUMBERS_AT_AGE_Annual_2",-1)
    # Dynamic_Bzero output "no fishery"
    Dynamic_Bzero2 <- matchfun2("Spawning_Biomass_Report_1",1,"NUMBERS_AT_AGE_Annual_1",-1)

      Dynamic_Bzero <- cbind(Dynamic_Bzero1,Dynamic_Bzero2[,3])
      names(Dynamic_Bzero) <- c("Yr","Era","SSB","SSB_nofishing")
      # if(nareas==1 & ngpatterns==1){ # for simpler models, do some cleanup
        Dynamic_Bzero <- type.convert(Dynamic_Bzero[-(1:2),], as.is = TRUE)
        names(Dynamic_Bzero) <- c("Yr","Era","SSB","SSB_nofishing")

    returndat$Dynamic_Bzero <- Dynamic_Bzero
    
   
    returndat$comp_data_exists <- FALSE
    
    # tables on fit to comps and mean age stuff from within Report.sso
    returndat$len_comp_fit_table <- fit_len_comps
    returndat$age_comp_fit_table <- fit_age_comps
    returndat$size_comp_fit_table <- fit_size_comps
    
    returndat$derived_quants <- der
    returndat$parameters <- parameters
    returndat$FleetNames <- FleetNames
    # returndat$repfiletime <- repfiletime
    returndat$SRRtype <- as.numeric(rawrep[matchfun("SPAWN_RECRUIT"),3]) # type of stock recruit relationship
    
    # get "sigma" used by Pacific Council in P-star calculations
    SSB_final_Label <- paste0("SSB_",endyr+1)
    # if(SSB_final_Label %in% der$Label){
      SSB_final_EST <- der$Value[der$Label==SSB_final_Label]
      SSB_final_SD <- der$StdDev[der$Label==SSB_final_Label]
      returndat$Pstar_sigma <- sqrt(log((SSB_final_SD/SSB_final_EST)^2+1))
    # }else{
    #   returndat$Pstar_sigma <- NULL
    # }
    # get alternative "sigma" based on OFL catch used by Pacific Council
    # (added 23 Sept 2019 based on decision by PFMC SSC)
    OFL_final_Label <- paste0("OFLCatch_",endyr+1)
    # if(OFL_final_Label %in% der$Label){
      OFL_final_EST <- der$Value[der$Label==OFL_final_Label]
      OFL_final_SD <- der$StdDev[der$Label==OFL_final_Label]
      returndat$OFL_sigma <- sqrt(log((OFL_final_SD/OFL_final_EST)^2+1))
    # }else{
    #   returndat$OFL_sigma <- NULL
    # }
    

    
    # extract parameter lines representing annual recruit devs
    recdevEarly   <- parameters[substring(parameters$Label,1,13)=="Early_RecrDev",]
    early_initage <- parameters[substring(parameters$Label,1,13)=="Early_InitAge",]
    main_initage  <- parameters[substring(parameters$Label,1,12)=="Main_InitAge",]
    recdev        <- parameters[substring(parameters$Label,1,12)=="Main_RecrDev",]
    recdevFore    <- parameters[substring(parameters$Label,1, 8)=="ForeRecr",]
    recdevLate    <- parameters[substring(parameters$Label,1,12)=="Late_RecrDev",]
    
    # empty variable to fill in sections
    recruitpars <- NULL
    
    # assign "type" label to each one and identify year

    if(nrow(recdevEarly)>0){
      recdevEarly$type   <- "Early_RecrDev"
      recdevEarly$Yr   <- as.numeric(substring(recdevEarly$Label,15))
      recruitpars <- rbind(recruitpars, recdevEarly)
    }
    # if(nrow(main_initage)>0){
    #   main_initage$type  <- "Main_InitAge"
    #   main_initage$Yr  <- startyr - as.numeric(substring(main_initage$Label,14))
    #   recruitpars <- rbind(recruitpars, main_initage)
    # }
    if(nrow(recdev)>0){
      recdev$type        <- "Main_RecrDev"
      recdev$Yr        <- as.numeric(substring(recdev$Label,14))
      recruitpars <- rbind(recruitpars, recdev)
    }
    if(nrow(recdevFore)>0){
      recdevFore$type    <- "ForeRecr"
      recdevFore$Yr <- as.numeric(substring(recdevFore$Label,10))
      recruitpars <- rbind(recruitpars, recdevFore)
    }
    if(nrow(recdevLate)>0){
      recdevLate$type    <- "Late_RecrDev"
      recdevLate$Yr <- as.numeric(substring(recdevLate$Label,14))
      recruitpars <- rbind(recruitpars, recdevLate)
    }
    
    # sort by year and remove any retain only essential columns
    # if(!is.null(recruitpars)){
      recruitpars <- recruitpars[order(recruitpars$Yr),
                                 c("Value","Parm_StDev","type","Yr")]
    # }
    
    # add recruitpars to list of stuff that gets returned
    returndat$recruitpars <- recruitpars
    
    # if(is.null(recruitpars)){
    #   sigma_R_info <- NULL
    # }else{
      # calculating values related to tuning SigmaR
      sigma_R_info <- data.frame(period = c("Main","Early+Main","Early+Main+Late"),
                                 N_devs = 0,
                                 SD_of_devs = NA,
                                 Var_of_devs = NA,
                                 mean_SE = NA,
                                 mean_SEsquared = NA)
      
      # calculate recdev stats  for Main period
      subset <- recruitpars$type %in% c("Main_InitAge", "Main_RecrDev")
      within_period <- sigma_R_info$period=="Main"
      sigma_R_info$N_devs[within_period] <- sum(subset)
      sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
      sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
      sigma_R_info$mean_SEsquared[within_period] <-
        mean((recruitpars$Parm_StDev[subset])^2)
      
      # calculate recdev stats  for Early+Main periods
      subset <- recruitpars$type %in% c("Early_RecrDev", "Early_InitAge",
                                        "Main_InitAge", "Main_RecrDev")
      within_period <- sigma_R_info$period=="Early+Main"
      sigma_R_info$N_devs[within_period] <- sum(subset)
      sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
      sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
      sigma_R_info$mean_SEsquared[within_period] <-
        mean((recruitpars$Parm_StDev[subset])^2)
      
      # calculate recdev stats for Early+Main+Late periods
      subset <- recruitpars$type %in% c("Early_RecrDev", "Early_InitAge",
                                        "Main_InitAge", "Main_RecrDev", "Late_RecrDev")
      within_period <- sigma_R_info$period=="Early+Main+Late"
      sigma_R_info$N_devs[within_period] <- sum(subset)
      sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
      sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
      sigma_R_info$mean_SEsquared[within_period] <-
        mean((recruitpars$Parm_StDev[subset])^2)
      
      # add variance as square of SD
      sigma_R_info$Var_of_devs <- sigma_R_info$SD_of_devs^2
      
      # add sqrt of sum
      sigma_R_info$sqrt_sum_of_components <- sqrt(sigma_R_info$Var_of_devs +
                                                    sigma_R_info$mean_SEsquared)
      # ratio of sqrt of sum to sigmaR
      sigma_R_info$SD_of_devs_over_sigma_R <- sigma_R_info$SD_of_devs/sigma_R_in
      sigma_R_info$sqrt_sum_over_sigma_R <- sigma_R_info$sqrt_sum_of_components/sigma_R_in
      sigma_R_info$alternative_sigma_R <- sigma_R_in * sigma_R_info$sqrt_sum_over_sigma_R
     #}
    stats$sigma_R_in   <- sigma_R_in
    stats$sigma_R_info <- sigma_R_info
    stats$rmse_table   <- rmse_table
    
    # process adjustments to recruit devs
    RecrDistpars <- parameters[substring(parameters$Label,1,8)=="RecrDist",]
    returndat$RecrDistpars <- RecrDistpars
    
    # adding read of wtatage file
    returndat$wtatage <- wtatage
    
    # adding new jitter info table
    # returndat$jitter_info <- jitter_info
    
    # add list of stats to list that gets returned
    returndat <- c(returndat, stats)
    
    # add info on semi-parametric selectivity deviations
    returndat$seldev_pars <- seldev_pars
    returndat$seldev_matrix <- seldev_matrix
    
   
    # add log file to list that gets returned
    returndat$logfile <- logfile
    
    
    # return the inputs to this function so they can be used by SS_plots
    # or other functions
    inputs <- list()
    inputs$dir      <- dir
    inputs$repfile  <- repfile
    inputs$forecast <- forecast
    inputs$warn     <- warn
    inputs$covar    <- covar
    inputs$verbose  <- verbose
    
    returndat$inputs <- inputs
    
    # if(verbose) cat("completed SS_output\n")
    invisible(returndat)
    
  } # end function

# st1 = Sys.time()
xx = SS_output_Report(dir="D:\\MSE_Run\\OM_BH\\HCR1\\StoreResults", repfile="OMReport_46.sso")
yy = SS_output_Report(dir="D:\\MSE_Run\\OM_BH\\HCR1\\StoreResults", repfile="OMReport_47.sso")
# xx2 = SS_output_Report(dir="D:\\MSE_Run\\OM_BH\\HCR2\\StoreResults", repfile="OMReport_46.sso")
# xx3 = SS_output_Report(dir="D:\\MSE_Run\\OM_BH\\HCR3\\StoreResults", repfile="OMReport_46.sso")
# xx4 = SS_output_Report(dir="D:\\MSE_Run\\OM_BH\\HCR4\\StoreResults", repfile="OMReport_46.sso")
# st2 = Sys.time()
# 
# st2-st1




######################################
# NOTES 
######################################

plot(xx$timeseries$SpawnBio~xx$timeseries$Yr, type='l')
lines(yy$timeseries$SpawnBio~yy$timeseries$Yr, type='l', col='blue')
lines(xx2$timeseries$SpawnBio~yy$timeseries$Yr, type='l', col='black', lty=2)
abline(h=mean(c(xx$derived_quants["SSB_MSY","Value"], yy$derived_quants["SSB_MSY","Value"])))

plot(xx$timeseries$`obs_cat:_1`+xx$timeseries$`obs_cat:_2` ~xx$timeseries$Yr, type='l', 
     ylim=c(0, mean(c(xx$derived_quants["SSB_MSY","Value"], yy$derived_quants["SSB_MSY","Value"]))) )
lines(yy$timeseries$`obs_cat:_1`+yy$timeseries$`obs_cat:_2` ~yy$timeseries$Yr, type='l', col='blue')
lines(xx2$timeseries$`obs_cat:_1`+xx2$timeseries$`obs_cat:_2` ~xx2$timeseries$Yr, type='l', lty=2)
lines(xx$timeseries$`obs_cat:_1`+xx$timeseries$`obs_cat:_2`+xx$timeseries$`obs_cat:_3`+xx$timeseries$`obs_cat:_4` ~xx$timeseries$Yr, type='l')
lines(yy$timeseries$`obs_cat:_1`+yy$timeseries$`obs_cat:_2`+yy$timeseries$`obs_cat:_3`+yy$timeseries$`obs_cat:_4` ~yy$timeseries$Yr, type='l', col='blue')
lines(xx2$timeseries$`obs_cat:_1`+xx2$timeseries$`obs_cat:_2`+xx2$timeseries$`obs_cat:_3`+xx2$timeseries$`obs_cat:_4` ~xx2$timeseries$Yr, type='l', lty=2)


abline(h=mean(c(xx$derived_quants["SSB_MSY","Value"], yy$derived_quants["SSB_MSY","Value"])))

#### CALC MEAN & MEDIAN AGE ###
xx$natage[xx$natage$`Beg/Mid`=='M' & xx$natage$Era=="TIME",]
nn = xx$natage[xx$natage$`Beg/Mid`=='M' & xx$natage$Era=="TIME", -c(1:12)]

MeanAge = vector()
MedAge = vector()
for(y in 1:nrow(nn)){
  n1 = vector()
  for(i in 1:ncol(nn)){
    n1 = c(n1, rep(i-1, nn[y,i]) )
  }
  MeanAge = c( MeanAge, mean(n1) )
  MedAge = c( MedAge, median(n1) )
}





# 
# library(r4ss)
# EMdat = SS_readdat("D:\\MSE_Run\\OM_BH\\HCR1\\StoreResults\\EMdata_46.ss_new")
# 
# 
# ll = subset(EMdat$lencomp, EMdat$lencomp$Yr==2000)
# ll2 = ll[,-c(1:6)]
# ll3 = ll2[which(ll2!=0)]
# apply(EMdat$lencomp, 2, median)
