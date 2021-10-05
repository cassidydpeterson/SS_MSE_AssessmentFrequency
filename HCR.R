###########
# Run HCR
###########


# library(r4ss)



HCR <- function(Btarg = "BMSY", Bconst = 1, Ftarg = "FMSY", Fconst = 1, a = a, b = b, modEM = modEM, tt, ...) {
  #   This is for a threshold control rule
  #   Btarg can be BMSY, B0, or BSPR
  #   Bconst is a multiplication factor that we use to scale Btarg
  #   Ftarg can be FMSY or F=M
  #   Fconst is a multiplication factor that we use to scale Ftarg
  #   a is proportion of B0; bottom threshold after which F gets set to 0
  #   b is proportion Bt; threshold below which F starts to decline and above which F=Fconst*Ftarg

  ### ---------------------------------------------------------------------------------------------------
  # get EM results
  ### ---------------------------------------------------------------------------------------------------


  # get end year of the EM
  year <- tt - 1


  ### ---------------------------------------------------------------------------------------------------
  # Define reference management parameters
  ### ---------------------------------------------------------------------------------------------------
  # B current
  Bcurrent <- modEM$derived_quants[paste("SSB_", year, sep = ""), "Value"]
  # total biomass (not just SSB)
  Btotal <- modEM$timeseries[modEM$timeseries$Yr == year, "Bio_all"]
  # B0
  B0 <- modEM$derived_quants["SSB_unfished", "Value"]


  # define F target
  if (Ftarg == "FMSY") {
    Ft <- Fconst * modEM$derived_quants["annF_MSY", "Value"]
  }
  if (Ftarg == "F=M") {
    NatM <- mean(unlist(modEM$M_at_age[nrow(modEM$M_at_age), which(colnames(modEM$M_at_age) == "1"):ncol(modEM$M_at_age)]), na.rm = T) # Take average M of age 1+ of the FINAL YEAR of the EM assessment; implications for time-varying M
    Ft <- Fconst * NatM
  }

  # define B target
  if (Btarg == "BMSY") {
    Bt <- Bconst * modEM$derived_quants["SSB_MSY", "Value"]
  }
  if (Btarg == "B0") {
    Bt <- Bconst * B0
  }
  if (Btarg == "BSPR") {
    Bt <- Bconst * modEM$derived_quants["SSB_SPR", "Value"]
  }



  ### ---------------------------------------------------------------------------------------------------
  # define HCR params a and b
  ### ---------------------------------------------------------------------------------------------------

  if (is.numeric(b) == TRUE) {
    bb <- b * Bt
  }

  if (is.numeric(a) == TRUE) {
    aa <- B0 * a
  }
  if (a == b) {
    aa <- bb
  }


  ### ---------------------------------------------------------------------------------------------------
  # RUN HCR
  ### ---------------------------------------------------------------------------------------------------

  if (Bcurrent < aa) {
    Fset <- 0
  }
  if (Bcurrent <= bb & Bcurrent >= aa) {
    Fset <- (((Ft / (bb - aa)) * Bcurrent) - ((aa * Ft) / (bb - aa)))
  }
  if (Bcurrent > bb) {
    Fset <- Ft
  }

  B_at_age <- modEM$batage[modEM$batage$Yr == (max(modEM$batage$Yr) - 1) & modEM$batage$"Beg/Mid" == "B", ] # get B_at_age this year
  B_at_age <- B_at_age[, -c(1:12)] # remove first col lables so just B at age by sex


  Fprop <- rbind(
    c(
      0.4885988, 0.4597670, 0.3600514, 0.2761254, 0.2255882, 0.2017680, 0.1932426, 0.1911232, 0.1903312, 0.1886617, 0.1855537,
      0.1811965, 0.1760202, 0.1704546, 0.1648404, 0.1594134, 0.1543187, 0.1496327, 0.1453845, 0.1415724, 0.1381764, 0.1351665,
      0.1325079, 0.1301654, 0.1281045, 0.1262932, 0.1247019, 0.1233043, 0.1220767, 0.1209983, 0.1200508, 0.1179860
    ),
    c(
      0.4316876, 0.4939675, 0.3939772, 0.2936559, 0.2294996, 0.1966944, 0.1826377, 0.1772807, 0.1748025, 0.1725445, 0.1696375,
      0.1660356, 0.1619793, 0.1577488, 0.1535708, 0.1495992, 0.1459239, 0.1425869, 0.1395980, 0.1369467, 0.1346112, 0.1325642,
      0.1307765, 0.1292193, 0.1278654, 0.1266896, 0.1256695, 0.1247850, 0.1240184, 0.1233541, 0.1227786, 0.1217628
    )
  )

  Fset_at_age <- Fset * Fprop # take Fprop and apply to new Fset
  ACL <- sum(Fset_at_age * B_at_age) # Take new Fset_at_age and apply to predicted B_at_age next year to approximate ACL


  return(list(Fset = Fset, ACL = ACL))
} # end HCR function
