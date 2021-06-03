library(fmsb)
library(vioplot)

iters = seq(47,245, by=2)
years = 1960:2115
col_list = c(rgb(0.75, 0.75, 0.75, 0.20),rgb(0.33, 0.80, 0.92, 0.15),rgb(0.79, 0.9, 0.44, 0.3),rgb(0.86, 0.44, 0.84, 0.2))
col_list2 = c("black","deepskyblue3","forestgreen","darkorchid")
lty_list = c(2,1,2,1)

col_lista = rep(col_list, (24/4))
col_list2a = rep(col_list2, (24/4))
lty_lista = rep(lty_list, (24/4))



######################### ALL  ################################

####### Worm Plots -----


# SSB/SSBMSY
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=200,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 3, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 & BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_Concept } 
  if(o==2) { OM_Plot=OM_BH_Concept } 
  if(o==3) { OM_Plot=OM_Hih_Concept } 
  if(o==4) { OM_Plot=OM_Loh_Concept } 
  if(o==5) { OM_Plot=OM_lnR0_Concept }
  if(o==6) { OM_Plot=OM_M_BH_Concept } 
  plot(years, OM_Plot$FRQ_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_LoMexRec } 
  if(o==2) { OM_Plot=OM_BH_LoMexRec } 
  if(o==3) { OM_Plot=OM_Hih_LoMexRec } 
  if(o==4) { OM_Plot=OM_Loh_LoMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_LoMexRec }
  if(o==6) { OM_Plot=OM_M_BH_LoMexRec } 
  plot(years, OM_Plot$FRQ_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop


## HiMexRec
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_HiMexRec } 
  if(o==2) { OM_Plot=OM_BH_HiMexRec } 
  if(o==3) { OM_Plot=OM_Hih_HiMexRec } 
  if(o==4) { OM_Plot=OM_Loh_HiMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_HiMexRec }
  if(o==6) { OM_Plot=OM_M_BH_HiMexRec } 
  plot(years, OM_Plot$FRQ_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop
legend("topright",c("FRQ1","FRQ5","FRQ10","FRQ15"), lwd=2, lty=lty_lista[1:4], col=col_list2a[1:4], bty='n')

mtext(expression("SSB/SSB"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)


#####
dev.off()




# F/FMSY
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_WORM_FFMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)

#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 3, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 & BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_Concept } 
  if(o==2) { OM_Plot=OM_BH_Concept } 
  if(o==3) { OM_Plot=OM_Hih_Concept } 
  if(o==4) { OM_Plot=OM_Loh_Concept } 
  if(o==5) { OM_Plot=OM_lnR0_Concept }
  if(o==6) { OM_Plot=OM_M_BH_Concept } 
  plot(years, OM_Plot$FRQ_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_LoMexRec } 
  if(o==2) { OM_Plot=OM_BH_LoMexRec } 
  if(o==3) { OM_Plot=OM_Hih_LoMexRec } 
  if(o==4) { OM_Plot=OM_Loh_LoMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_LoMexRec }
  if(o==6) { OM_Plot=OM_M_BH_LoMexRec } 
  plot(years, OM_Plot$FRQ_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop


## HiMexRec
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_HiMexRec } 
  if(o==2) { OM_Plot=OM_BH_HiMexRec } 
  if(o==3) { OM_Plot=OM_Hih_HiMexRec } 
  if(o==4) { OM_Plot=OM_Loh_HiMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_HiMexRec }
  if(o==6) { OM_Plot=OM_M_BH_HiMexRec } 
  plot(years, OM_Plot$FRQ_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$FM_FMMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop
legend("topright",c("FRQ1","FRQ5","FRQ10","FRQ15"), lwd=2, lty=lty_lista[1:4], col=col_list2a[1:4], bty='n')

mtext(expression("F/F"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)

#####

dev.off()




# Commercial Catch Com_catch
# F/FMSY
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_WORM_ComCatch.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 3, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 & BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_Concept } 
  if(o==2) { OM_Plot=OM_BH_Concept } 
  if(o==3) { OM_Plot=OM_Hih_Concept } 
  if(o==4) { OM_Plot=OM_Loh_Concept } 
  if(o==5) { OM_Plot=OM_lnR0_Concept }
  if(o==6) { OM_Plot=OM_M_BH_Concept } 
  plot(years, OM_Plot$FRQ_1$Com_catch[,1], type='l', col="white", ylim=c(0, 1000), ylab="")
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$Com_catch) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$Com_catch[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$Com_catch, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_LoMexRec } 
  if(o==2) { OM_Plot=OM_BH_LoMexRec } 
  if(o==3) { OM_Plot=OM_Hih_LoMexRec } 
  if(o==4) { OM_Plot=OM_Loh_LoMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_LoMexRec }
  if(o==6) { OM_Plot=OM_M_BH_LoMexRec } 
  plot(years, OM_Plot$FRQ_1$Com_catch[,1], type='l', col="white", ylim=c(0, 1000), ylab="")
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$Com_catch) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$Com_catch[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$Com_catch, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop


## HiMexRec
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_HiMexRec } 
  if(o==2) { OM_Plot=OM_BH_HiMexRec } 
  if(o==3) { OM_Plot=OM_Hih_HiMexRec } 
  if(o==4) { OM_Plot=OM_Loh_HiMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_HiMexRec }
  if(o==6) { OM_Plot=OM_M_BH_HiMexRec } 
  plot(years, OM_Plot$FRQ_1$Com_catch[,1], type='l', col="white", ylim=c(0, 1000), ylab="")
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$Com_catch) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$Com_catch[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$Com_catch, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
} # end o loop
legend("topright",c("FRQ1","FRQ5","FRQ10","FRQ15"), lwd=2, lty=lty_lista[1:4], col=col_list2a[1:4], bty='n')

mtext(expression("SSB/SSB"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)

#####
dev.off()





### VIOLIN PLOTS -----
# SSB/SSBMSY #
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_VIOPLOT_SSBMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))


OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")

# CONCEPT
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  r=1
  vioplot(Results$SSB_SSBMSY_2115[,r], Results$SSB_SSBMSY_2115[,(r+1)], Results$SSB_SSBMSY_2115[,(r+2)], Results$SSB_SSBMSY_2115[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  r=1
  vioplot(Results$SSB_SSBMSY_2115[,r], Results$SSB_SSBMSY_2115[,(r+1)], Results$SSB_SSBMSY_2115[,(r+2)], Results$SSB_SSBMSY_2115[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# HiMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  r=1
  vioplot(Results$SSB_SSBMSY_2115[,r], Results$SSB_SSBMSY_2115[,(r+1)], Results$SSB_SSBMSY_2115[,(r+2)], Results$SSB_SSBMSY_2115[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop


mtext(expression("SSB"[2115]*"/SSB"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)


#####
dev.off()





# SSB/SSBMSY 2065#
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_VIOPLOT_SSBMSY2065.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))


OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")

# CONCEPT
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  r=1
  vioplot(Results$SSB_SSBMSY_2065[,r], Results$SSB_SSBMSY_2065[,(r+1)], Results$SSB_SSBMSY_2065[,(r+2)], Results$SSB_SSBMSY_2065[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2), names=c(rep("", 4)) )
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  r=1
  vioplot(Results$SSB_SSBMSY_2065[,r], Results$SSB_SSBMSY_2065[,(r+1)], Results$SSB_SSBMSY_2065[,(r+2)], Results$SSB_SSBMSY_2065[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2), names=c(rep("", 4)) )
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# HiMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  r=1
  vioplot(Results$SSB_SSBMSY_2065[,r], Results$SSB_SSBMSY_2065[,(r+1)], Results$SSB_SSBMSY_2065[,(r+2)], Results$SSB_SSBMSY_2065[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2), names=c(rep("", 4)) )
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop


mtext(expression("SSB"[2065]*"/SSB"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)


#####
dev.off()



# F/FMSY 2115 # FM_FMMSY_2115
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_VIOPLOT_FMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))


OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")

# CONCEPT
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  r=1
  vioplot(Results$FM_FMMSY_2115[,r], Results$FM_FMMSY_2115[,(r+1)], Results$FM_FMMSY_2115[,(r+2)], Results$FM_FMMSY_2115[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  r=1
  vioplot(Results$FM_FMMSY_2115[,r], Results$FM_FMMSY_2115[,(r+1)], Results$FM_FMMSY_2115[,(r+2)], Results$FM_FMMSY_2115[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# HiMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  r=1
  vioplot(Results$FM_FMMSY_2115[,r], Results$FM_FMMSY_2115[,(r+1)], Results$FM_FMMSY_2115[,(r+2)], Results$FM_FMMSY_2115[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop


mtext(expression("F"[2115]*"/F"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)


#####
dev.off()




# F/FMSY 2065 # FM_FMMSY_2065
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_VIOPLOT_FMSY2065.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))


OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")

# CONCEPT
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  r=1
  vioplot(Results$FM_FMMSY_2065[,r], Results$FM_FMMSY_2065[,(r+1)], Results$FM_FMMSY_2065[,(r+2)], Results$FM_FMMSY_2065[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  r=1
  vioplot(Results$FM_FMMSY_2065[,r], Results$FM_FMMSY_2065[,(r+1)], Results$FM_FMMSY_2065[,(r+2)], Results$FM_FMMSY_2065[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# HiMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  r=1
  vioplot(Results$FM_FMMSY_2065[,r], Results$FM_FMMSY_2065[,(r+1)], Results$FM_FMMSY_2065[,(r+2)], Results$FM_FMMSY_2065[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop


mtext(expression("F"[2065]*"/F"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)


#####
dev.off()





# Cumulative US Commercial Catch #
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_VIOPLOT_CumComCatch.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))


OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")

# CONCEPT
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  r=1
  vioplot(Results$Com_Catch_cumulative[,r], Results$Com_Catch_cumulative[,(r+1)], Results$Com_Catch_cumulative[,(r+2)], Results$Com_Catch_cumulative[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 20000), names=c(rep("", 4)) )
  if(o==1) {  mtext("Concept", side=2, cex = 1, line=1) }
  abline(h=1)
  mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# LoMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  r=1
  vioplot(Results$Com_Catch_cumulative[,r], Results$Com_Catch_cumulative[,(r+1)], Results$Com_Catch_cumulative[,(r+2)], Results$Com_Catch_cumulative[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 20000), names=c(rep("", 4)) )
  if(o==1) {  mtext("LoMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop

# HiMexRec
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  r=1
  vioplot(Results$Com_Catch_cumulative[,r], Results$Com_Catch_cumulative[,(r+1)], Results$Com_Catch_cumulative[,(r+2)], Results$Com_Catch_cumulative[,(r+3)], 
          col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 20000), names=c(rep("", 4)) )
  if(o==1) {  mtext("HiMexRec", side=2, cex = 1, line=1) }
  abline(h=1)
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
} # end o loop


mtext("Cumulative Commercial Catch"  , side=3, outer = TRUE, cex = 1, line=1)


#####
dev.off()












############# BASE ACROSS IMPLEMENTATION MODELS --------

# SSB/SSBMSY
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\BASE_AllIs_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=150,
    height=300,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,1),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 3, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 & BH")
 OM_Plot=OM_Base_Concept 
  plot(years, OM_Plot$FRQ_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
    mtext("Concept", side=2, cex = 1, line=1) 
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  mtext(OM_labs[1], side=3, cex=1, line=0.25)
  

# LoMexRec
 OM_Plot=OM_Base_LoMexRec 
  plot(years, OM_Plot$FRQ_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
    mtext("LoMexRec", side=2, cex = 1, line=1) 
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  


## HiMexRec
OM_Plot=OM_Base_HiMexRec 
  plot(years, OM_Plot$FRQ_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
    mtext("HiMexRec", side=2, cex = 1, line=1) 
  abline(h=1)
  for(h in rev(hlist)){
    for(i in 1:length(iters)){
      if(i <= ncol(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY) ){                     # to skip over missing iterations
        lines(years, OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY[,i], col=col_list[which(flist==h)])
      } # end if
    } # end for i
  } # end for h
  abline(h=1)
  for(h in rev(hlist)){
    lines(years, apply(OM_Plot[[paste0("FRQ_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[which(flist==h)], lty=lty_lista[which(flist==h)])
  }
  # mtext(OM_labs[o], side=3, cex=1, line=0.25)
  
legend("topright",c("FRQ1","FRQ5","FRQ10","FRQ15"), lwd=2, lty=lty_lista[1:4], col=col_list2a[1:4], bty='n')

mtext(expression("SSB/SSB"["MSY"])  , side=3, outer = TRUE, cex = 1, line=1)


#####
dev.off()


