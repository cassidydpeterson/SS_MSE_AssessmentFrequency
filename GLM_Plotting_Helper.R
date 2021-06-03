
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_TotComCatch.png",
    type="cairo",
    units="mm",
    width=200,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(3,2), mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1,1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"Base")){
    coll='black'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
}
mtext("Base",line=-1.2, side=3)

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"BH")){
    if(stringr::str_detect(i,"M_BH")){
      coll="white"
    } else {
      coll='deepskyblue'
      if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
      if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
      if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
      lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
    }
    
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}
mtext("BH",line=-1.2, side=3)

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"Hih")){
    coll='olivedrab3'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}
mtext("Hih",line=-1.2, side=3)


boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"lnR0")){
    coll='darkorchid'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}
mtext("lnR0",line=-1.2, side=3)

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"Loh")){
    coll='orange'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}
mtext("Loh",line=-1.2, side=3)


boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"M_BH")){
    coll='salmon'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}
mtext("M_BH",line=-1.2, side=3)

mtext("Total US Commercial Catch",line=0, outer=T, side=2)
mtext("Assessment Frequency",line=0, outer=T, side=1)
######
dev.off()




png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_ProbRecov.png",
    type="cairo",
    units="mm",
    width=200,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(3,2), mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1.2,1.2,0.1,0.1))
boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,1.1) )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"Base")){
    coll='black'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("Base",line=-1.2, side=3)

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,1.1) )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"BH")){
    if(stringr::str_detect(i,"M_BH")){
      coll="white"
    } else {
      coll='deepskyblue'
      if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
      if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
      if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
      lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
    }
  }
}
mtext("BH",line=-1.2, side=3)

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,1.1) )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"Hih")){
    coll='olivedrab3'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("Hih",line=-1.2, side=3)


boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,1.1) )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"lnR0")){
    coll='darkorchid'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("lnR0",line=-1.2, side=3)

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,1.1) )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"Loh")){
    coll='orange'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("Loh",line=-1.2, side=3)


boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,1.1) )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"M_BH")){
    coll='salmon'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("M_BH",line=-1.2, side=3)

mtext("Probability of Recovery by 2115",line=0, outer=T, side=2)
mtext("Assessment Frequency",line=0, outer=T, side=1)
######
dev.off()




png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_SSB.png",
    type="cairo",
    units="mm",
    width=200,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(3,2), mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1.2,1.2,0.1,0.1))
boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"Base")){
    coll='black'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("Base",line=-1.2, side=3)

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"BH")){
    if(stringr::str_detect(i,"M_BH")){
      coll="white"
    } else {
      coll='deepskyblue'
      if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
      if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
      if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
      lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
    }
  }
}
mtext("BH",line=-1.2, side=3)

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"Hih")){
    coll='olivedrab3'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("Hih",line=-1.2, side=3)


boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"lnR0")){
    coll='darkorchid'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("lnR0",line=-1.2, side=3)

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"Loh")){
    coll='orange'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("Loh",line=-1.2, side=3)


boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"M_BH")){
    coll='salmon'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
mtext("M_BH",line=-1.2, side=3)

mtext(expression("SSB"["2115"]*" / SSB"["MSY"]),line=0, outer=T, side=2)
mtext("Assessment Frequency",line=0, outer=T, side=1)
######
dev.off()


























#### ----------------------------------------
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_Base.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"Base")){
    coll='black'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"Base")){
    coll='black'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"Base")){
    coll='black'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
# legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("black","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)


boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="black", lwd=2, cex=1.5, lty=1, pch=16)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='black', lwd=2, cex=1.5, lty=2, pch=17)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='black', lwd=2, cex=1.5, lty=3, pch=15)
# legend("top", c("Concept","LoMexRec","HiMexRec"), col=c('black','black','black'), bty='n', ncol=2, lwd=2)
legend("top", c("Base","Concept","LoMexRec","HiMexRec"), col=c("black","grey45",'grey45','grey45'), lwd=2, lty=c(1, 1, 2, 3), bty='n', ncol=2)

########
dev.off()


png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_BH.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"BH")){
    if(stringr::str_detect(i,"M_BH")){
      coll="white"
    } else {
      coll='deepskyblue'
      if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
      if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
      if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
      lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
    }
    
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"BH")){
    if(stringr::str_detect(i,"M_BH")){
      coll="white"
    } else {
      coll='deepskyblue'
      if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
      if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
      if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
      lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
    }
  }
}

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"BH")){
    if(stringr::str_detect(i,"M_BH")){
      coll="white"
    } else {
      coll='deepskyblue'
      if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
      if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
      if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
      lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
    }
  }
}
# legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("black","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)


boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="deepskyblue", lwd=2, cex=1.5, lty=1, pch=16)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='deepskyblue', lwd=2, cex=1.5, lty=2, pch=17)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='deepskyblue', lwd=2, cex=1.5, lty=3, pch=15)
# legend("top", c("Concept","LoMexRec","HiMexRec"), col=c('black','black','black'), bty='n', ncol=2, lwd=2)
legend("top", c("BH","Concept","LoMexRec","HiMexRec"), col=c("deepskyblue","grey45",'grey45','grey45'), lwd=2, lty=c(1, 1, 2, 3), bty='n', ncol=2)

########
dev.off()


png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_Hih.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"Hih")){
    coll='olivedrab3'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"Hih")){
    coll='olivedrab3'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"Hih")){
    coll='olivedrab3'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
# legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("black","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)


boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="olivedrab3", lwd=2, cex=1.5, lty=1, pch=16)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='olivedrab3', lwd=2, cex=1.5, lty=2, pch=17)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='olivedrab3', lwd=2, cex=1.5, lty=3, pch=15)
# legend("top", c("Concept","LoMexRec","HiMexRec"), col=c('black','black','black'), bty='n', ncol=2, lwd=2)
legend("top", c("Hih","Concept","LoMexRec","HiMexRec"), col=c("olivedrab3","grey45",'grey45','grey45'), lwd=2, lty=c(1, 1, 2, 3), bty='n', ncol=2)

########
dev.off()



png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_lnR0.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"lnR0")){
    coll='darkorchid'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"lnR0")){
    coll='darkorchid'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"lnR0")){
    coll='darkorchid'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
# legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("black","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)


boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="darkorchid", lwd=2, cex=1.5, lty=1, pch=16)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='darkorchid', lwd=2, cex=1.5, lty=2, pch=17)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='darkorchid', lwd=2, cex=1.5, lty=3, pch=15)
# legend("top", c("Concept","LoMexRec","HiMexRec"), col=c('black','black','black'), bty='n', ncol=2, lwd=2)
legend("top", c("lnR0","Concept","LoMexRec","HiMexRec"), col=c("darkorchid","grey45",'grey45','grey45'), lwd=2, lty=c(1, 1, 2, 3), bty='n', ncol=2)

########
dev.off()







png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_Loh.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"Loh")){
    coll='orange'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"Loh")){
    coll='orange'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"Loh")){
    coll='orange'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
# legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("black","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)


boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="orange", lwd=2, cex=1.5, lty=1, pch=16)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='orange', lwd=2, cex=1.5, lty=2, pch=17)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='orange', lwd=2, cex=1.5, lty=3, pch=15)
# legend("top", c("Concept","LoMexRec","HiMexRec"), col=c('black','black','black'), bty='n', ncol=2, lwd=2)
legend("top", c("Loh","Concept","LoMexRec","HiMexRec"), col=c("orange","grey45",'grey45','grey45'), lwd=2, lty=c(1, 1, 2, 3), bty='n', ncol=2)

########
dev.off()





png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\FRQ_Results_M_BH.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"M_BH")){
    coll='salmon'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
  # if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  # if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  # if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  # if(stringr::str_detect(i,"Loh")){coll='orange'}
  # if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  
}

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"M_BH")){
    coll='salmon'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"M_BH")){
    coll='salmon'
    if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
    if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
    if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.5)
  }
}
# legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("black","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)


boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="salmon", lwd=2, cex=1.5, lty=1, pch=16)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='salmon', lwd=2, cex=1.5, lty=2, pch=17)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='salmon', lwd=2, cex=1.5, lty=3, pch=15)
# legend("top", c("Concept","LoMexRec","HiMexRec"), col=c('black','black','black'), bty='n', ncol=2, lwd=2)
legend("top", c("M_BH","Concept","LoMexRec","HiMexRec"), col=c("salmon","grey45",'grey45','grey45'), lwd=2, lty=c(1, 1, 2, 3), bty='n', ncol=2)

########
dev.off()




###################### -----------------------------------------------



png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_FRQ_Results_ByImp.png",
    type="cairo",
    units="mm",
    width=300,
    height=400,
    pointsize=18,
    res=300)
######
par(mfrow=c(4,3), mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1.1,1.1,1.1,0.1))

## Probability of Recovery 
boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" ,ylim=c(0, 1.2))
mtext("Probability of Recovery by 2115", side=2, line=1)
for(i in rev(levels(as.factor(p.data.PR$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  if(stringr::str_detect(i,"Concept")){
    ltyy = 1; pchh=16
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  # if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  # if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
  
}
mtext("Concept", side=3, line=0.2)

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,1.1) )
for(i in rev(levels(as.factor(p.data.PR$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  # if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  if(stringr::str_detect(i,"LoMexRec")){
    ltyy = 2; pchh=17
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  # if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}

}
mtext("LoMexRec", side=3, line=0.2)

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0,1.1))
for(i in rev(levels(as.factor(p.data.PR$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  # if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  # if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  if(stringr::str_detect(i,"HiMexRec")){
    ltyy = 3; pchh=15
    lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  
}
mtext("HiMexRec", side=3, line=0.2)



### SSB
boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1)
abline(h=1)
for(i in rev(levels(as.factor(p.data.SSB$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  if(stringr::str_detect(i,"Concept")){
    ltyy = 1; pchh=16
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  # if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  # if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
  
}



boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in rev(levels(as.factor(p.data.SSB$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  # if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  if(stringr::str_detect(i,"LoMexRec")){
    ltyy = 2; pchh=17
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  # if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
  
}



boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in rev(levels(as.factor(p.data.SSB$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  # if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  # if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  if(stringr::str_detect(i,"HiMexRec")){
    ltyy = 3; pchh=15
    lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  
}




### Total commercial catch 
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
mtext("Total US Commercial Catch", side=2, line=1)
for(i in rev(levels(as.factor(p.data.TC$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  if(stringr::str_detect(i,"Concept")){
    ltyy = 1; pchh=16
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  # if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  # if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
  
}

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in rev(levels(as.factor(p.data.TC$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  # if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  if(stringr::str_detect(i,"LoMexRec")){
    ltyy = 2; pchh=17
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  # if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
 
}


boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="", lwd=2, border="grey75" )
for(i in rev(levels(as.factor(p.data.TC$name)))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  # if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  # if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  if(stringr::str_detect(i,"HiMexRec")){
    ltyy = 3; pchh=15
    lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=1.25)
    }
  
}


### POF

boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
mtext("Probability of Overfishing", side=2, line=1)
mtext("Assessment Frequency", side=1, line=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="grey45", lwd=2, cex=1.25, lty=1, pch=16)
# lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='b', col='blue', lwd=2)
# lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='b', col='red', lwd=2)
# legend("top", c("Predicted","Concept","LoMexRec","HiMexRec"), col=c('grey75','black','blue','red'), bty='n', ncol=2, lwd=2)
legend("top", c("Base","All OMs","BH"), col=c("black","grey45","deepskyblue"), lwd=2, lty=1, bty='n', ncol=2, cex=0.75)

boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
mtext("Assessment Frequency", side=1, line=1)
# lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='b', col="grey45", lwd=2)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='grey45', lwd=2, cex=1.25, lty=2, pch=17)
# lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='b', col='red', lwd=2)
legend("top", c("Hih","lnR0"), col=c("olivedrab3","darkorchid"), lwd=2, lty=1, bty='n', ncol=2, cex=0.75)


boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="",xlab="", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
mtext("Assessment Frequency", side=1, line=1)
# lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='b', col="grey45", lwd=2)
# lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='b', col='blue', lwd=2)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='grey45', lwd=2, cex=1.25, lty=3, pch=15)
legend("top", c("Loh","M_BH"), col=c("orange","salmon"), lwd=2, lty=1, bty='n', ncol=2, cex=0.75)




#####
dev.off()







##### TOTAL CATCH  #####
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\TC_FRQ_Results.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#####
par(mar=c(2.1, 2.1, 1.1, 1.1),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1,1,1,1))
layout(  matrix(c(1,2,1,3,4,4), ncol=2, byrow=TRUE) #, 
         # widths=c(3,1), 
         # heights=c(2,2)
)

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TC$name))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
  lines(pred_BT~c(1,2,3,4), data=p.data.TC[p.data.TC$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=2)
}

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext("Total US Commercial Catch", side=2, outer=T, line=-0.7)
#####
dev.off()


## PROB RECOVERY 
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\PR_FRQ_Results.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#####
par(mar=c(2.1, 2.1, 1.1, 1.1),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1,1,1,1))
layout(  matrix(c(1,2,1,3,4,4), ncol=2, byrow=TRUE) #, 
         # widths=c(3,1), 
         # heights=c(2,2)
)

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PR$name))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
  lines(pred~c(1,2,3,4), data=p.data.PR[p.data.PR$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=2)
}

boxplot(predict(PRp, type='response')~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(predict(PRp, type='response')~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(predict(PRp, type='response')~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext("Probability of Recovery by 2115", side=2, outer=T, line=-0.7)
#######
dev.off()



## SSB
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\SSB_FRQ_Results.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#####
par(mar=c(2.1, 2.1, 1.1, 1.1),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1,1,1,1))
layout(  matrix(c(1,2,1,3,4,4), ncol=2, byrow=TRUE) #, 
         # widths=c(3,1), 
         # heights=c(2,2)
)

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab="",xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 2))
abline(h=1)
for(i in levels(as.factor(p.data.SSB$name))){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  if(stringr::str_detect(i,"Concept")){ltyy = 1; pchh=16}
  if(stringr::str_detect(i,"LoMexRec")){ltyy = 2; pchh=17}
  if(stringr::str_detect(i,"HiMexRec")){ltyy = 3; pchh=15}
  lines(pred~c(1,2,3,4), data=p.data.SSB[p.data.SSB$name==i,], type='o', lwd=2, col=coll, lty=ltyy, pch=pchh, cex=2)
}
legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("black","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)

boxplot(predict(SSBp, type='response')~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(predict(SSBp, type='response')~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(predict(SSBp, type='response')~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, outer=T, line=-0.7)
#####
dev.off()




## POF
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\POF_FRQ_Results.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#####
par(mar=c(2.1, 2.1, 1.1, 1.1),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(1,1,1,1))
layout(  matrix(c(1,2,1,3,4,4), ncol=2, byrow=TRUE) #, 
         # widths=c(3,1), 
         # heights=c(2,2)
)

boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='b', col="grey45", lwd=2)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='b', col='blue', lwd=2)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='b', col='red', lwd=2)
legend("top", c("Predicted","Concept","LoMexRec","HiMexRec"), col=c('grey75','black','blue','red'), bty='n', ncol=2, lwd=2)

boxplot(predict(POF_p, type='response')~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(predict(POF_p, type='response')~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(predict(POF_p, type='response')~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext("Prob overfishing", side=2, outer=T, line=-0.7)
#####
dev.off()
