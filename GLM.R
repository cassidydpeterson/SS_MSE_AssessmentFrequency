####################
# MSE_AssessFRQ
# GLM 
# May 2021
###################

library(betareg)
library(doBy)


### GET DATA ###
load(file="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\FRQ_DATA.RData")

summary(FRQ_DATA)
str(FRQ_DATA)



FD = FRQ_DATA
FD$Imp = as.factor(FD$Imp)
FD$OM = as.factor(FD$OM)
FD$FRQ = as.numeric(as.character(FD$FRQ))
FD$TotComCatch = as.numeric(as.character(FD$TotComCatch))
FD$ProbRec = as.numeric(as.character(FD$ProbRec))
FD$SSB2115 = as.numeric(as.character(FD$SSB2115))
FD$F2115 = as.numeric(as.character(FD$F2115))
FD$POF = as.numeric(as.character(FD$POF))
FD$AAV = as.numeric(as.character(FD$AAV))
str(FD)





#############
# Meet Data
#############
par(mfrow=c(2,2))
plot(FD$TotComCatch~FD$Imp)
plot(FD$TotComCatch~FD$OM)
boxplot(FD$TotComCatch~FD$FRQ)
plot(FD$TotComCatch~FD$name)

plot(FD$ProbRec~FD$Imp)
plot(FD$ProbRec~FD$OM)
boxplot(FD$ProbRec~FD$FRQ)
plot(FD$ProbRec~FD$name)

plot(FD$SSB2115~FD$Imp)
plot(FD$SSB2115~FD$OM)
boxplot(FD$SSB2115~FD$FRQ)
abline(h=1)
plot(FD$SSB2115~FD$name)

plot(FD$F2115~FD$Imp, ylim=c(0,5)) ###### See when stock collapses F2115 >> 100
plot(FD$F2115~FD$OM, ylim=c(0,5))
boxplot(FD$F2115~FD$FRQ, ylim=c(0,5))
plot(FD$F2115~FD$name, ylim=c(0,5))

plot(FD$AAV~FD$Imp)
plot(FD$AAV~FD$OM)
boxplot(FD$AAV~FD$FRQ)
plot(FD$AAV~FD$name)

plot(FD$POF~FD$Imp)
plot(FD$POF~FD$OM)
boxplot(FD$POF~FD$FRQ)
abline(h=0.1)
plot(FD$POF~FD$name)

library(lattice)
histogram(~TotComCatch|as.factor(Imp),data=FD,xlab="TotComCatch")
histogram(~TotComCatch|as.factor(OM),data=FD,xlab="TotComCatch") #
histogram(~TotComCatch|as.factor(FRQ),data=FD,xlab="TotComCatch") #
histogram(~TotComCatch|as.factor(name),data=FD,xlab="TotComCatch") #
# NEG BIN

histogram(~ProbRec|as.factor(Imp),data=FD,xlab="ProbRec")
histogram(~ProbRec|as.factor(OM),data=FD,xlab="ProbRec") #
histogram(~ProbRec|as.factor(FRQ),data=FD,xlab="ProbRec") #
histogram(~ProbRec|as.factor(name),data=FD,xlab="ProbRec") #

histogram(~SSB2115|as.factor(Imp),data=FD,xlab="SSB2115")
histogram(~SSB2115|as.factor(OM),data=FD,xlab="SSB2115") #
histogram(~SSB2115|as.factor(FRQ),data=FD,xlab="SSB2115") #
histogram(~SSB2115|as.factor(name),data=FD,xlab="SSB2115") #
# NORMAL 

histogram(~F2115|as.factor(Imp),data=FD,xlab="F2115", xlim=c(0,3))
histogram(~F2115|as.factor(OM),data=FD,xlab="F2115") #
histogram(~F2115|as.factor(FRQ),data=FD,xlab="F2115") #
histogram(~F2115|as.factor(name),data=FD,xlab="F2115") #
hist(FD$F2115, xlim=c(0,3), breaks=10000)

histogram(~AAV|as.factor(Imp),data=FD,xlab="AAV")
histogram(~AAV|as.factor(OM),data=FD,xlab="AAV") #
histogram(~AAV|as.factor(FRQ),data=FD,xlab="AAV") #
histogram(~AAV|as.factor(name),data=FD,xlab="AAV") #

histogram(~POF|as.factor(Imp),data=FD,xlab="POF")
histogram(~POF|as.factor(OM),data=FD,xlab="POF") #
histogram(~POF|as.factor(FRQ),data=FD,xlab="POF") #
histogram(~POF|as.factor(name),data=FD,xlab="POF") #

# Look at graphical relationships
xyplot(FD$TotComCatch ~ FD$FRQ | as.factor(FD$name), xlab="FRQ",ylab="TotComCatch", col=1,data=FD) 
xyplot(FD$SSB2115 ~ FD$FRQ | as.factor(FD$name), xlab="FRQ",ylab="SSB2115", col=1,data=FD) 


# look for outliers
dotchart(FD$TotComCatch, groups=factor(FD$name), xlab="TotComCatch", ylab="", col=factor(FD$OM), pch=as.numeric(FD$Imp))
dotchart(FD$SSB2115, groups=factor(FD$name), xlab="SSB2115", ylab="", col=factor(FD$OM), pch=as.numeric(FD$Imp))
dotchart(FD$AAV, groups=factor(FD$name), xlab="AAV", ylab="", col=factor(FD$OM), pch=as.numeric(FD$Imp))
dotchart(FD$POF, groups=factor(FD$name), xlab="POF", ylab="", col=factor(FD$OM), pch=as.numeric(FD$Imp))


# Look for correlations
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(FD[complete.cases(FD),2:8], lower.panel=panel.cor)


hist(FD$TotComCatch)
hist(FD$POF)
hist(FD$AAV)
hist(FD$SSB2115)


#####################################################################
# GLM #
#####################################################################

FD_C = FD[complete.cases(FD),]

### TotCatch ####
# TC1=glm(log(TotComCatch)~FRQ+Imp+OM, data=FD_C, family=gaussian(link="identity"))
TC=glm(log(TotComCatch)~as.factor(FRQ)+Imp+OM, data=FD_C, family=gaussian(link="identity"))
TC2=glm(log(TotComCatch)~as.factor(FRQ)+Imp*OM, data=FD_C, family=gaussian(link="identity"))
TC3=glm(log(TotComCatch)~as.factor(FRQ)*name, data=FD_C, family=gaussian(link="identity"))
TC4=glm(log(TotComCatch)~as.factor(FRQ)*Imp*OM, data=FD_C, family=gaussian(link="identity")) ### THIS ONE
TC1=glm(log(TotComCatch)~as.factor(FRQ)+name, data=FD_C, family=gaussian(link="identity"))
TC5=glm(log(TotComCatch)~FRQ*Imp*OM, data=FD_C, family=gaussian(link="identity"))
# TC3=glm(log(TotComCatch)~Imp+OM, data=FD_C, family=gaussian(link="identity"))
# TC2=glm(TotComCatch~as.factor(FRQ)+Imp+OM, data=FD_C, family=Gamma(link="inverse"))

aics = AIC(TC, TC1, TC2, TC3,TC4,TC5)
aics$deltaAIC = aics$AIC - min(aics$AIC); aics

summary(TC4)
#diagnostic plots
par(mfrow=c(2,2))
plot(TC4)
boxplot(resid(TC2)~FD_C$name)
boxplot(resid(TC2)~FD_C$FRQ)
boxplot(resid(TC2)~FD_C$Imp)
boxplot(resid(TC2)~FD_C$OM)
summary(TC2)
car::vif(TC)
car::vif(TC1)
car::vif(TC2)
car::vif(TC4)

par(mfrow=c(1,1))
plot(predict(TC4, type="response")~log(FD_C$TotComCatch), ylim=c(0,15),xlim=c(0,15))
abline(a=0, b=1)
boxplot(predict(TC4, type="response")~FD_C$FRQ)
boxplot(predict(TC4, type="response")~FD_C$Imp)
boxplot(predict(TC4, type="response")~FD_C$OM)
boxplot(predict(TC4, type="response")~FD_C$name)

par(mfrow=c(2,2))
boxplot(predict(TC, type="response")~FD_C$FRQ)
boxplot(predict(TC1, type="response")~FD_C$FRQ)
boxplot(predict(TC2, type="response")~FD_C$FRQ)
abline(h=median(predict(TC2, type='response')))
boxplot(predict(TC3, type="response")~FD_C$FRQ)
abline(h=median(predict(TC3, type='response')))
boxplot(predict(TC4, type="response")~FD_C$FRQ)
abline(h=median(predict(TC4, type='response')))


plot(predict(TC, type="response")~fitted(TC))
plot(predict(TC, type="response")~fitted(TC))
abline(a=0,b=1)


# ExpTC_bt = exp(predict(TC, type='response') + 0.5*summary(TC)$dispersion)
# boxplot(ExpTC_bt~FD_C$FRQ) 
boxplot(exp(predict(TC, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ) 
boxplot(exp(predict(TC1, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ) 
boxplot(exp(predict(TC2, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ) 
boxplot(exp(predict(TC4, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ) 
  

boxplot(exp(predict(TC1, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ) 

boxplot(exp(predict(TC1, type='response') + 0.5*summary(TC)$dispersion)~FD_C$Imp) 

boxplot(exp(predict(TC1, type='response') + 0.5*summary(TC)$dispersion)~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2) 

boxplot(exp(predict(TC1, type='response') + 0.5*summary(TC)$dispersion)~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1) 


TCp = TC4
p.data=expand.grid(FRQ=c(1, 5, 10, 15),Imp=levels(FD_C$Imp),OM=levels(FD_C$OM))
p.data=cbind(p.data,pred=predict(TCp,newdata=p.data,type='response'))
p.data$pred_BT = exp(p.data$pred + (0.5*summary(TCp)$dispersion) )
p.data$name = paste0(p.data$Imp,"_",p.data$OM)
predTC4=summaryBy(pred_BT~FRQ,data=p.data,FUN=mean)
plot(pred_BT.mean~FRQ, data=predTC4, type='b')

boxplot(p.data$pred_BT ~ p.data$FRQ) 
boxplot(p.data$pred_BT ~ p.data$Imp)
boxplot(p.data$pred_BT ~ p.data$OM)
boxplot(p.data$pred_BT ~ p.data$Imp*p.data$OM)


p.data.TC = p.data




col2rgb("blue")
col2rgb("red")
col2rgb("forestgreen")
col2rgb("grey")
c(rgb(0.75, 0.75, 0.75, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(0.13, 0.55, 0.13, 0.5) )

TCf =TC5

par(mar=c(2.1, 2.1, 1.1, 1.1),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1, oma=c(1,1,1,1))
layout(  matrix(c(1,2,1,3,4,4), ncol=2, byrow=TRUE) #, 
         # widths=c(3,1), 
         # heights=c(2,2)
)

boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2, col=c(rgb(0.75, 0.75, 0.75, 0.5),rgb(0.13, 0.55, 0.13, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)) )

boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext("Total US Commercial Catch", side=2, outer=T, line=-0.75)





TCf =TC4

par(mar=c(2.1, 2.1, 1.1, 1.1),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1, oma=c(1,1,1,1))
layout(  matrix(c(1,2,1,3,4,4), ncol=2, byrow=TRUE) #, 
         # widths=c(3,1), 
         # heights=c(2,2)
)

boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2, col=c(rgb(0.75, 0.75, 0.75, 0.5),rgb(0.13, 0.55, 0.13, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)) )

boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(exp(predict(TCf, type='response') + 0.5*summary(TC)$dispersion)~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext("Total US Commercial Catch", side=2, outer=T, line=-0.75)



boxplot(exp(predict(TC1, type='response') + 0.5*summary(TC)$dispersion)~FD_C$FRQ, ylab="",xlab="Assessment Frequency")

# library(lme4)
# TC4=lmer(log(TotComCatch)~as.factor(FRQ)+(1|name), data=FD_C)
# summary(TC4)

anova(TC4, test="F")
car::Anova(TC)
TukeyHSD(aov(TC4))
plot(aov(TC4))


##### Prob Recovery #####
FD_C_L = subset(FD_C, FD_C$Imp=='LoMexRec')
PRa=glm(ProbRec ~ as.factor(FRQ)+OM, data=FD_C_L,family=binomial(link="logit"))
PR1a=glm(ProbRec ~ as.factor(FRQ)*OM, data=FD_C_L,family=binomial(link="logit"))
PR2a=glm(ProbRec ~ FRQ+OM, data=FD_C_L,family=binomial(link="logit"))
AIC(PRa, PR1a, PR2a)
summary(PRa)
boxplot(predict(PRa, type="response")~FD_C_L$FRQ)
boxplot(predict(PRa, type="response")~FD_C_L$OM)

FD_C_H = subset(FD_C, FD_C$Imp=='HiMexRec')
PRb=glm(ProbRec ~ as.factor(FRQ)+OM, data=FD_C_H,family=binomial(link="logit"))
PR1b=glm(ProbRec ~ as.factor(FRQ)*OM, data=FD_C_H,family=binomial(link="logit"))
PR2b=glm(ProbRec ~ FRQ+OM, data=FD_C_H,family=binomial(link="logit"))
AIC(PRb, PR1b, PR2b)
summary(PRb)
boxplot(predict(PRb, type="response")~FD_C_H$FRQ)
boxplot(predict(PRb, type="response")~FD_C_H$OM)


FD_C_C = subset(FD_C, FD_C$Imp=='Concept')
PRc=glm(ProbRec ~ as.factor(FRQ)+OM, data=FD_C_C,family=binomial(link="logit"))
PR2c=glm(ProbRec ~ FRQ+OM, data=FD_C_C,family=binomial(link="logit"))
PR1c=glm(ProbRec ~ as.factor(FRQ)*OM, data=FD_C_C,family=binomial(link="logit"))
AIC(PRc, PR1c, PR2c)
summary(PRc)
boxplot(predict(PRc, type="response")~FD_C_C$FRQ)
boxplot(predict(PRc, type="response")~FD_C_C$OM)


PR=glm(ProbRec ~ as.factor(FRQ)+Imp+OM, data=FD_C,family=binomial(link="logit"))
PRx=glm(ProbRec ~ FRQ+Imp+OM, data=FD_C,family=binomial(link="logit"))
PR1=glm(ProbRec ~ as.factor(FRQ)+Imp*OM, data=FD_C,family=binomial(link="logit"))
PR4=glm(ProbRec ~ as.factor(FRQ)*Imp*OM, data=FD_C,family=binomial(link="logit"))
PR5=glm(ProbRec ~ FRQ*Imp*OM, data=FD_C,family=binomial(link="logit")) ## Best except all zeros
PR6=glm(ProbRec ~ FRQ+Imp*OM, data=FD_C,family=binomial(link="logit"))

aics = AIC(PR, PRx, PR1, PR4, PR5, PR6)
aics$deltaAIC = aics$AIC - min(aics$AIC)
aics
summary(PR5)
summary(PR4)


PRp = PR5
p.data=expand.grid(FRQ=c(1,5,10,15),Imp=levels(FD_C$Imp),OM=levels(FD_C$OM))
p.data=cbind(p.data,pred=predict(PRp,newdata=p.data,type='response'))
p.data$name= paste0(p.data$Imp,"_",p.data$OM)
predPR5=summaryBy(pred~FRQ,data=p.data,FUN=mean)
plot(pred.mean~FRQ, data=predPR5, type='b', ylim=c(0,1))
predPR5a=summaryBy(pred~FRQ*OM,data=p.data,FUN=mean)
# plot(pred.mean~FRQ, data=predPR5a, type='b', ylim=c(0,1))
p.data.PR = p.data

# "grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"
plot(pred.mean~FRQ, data=predPR5a[predPR5a$OM=="Base",], type='b', ylim=c(0,1), lwd=2,  ylab="",xlab="Assessment Frequency")
lines(pred.mean~FRQ, data=predPR5a[predPR5a$OM=="BH",], type='b', col="deepskyblue", lwd=2)
lines(pred.mean~FRQ, data=predPR5a[predPR5a$OM=="Hih",], type='b', col='olivedrab3', lwd=2)
lines(pred.mean~FRQ, data=predPR5a[predPR5a$OM=="lnR0",], type='b', col='darkorchid', lwd=2)
lines(pred.mean~FRQ, data=predPR5a[predPR5a$OM=="Loh",], type='b', col='orange', lwd=2)
lines(pred.mean~FRQ, data=predPR5a[predPR5a$OM=="M_BH",], type='b', col='salmon', lwd=2)

boxplot(predict(PRp, type='response')~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(predict(PRp, type='response')~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(predict(PRp, type='response')~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 

mtext("Probability of Recovery", side=2, outer=T, line=-0.75)


boxplot(predict(PRp, type='response')~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2, col=c(rgb(0.75, 0.75, 0.75, 0.5),rgb(0.13, 0.55, 0.13, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)) )



boxplot(p.data$pred ~ p.data$FRQ)
boxplot(predict(PR1, type="response")~FD_C$FRQ)


predPR5=summaryBy(pred~FRQ,data=p.data,FUN=mean)






p.data=expand.grid(FRQ=c(1,5,10,15),Imp=levels(FD_C$Imp),OM=levels(FD_C$OM))
p.data=cbind(p.data,pred=predict(PR4,newdata=p.data,type='response'))
predPR4=summaryBy(pred~FRQ,data=p.data,FUN=mean)
plot(pred.mean~FRQ, data=predPR5, type='b', ylim=c(0,1))

boxplot(p.data$pred ~ p.data$FRQ)
boxplot(predict(PR4, type="response")~FD_C$FRQ)


predPR5=summaryBy(pred~FRQ,data=p.data,FUN=mean)

summary(PR1)
par(mfrow=c(2,2))
plot(PR1)
par(mfrow=c(1,1))
boxplot(predict(PR1, type="response")~FD_C$FRQ)
boxplot(predict(PR1, type="response")~FD_C$Imp)
boxplot(predict(PR1, type="response")~FD_C$OM)
boxplot(predict(PR1, type="response")~FD_C$name)

AIC(PR, PR1, PR2, PR3)

vioplot(predict(PR, type="response")~FD_C$FRQ)
vioplot(predict(PR, type="response")~FD_C$Imp)
vioplot(predict(PR, type="response")~FD_C$OM)
vioplot(predict(PR, type="response")~FD_C$name)


anova(PR1, test="Chisq")
car::Anova(PR1)


## SSB #####
SSB=glm(SSB2115~as.factor(FRQ)+Imp+OM, data=FD_C, family=gaussian(link="identity"))
SSB1=glm(SSB2115~as.factor(FRQ)+Imp*OM, data=FD_C, family=gaussian(link="identity"))
SSB4=glm(SSB2115~as.factor(FRQ)*Imp*OM, data=FD_C, family=gaussian(link="identity")) ###
SSB5=glm(SSB2115~FRQ*Imp*OM, data=FD_C, family=gaussian(link="identity"))
SSB6=glm(SSB2115~FRQ+Imp*OM, data=FD_C, family=gaussian(link="identity"))
aics = AIC(SSB, SSB1,  SSB4, SSB5, SSB6)
aics$deltaAIC = aics$AIC - min(aics$AIC); aics
summary(SSB4)
summary(SSB5)
par(mfrow=c(2,2))
plot(SSB4)

par(mfrow=c(1,1))
plot(predict(SSB, type="response")~FD_C$SSB2115)
abline(a=0,b=1)
boxplot(predict(SSB, type="response")~FD_C$FRQ)
abline(h=1)
boxplot(predict(SSB, type="response")~FD_C$Imp)
abline(h=1)
boxplot(predict(SSB, type="response")~FD_C$OM)
abline(h=1)
boxplot(predict(SSB, type="response")~FD_C$name)
abline(h=1)

SSB_C=glm(SSB2115~FRQ+Imp+OM, data=FD_C, family=gaussian(link="identity"))
summary(SSB_C)
par(mfrow=c(2,2))
plot(SSB_C)

par(mfrow=c(1,1))
plot(predict(SSB_C, type="response")~FD_C$SSB2115)
abline(a=0,b=1)
boxplot(predict(SSB_C, type="response")~FD_C$FRQ)
boxplot(predict(SSB_C, type="response")~FD_C$Imp)
boxplot(predict(SSB_C, type="response")~FD_C$OM)




SSBp = SSB4
p.data=expand.grid(FRQ=c(1, 5, 10, 15),Imp=levels(FD_C$Imp),OM=levels(FD_C$OM))
p.data=cbind(p.data,pred=predict(SSBp,newdata=p.data,type='response'))
predSSB1=summaryBy(pred~FRQ,data=p.data,FUN=mean)
plot(pred.mean~FRQ, data=predSSB1, type='b', ylim=c(0,1))
boxplot(pred~FRQ, data=p.data, type='b', ylim=c(0,2))
abline(h=1)

boxplot(pred~Imp, data=p.data, type='b', ylim=c(0,2))
abline(h=1)
boxplot(pred~OM, data=p.data, type='b', ylim=c(0,2))
abline(h=1)
boxplot(pred~Imp*OM, data=p.data, type='b', ylim=c(0,2))
abline(h=1)


predSSB1a=summaryBy(pred~FRQ*OM,data=p.data,FUN=mean)
p.data$name = paste0(p.data$Imp,"_",p.data$OM)
p.data.SSB=p.data
# "grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"
plot(pred.mean~FRQ, data=predSSB1a[predSSB1a$OM=="Base",], type='b', ylim=c(0,2), lwd=2,  ylab="",xlab="Assessment Frequency")
lines(pred.mean~FRQ, data=predSSB1a[predSSB1a$OM=="BH",], type='b', col="deepskyblue", lwd=2)
lines(pred.mean~FRQ, data=predSSB1a[predSSB1a$OM=="Hih",], type='b', col='olivedrab3', lwd=2)
lines(pred.mean~FRQ, data=predSSB1a[predSSB1a$OM=="lnR0",], type='b', col='darkorchid', lwd=2)
lines(pred.mean~FRQ, data=predSSB1a[predSSB1a$OM=="Loh",], type='b', col='orange', lwd=2)
lines(pred.mean~FRQ, data=predSSB1a[predSSB1a$OM=="M_BH",], type='b', col='salmon', lwd=2)

par(mfrow=c(1,1))
plot(pred~FRQ, data=p.data[p.data$name=="Concept_Base",], type='b', ylim=c(0,2), lwd=2,  ylab="",xlab="Assessment Frequency")
for(i in levels(as.factor(p.data$name))){
  if(stringr::str_detect(i,"Base")){coll='grey45'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  if(stringr::str_detect(i,"Concept")){ltyy = 1}
  if(stringr::str_detect(i,"LoMexRec")){ltyy = 2}
  if(stringr::str_detect(i,"HiMexRec")){ltyy = 3}
  lines(pred~FRQ, data=p.data[p.data$name==i,], type='b', lwd=2, col=coll, lty=ltyy)
}
legend("bottom", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)


par(mfrow=c(2,2))
plot(pred~FRQ, data=p.data[p.data$name=="Concept_Base",], type='b', ylim=c(0,2), lwd=2,  ylab="",xlab="Assessment Frequency", col="white")
for(i in levels(as.factor(p.data$name))){
  if(stringr::str_detect(i,"Concept")){
    if(stringr::str_detect(i,"Base")){coll='grey45'}
    if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
    if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
    if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
    if(stringr::str_detect(i,"Loh")){coll='orange'}
    if(stringr::str_detect(i,"M_BH")){coll='salmon'}
    lines(pred~FRQ, data=p.data[p.data$name==i,], type='b', lwd=2, col=coll, lty=ltyy)
  }
}

plot(pred~FRQ, data=p.data[p.data$name=="Concept_Base",], type='b', ylim=c(0,2), lwd=2,  ylab="",xlab="Assessment Frequency", col="white")
for(i in levels(as.factor(p.data$name))){
  if(stringr::str_detect(i,"LoMexRec")){
    if(stringr::str_detect(i,"Base")){coll='grey45'}
    if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
    if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
    if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
    if(stringr::str_detect(i,"Loh")){coll='orange'}
    if(stringr::str_detect(i,"M_BH")){coll='salmon'}
    lines(pred~FRQ, data=p.data[p.data$name==i,], type='b', lwd=2, col=coll, lty=2)
  }
}


plot(pred~FRQ, data=p.data[p.data$name=="Concept_Base",], type='b', ylim=c(0,2), lwd=2,  ylab="",xlab="Assessment Frequency", col="white")
for(i in levels(as.factor(p.data$name))){
  if(stringr::str_detect(i,"HiMexRec")){
    if(stringr::str_detect(i,"Base")){coll='grey45'}
    if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
    if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
    if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
    if(stringr::str_detect(i,"Loh")){coll='orange'}
    if(stringr::str_detect(i,"M_BH")){coll='salmon'}
    lines(pred~FRQ, data=p.data[p.data$name==i,], type='b', lwd=2, col=coll, lty=3)
  }
}
plot.new()
legend("center", c("Base","BH","Hih","lnR0","Loh","M_BH","Concept","LoMexRec","HiMexRec"), col=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon","black",'black','black'), lwd=2, lty=c(rep(1, 6), 1, 2, 3), bty='n', ncol=3)









#### POF! ####

POFa = (FD_C$POF*(length(FD_C$POF)-1) + 0.5) / (length(FD_C$POF))


hist(logit(POFa))
hist(POFa)
POF_1 = glm(POFa~as.factor(FRQ)+Imp+OM, data=FD_C, family=gaussian(link="logit"))
POF_2 = glm(POFa~as.factor(FRQ)+Imp*OM, data=FD_C, family=gaussian(link="logit"))
POF_3 = glm(POFa~as.factor(FRQ)*Imp*OM, data=FD_C, family=gaussian(link="logit"))
POF_4 = glm(POFa~FRQ+Imp+OM, data=FD_C, family=gaussian(link="logit"))
POF_5 = glm(POFa~FRQ+Imp*OM, data=FD_C, family=gaussian(link="logit"))
POF_6 = glm(POFa~FRQ*Imp*OM, data=FD_C, family=gaussian(link="logit"))
POF_7 = glm(POFa~as.factor(FRQ)+Imp, data=FD_C, family=gaussian(link="logit"))
POF_8 = glm(POFa~as.factor(FRQ)*Imp, data=FD_C, family=gaussian(link="logit"))
POF_9 = glm(POFa~FRQ+Imp, data=FD_C, family=gaussian(link="logit"))
POF_10 = glm(POFa~FRQ*Imp, data=FD_C, family=gaussian(link="logit"))

aics = AIC(POF_1, POF_2, POF_3, POF_4, POF_5, POF_6, POF_7, POF_8, POF_9, POF_10)
aics$deltaAIC = aics$AIC - min(aics$AIC); aics
plot(POF_8)
summary(POF_8)



boxplot(predict(POF, type="response")~FD_C$FRQ)
boxplot(predict(POF, type="response")~FD_C$Imp)
boxplot(predict(POF, type="response")~FD_C$OM)
boxplot(predict(POF, type="response")~FD_C$name)






boxplot(pred~Imp, data=p.data, type='b', ylim=c(0,2))
abline(h=1)
boxplot(pred~OM, data=p.data, type='b', ylim=c(0,2))
abline(h=1)
boxplot(pred~Imp*OM, data=p.data, type='b', ylim=c(0,2))
abline(h=1)

POF_p = POF_8
p.data=expand.grid(FRQ=c(1, 5, 10, 15),Imp=levels(FD_C$Imp))
p.data=cbind(p.data,pred=predict(POF_p,newdata=p.data,type='response'))
predPOF=summaryBy(pred~FRQ,data=p.data,FUN=mean)
plot(pred.mean~FRQ, data=predPOF, type='b', ylim=c(0,1))
boxplot(pred~FRQ, data=p.data, type='b', ylim=c(0,2))
abline(h=1)

predPOFa=summaryBy(pred~FRQ*Imp,data=p.data,FUN=mean)
p.data.POF = p.data
# "grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"
# boxplot(p.data$pred~p.data$FRQ,  ylab="",xlab="Assessment Frequency", ylim=c(0,0.6))
boxplot(predict(POF_8, type="response")~FD_C$FRQ,  ylab="",xlab="Assessment Frequency", ylim=c(0,0.6))
# plot(pred.mean~FRQ, data=predPOF, type='b', lwd=2,  ylab="",xlab="Assessment Frequency", ylim=c(0,0.6))
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='b', col="grey45", lwd=2)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='b', col='blue', lwd=2)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='b', col='red', lwd=2)
legend("top", c("Predicted","Concept","LoMexRec","HiMexRec"), col=c('black','grey45','blue','red'), bty='n', ncol=2, lwd=2)






POFp = FD_C$POF+0.01
POFa=betareg(POFp ~ as.factor(FRQ)*Imp*OM, data=FD_C, link="logit")
POFb=betareg(POFp ~ as.factor(FRQ)*Imp*OM, data=FD_C, link="loglog") #
POFc=betareg(POFp ~ as.factor(FRQ)*Imp*OM, data=FD_C, link="probit")
POFd=betareg(POFp ~ as.factor(FRQ)*Imp*OM, data=FD_C, link="log")
POFe=betareg(POFp ~ as.factor(FRQ)*Imp*OM, data=FD_C, link="cauchit")
POFf=betareg(POFp ~ as.factor(FRQ)*Imp*OM, data=FD_C, link="cloglog")

POFa=betareg(POFp ~ as.factor(FRQ)+Imp+OM, data=FD_C, link="logit")
POFb=betareg(POFp ~ as.factor(FRQ)+Imp+OM, data=FD_C, link="loglog") #
POFc=betareg(POFp ~ as.factor(FRQ)+Imp+OM, data=FD_C, link="probit")
POFd=betareg(POFp ~ as.factor(FRQ)+Imp+OM, data=FD_C, link="log")
POFe=betareg(POFp ~ as.factor(FRQ)+Imp+OM, data=FD_C, link="cauchit")
POFf=betareg(POFp ~ as.factor(FRQ)+Imp+OM, data=FD_C, link="cloglog")
AIC(POFa, POFb, POFc, POFd, POFe, POFf)

par(mfrow=c(2,2))
plot(POFb)

TC=glm(log(TotComCatch)~as.factor(FRQ)+Imp+OM, data=FD_C, family=gaussian(link="identity"))


par(mfrow=c(2,2))

POFb = (FD_C$POF*100)


POF=betareg((POF+0.01) ~ as.factor(FRQ)+Imp+OM, data=FD_C, link="loglog")
plot(POF)

POF1=glm(POF ~ as.factor(FRQ)+Imp*OM, data=FD_C,family=binomial(link="log"))
summary(POF1)
plot(POF1)


POF1=glm(ProbRec ~ as.factor(FRQ)+Imp*OM, data=FD_C,family=binomial(link="logit"))
POF3=glm(ProbRec ~ as.factor(FRQ)+Imp:OM, data=FD_C,family=binomial(link="logit"))
POF2=glm(ProbRec ~ as.factor(FRQ)+name, data=FD_C,family=binomial(link="logit"))
summary(POF2)
par(mfrow=c(2,2))
plot(POF)
par(mfrow=c(1,1))
boxplot(predict(POF, type="response")~FD_C$FRQ)
boxplot(predict(POF, type="response")~FD_C$Imp)
boxplot(predict(POF, type="response")~FD_C$OM)
boxplot(predict(POF, type="response")~FD_C$name)




anova(PR1, test="Chisq")
car::Anova(PR1)

AIC(POF, POF1, POF2, POF3)


######## PLOT ALL #######################

png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_FRQ_Results2.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#####
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TC1, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, col=c("grey45","deepskyblue","olivedrab3","darkorchid") )
boxplot(predict(PR1, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, col=c("grey45","deepskyblue","olivedrab3","darkorchid") )
boxplot(predict(SSB, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, col=c("grey45","deepskyblue","olivedrab3","darkorchid") , ylim=c(0, 1.5))
abline(h=1)
## AAV BOXPLOT INSTEAD OF FMSY B/C OF THE COMPLICATING SCENARIOS OF STOCK COLLAPSE
boxplot(predict(POF_8, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, col=c("grey45","deepskyblue","olivedrab3","darkorchid"), ylim=c(0,0.6))
abline(h=1)
#####
dev.off()




png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\ALL_FRQ_Results.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
#######
par(mfrow=c(2,2), mar=c(2.1, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(1, 0.1, 0), cex=1, oma=c(0.1,0.1,0.1,0.1))
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in rev(levels(as.factor(p.data.TC$name))) ){
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

boxplot(predict(PRp, type='response')~FD_C$FRQ , ylab="Probability of Recovery by 2115",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in rev(levels(as.factor(p.data.PR$name))) ){
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

boxplot(predict(SSBp, type='response')~FD_C$FRQ , ylab=expression("SSB"["2115"]*" / SSB"["MSY"]),xlab="Assessment Frequency", lwd=2, border="grey75" , ylim=c(0, 1.75))
abline(h=1)
for(i in rev(levels(as.factor(p.data.SSB$name))) ){
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

boxplot(predict(POF_p, type='response')~FD_C$FRQ, 
        ylab="Probability of Overfishing",xlab="Assessment Frequency", lwd=2, border="grey75", ylim=c(0,0.6))
# abline(h=1)
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='o', col="grey45", lwd=2, lty=1, pch=16, cex=1.5)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='o', col='grey45', lwd=2, lty=2, pch=17, cex=1.5)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='o', col='grey45', lwd=2, lty=3, pch=15, cex=1.5)
legend("top", c("Concept","LoMexRec","HiMexRec"), col=c('grey45','grey45','grey45'), bty='n', ncol=2, lwd=2, lty=c(1,2,3), pch=c(16, 17, 15), pt.cex=1.5)
########
dev.off()





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


#----------------------------------------------------------------------------


##### TOTAL CATCH  #####
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\TC_FRQ_Results1.png",
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

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2,  col=c(rgb(0.75, 0.75, 0.75, 0.5),rgb(0.13, 0.55, 0.13, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)) )

boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(exp(predict(TCp, type='response') + 0.5*summary(TCp)$dispersion)~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext("Total US Commercial Catch", side=2, outer=T, line=0)
#####
dev.off()


## PROB RECOVERY 
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\PR_FRQ_Results1.png",
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

boxplot(predict(PRp, type='response')~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2,  col=c(rgb(0.75, 0.75, 0.75, 0.5),rgb(0.13, 0.55, 0.13, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)) )

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
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\SSB_FRQ_Results1.png",
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

boxplot(predict(SSBp, type='response')~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2,  col=c(rgb(0.75, 0.75, 0.75, 0.5),rgb(0.13, 0.55, 0.13, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)) )

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
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\POF_FRQ_Results1.png",
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

boxplot(predict(POF_p, type='response')~FD_C$FRQ, ylab="",xlab="Assessment Frequency", lwd=2,  col=c(rgb(0.75, 0.75, 0.75, 0.5),rgb(0.13, 0.55, 0.13, 0.5), rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)), ylim=c(0, 0.6) )

boxplot(predict(POF_p, type='response')~FD_C$Imp, border=c("black","blue",'red'),ylab="",xlab="Implementation Model", lwd=2) 
boxplot(predict(POF_p, type='response')~FD_C$OM, border=c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), lwd=2,ylab="",xlab="OM") 
boxplot(predict(POF_p, type='response')~FD_C$name, border=rep(c("grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"), 3), lwd=1, 
        names=c("Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H"), ylab="",xlab="OM * Imp Mod") 
mtext("Prob Overfishing", side=2, outer=T, line=-0.7)
#####
dev.off()








#######
# TRADEOFF BETWEEN SSB and TotComCatch
png(filename="D:\\MSE_Run\\Assessment_Frequency\\AssessFreq_Results\\Plots\\Tradeoff_plot.png",
    type="cairo",
    units="mm",
    width=300,
    height=300,
    pointsize=18,
    res=300)
par(mfrow=c(1,1))
plot(predict(SSBp, type='response') ~ exp(predict(TCp, type='response') + 0.5*summary(TC)$dispersion), col=as.numeric(as.factor(FD_C$FRQ)), pch=as.numeric(FD_C$name), xlab="Total US Commercial Catch",ylab=expression("SSB"["2015"]*" / SSB"["MSY"])) # COLOR CODE BY 
abline(h=1)
legend("bottomright",c(
                       "Base_C","BH_C","Hih_C","lnR0_C","Loh_C","M_BH_C",
                       "Base_L","BH_L","Hih_L","lnR0_L","Loh_L","M_BH_L",
                       "Base_H","BH_H","Hih_H","lnR0_H","Loh_H","M_BH_H",
                       "FRQ1","FRQ5","FRQ10","FRQ15"), 
       col=c( rep(8,18),1:4 ), 
       pch=as.numeric(c(levels(as.factor(as.numeric(FD_C$name))),rep(19, 4) ) ) , 
       bty="n", ncol=4  )
mtext("Tradeoff plot",3,line=0.5)
dev.off()





## POF ### 
predPOFa=summaryBy(pred~FRQ*Imp,data=p.data,FUN=mean)
# "grey45","deepskyblue","olivedrab3","darkorchid","orange","salmon"
# boxplot(p.data$pred~p.data$FRQ,  ylab="",xlab="Assessment Frequency", ylim=c(0,0.6))
boxplot(predict(POF, type="response")~FD_C$FRQ,  ylab="",xlab="Assessment Frequency", ylim=c(0,0.6))
# plot(pred.mean~FRQ, data=predPOF, type='b', lwd=2,  ylab="",xlab="Assessment Frequency", ylim=c(0,0.6))
lines(p.data.POF[p.data.POF$Imp=="Concept",]$pred~c(1,2,3,4), type='b', col="grey45", lwd=2)
lines(p.data.POF[p.data.POF$Imp=="LoMexRec",]$pred~c(1,2,3,4), type='b', col='blue', lwd=2)
lines(p.data.POF[p.data.POF$Imp=="HiMexRec",]$pred~c(1,2,3,4), type='b', col='red', lwd=2)
legend("top", c("Predicted","Concept","LoMexRec","HiMexRec"), col=c('black','grey45','blue','red'), bty='n', ncol=2, lwd=2)


#### TRADEOFF PLOT W ERROR BARS ###
SSB_pred = as.data.frame(cbind(FD_C, SSB = predict(SSB1, type='response')))
TC_pred = as.data.frame(cbind(FD_C, TC = exp(predict(TC1, type='response') + 0.5*summary(TC)$dispersion) ))

library(doBy)
SSB_TO = as.data.frame(cbind(summaryBy(SSB~FRQ*Imp, data=SSB_pred, FUN=mean), SSB.sd=summaryBy(SSB~FRQ*Imp, data=SSB_pred, FUN=sd)[,3]))

TC_TO = as.data.frame(cbind(summaryBy(TC~FRQ*Imp, data=TC_pred, FUN=mean), TC.sd = summaryBy(TC~FRQ*Imp, data=TC_pred, FUN=sd)[,3]))



cols=c("black",'black','black','deepskyblue','deepskyblue','deepskyblue','darkolivegreen','darkolivegreen','darkolivegreen','purple','purple','purple')
pchs=c(15, 16, 17,15, 16, 17,15, 16, 17,15, 16, 17)



plot(SSB_TO$SSB.mean~TC_TO$TC.mean, col=cols,pch=pchs, ylab="", xlab="", ylim=c(0.25, 1.5), xlim=c(0, 20000), cex=2)
arrows(x0=TC_TO$TC.mean, 
         y0=SSB_TO$SSB.mean - SSB_TO$SSB.sd,
         y1=SSB_TO$SSB.mean + SSB_TO$SSB.sd,
         code=3, angle=90, length=0, col=cols, lwd=1.5)
  arrows(y0=SSB_TO$SSB.mean, 
         x0=TC_TO$TC.mean - TC_TO$TC.sd,
         x1=TC_TO$TC.mean + TC_TO$TC.sd,
         code=3, angle=90, length=0, col=cols, lwd=1.5)

mtext("Tradeoff plot", side=3, line=0.25, cex=1.5)
mtext(expression("SSB"["2015"]*"SSB"["MSY"]), side=2, line=1, cex=1)
mtext("Total US Commercial Catch", side=1, line=1, cex=1)

legend("bottomright",c("FRQ1","FRQ5","FRQ10","FRQ15","Concept","LoMexRec","HiMexRec"), col=c("black",'deepskyblue','darkolivegreen','purple', 'grey45', 'grey45', 'grey45'), pch=c(18, 18, 18, 18, 15, 16, 17), bty='n', ncol=2, pt.cex=1.5)




## F ######### DO SOMETHIGN ABOUT THE MAJOR OUTLIERS IN THIS ONE. USE F2065?? ##########
FD_C_subsetF = subset(FD_C, FD_C$F2115<3)

summary(FD_C$F2115); hist(FD_C$F2115)
summary(FD_C_subsetF$F2115); hist(FD_C_subsetF$F2115) ; hist(log(FD_C_subsetF$F2115))

FM=glm(log(F2115+0.01)~as.factor(FRQ)+Imp+OM, data=FD_C_subsetF, family=gaussian(link="identity"))
FM1=glm(log(F2115+0.01)~as.factor(FRQ)+Imp*OM, data=FD_C_subsetF, family=gaussian(link="identity"))
FM2=glm(log(F2115+0.01)~as.factor(FRQ)+name, data=FD_C_subsetF, family=gaussian(link="identity"))
FM=glm(log(F2115+0.01)~FRQ+Imp+OM, data=FD_C_subsetF, family=gaussian(link="identity"))
summary(FM)
summary(FM1)
car::vif(FM1)
par(mfrow=c(2,2))
plot(FM1)

par(mfrow=c(1,1))
plot(predict(FM, type="response")~FD_C_subsetF$SSB2115)
abline(a=0,b=1)
boxplot(predict(FM, type="response")~FD_C_subsetF$FRQ)
abline(h=0)
boxplot(predict(FM, type="response")~FD_C_subsetF$Imp)
abline(h=0)
boxplot(predict(FM, type="response")~FD_C_subsetF$OM)
abline(h=0)

# exp(predict(FM, type='response') + 0.5*summary(FM)$dispersion)
par(mfrow=c(1,1))
plot((exp(predict(FM, type='response') + 0.5*summary(FM)$dispersion)-0.01)~FD_C_subsetF$F2115)
abline(a=0,b=1)
boxplot((exp(predict(FM, type='response') + 0.5*summary(FM)$dispersion)-0.01)~FD_C_subsetF$FRQ)
abline(h=1)
boxplot((exp(predict(FM, type='response') + 0.5*summary(FM)$dispersion)-0.01)~FD_C_subsetF$Imp)
abline(h=1)
boxplot((exp(predict(FM, type='response') + 0.5*summary(FM)$dispersion)-0.01)~FD_C_subsetF$OM)
abline(h=1)
boxplot((exp(predict(FM, type='response') + 0.5*summary(FM)$dispersion)-0.01)~FD_C_subsetF$name)
abline(h=1)



########################
# SEPARATE IMP SCENARIOS 
########################
FD_CC = subset(FD_C, FD_C$Imp=="Concept")
FD_CL = subset(FD_C, FD_C$Imp=="LoMexRec")
FD_CH = subset(FD_C, FD_C$Imp=="HiMexRec")



#### TotCatch #####
# CONCEPT
TCC1=glm(log(TotComCatch)~as.factor(FRQ)+OM, data=FD_CC, family=gaussian(link="identity"))
TCC2=glm(log(TotComCatch)~as.factor(FRQ)*OM, data=FD_CC, family=gaussian(link="identity"))
TCC3=glm(log(TotComCatch)~FRQ+OM, data=FD_CC, family=gaussian(link="identity"))
TCC4=glm(log(TotComCatch)~FRQ*OM, data=FD_CC, family=gaussian(link="identity"))
aics=AIC(TCC1, TCC2, TCC3, TCC4)
aics$deltaAIC = aics$AIC - min(aics$AIC); aics
TCC = TCC3
summary(TCC)
par(mfrow=c(2,2))
plot(TCC)

par(mfrow=c(1,1))
boxplot(predict(TCC, type="response")~FD_CC$FRQ)
boxplot(predict(TCC, type="response")~FD_CC$OM)

p.data=expand.grid(FRQ=c(1, 5, 10, 15),OM=levels(FD_C$OM))
p.data=cbind(p.data,pred=predict(TCC,newdata=p.data,type='response'))
p.data$pred_BT = exp(p.data$pred + (0.5*summary(TCC)$dispersion) )
p.data.TCC=p.data
# predTCC=summaryBy(pred_BT~FRQ,data=p.data,FUN=mean)
# plot(predTCC)
# plot(pred_BT.mean~FRQ, data=predTCC, type='b')

par(mfrow=c(1,1))
boxplot(exp(predict(TCC, type='response') + 0.5*summary(TCC)$dispersion)~FD_CC$FRQ, ylab="Total US Commercial Catch",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.TCC$OM)) ){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  lines(pred_BT~c(1,2,3,4), data=p.data.TCC[p.data.TCC$OM==i,], type='o', lwd=2, col=coll, cex=2, pch=16)
}


#LoMexRec
TCL=glm(log(TotComCatch)~as.factor(FRQ)+OM, data=FD_CL, family=gaussian(link="identity"))
summary(TCL)
par(mfrow=c(2,2))
plot(TCL)

par(mfrow=c(1,1))
boxplot(predict(TCL, type="response")~FD_CL$FRQ)
boxplot(predict(TCL, type="response")~FD_CL$OM)

#HiMexRec
TCH=glm(log(TotComCatch)~as.factor(FRQ)+OM, data=FD_CH, family=gaussian(link="identity"))
summary(TCH)
par(mfrow=c(2,2))
plot(TCH)

par(mfrow=c(1,1))
boxplot(predict(TCH, type="response")~FD_CH$FRQ)
boxplot(predict(TCH, type="response")~FD_CH$OM)


##### Prob Recovery #####

PRc1=glm(ProbRec ~ as.factor(FRQ)+OM, data=FD_CC,family=binomial(link="logit"))
PRc2=glm(ProbRec ~ FRQ+OM, data=FD_CC,family=binomial(link="logit"))
PRc3=glm(ProbRec ~ as.factor(FRQ)*OM, data=FD_CC,family=binomial(link="logit"))
PRc4=glm(ProbRec ~ FRQ*OM, data=FD_CC,family=binomial(link="logit"))
aics = AIC(PRc1, PRc2, PRc3, PRc4)
aics$deltaAIC = aics$AIC - min(aics$AIC); aics
PRc = PRc2
summary(PRc)
par(mfrow=c(2,2))
plot(PRc)


boxplot(predict(PRc, type="response")~FD_CC$FRQ)
boxplot(predict(PRc, type="response")~FD_CC$OM)

p.data=expand.grid(FRQ=c(1, 5, 10, 15),OM=levels(FD_C$OM))
p.data=cbind(p.data,pred=predict(PRc,newdata=p.data,type='response'))
p.data.PRC=p.data
# predTCC=summaryBy(pred_BT~FRQ,data=p.data,FUN=mean)
# plot(predTCC)
# plot(pred_BT.mean~FRQ, data=predTCC, type='b')

par(mfrow=c(1,1))
boxplot(predict(PRc, type='response') ~FD_CC$FRQ, ylab="Probability of Recovery",xlab="Assessment Frequency", lwd=2, border="grey75" )
for(i in levels(as.factor(p.data.PRC$OM)) ){
  if(stringr::str_detect(i,"Base")){coll='black'}
  if(stringr::str_detect(i,"BH")){coll='deepskyblue'}
  if(stringr::str_detect(i,"Hih")){coll='olivedrab3'}
  if(stringr::str_detect(i,"lnR0")){coll='darkorchid'}
  if(stringr::str_detect(i,"Loh")){coll='orange'}
  if(stringr::str_detect(i,"M_BH")){coll='salmon'}
  lines(pred~c(1,2,3,4), data=p.data.PRC[p.data.TCC$OM==i,], type='o', lwd=2, col=coll, cex=2, pch=16)
}




boxplot(predict(PRc, type='response', newdata=FD_CC[FD_CC$OM=="Base",])~FD_CC[FD_CC$OM=="Base",]$FRQ)
par(mfrow=c(3,2))
boxplot(FD_CC[FD_CC$OM=="Base",]$TotComCatch~FD_CC[FD_CC$OM=="Base",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="Base",]$TotComCatch~FD_CC[FD_CC$OM=="Base",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="Base",]$TotComCatch~FD_CC[FD_CC$OM=="Base",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="Base",]$ProbRec~FD_CC[FD_CC$OM=="Base",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="Base",]$ProbRec~FD_CC[FD_CC$OM=="Base",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="Base",]$ProbRec~FD_CC[FD_CC$OM=="Base",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="Base",]$SSB2115~FD_CC[FD_CC$OM=="Base",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="Base",]$SSB2115~FD_CC[FD_CC$OM=="Base",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="Base",]$SSB2115~FD_CC[FD_CC$OM=="Base",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="Base",]$POF~FD_CC[FD_CC$OM=="Base",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="Base",]$POF~FD_CC[FD_CC$OM=="Base",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="Base",]$POF~FD_CC[FD_CC$OM=="Base",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="Base",]$AAV~FD_CC[FD_CC$OM=="Base",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="Base",]$AAV~FD_CC[FD_CC$OM=="Base",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="Base",]$AAV~FD_CC[FD_CC$OM=="Base",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="Base",]$F2115~FD_CC[FD_CC$OM=="Base",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="Base",]$F2115~FD_CC[FD_CC$OM=="Base",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="Base",]$F2115~FD_CC[FD_CC$OM=="Base",]$FRQ) )



par(mfrow=c(3,2))
boxplot(FD_CC[FD_CC$OM=="BH",]$TotComCatch~FD_CC[FD_CC$OM=="BH",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="BH",]$TotComCatch~FD_CC[FD_CC$OM=="BH",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="BH",]$TotComCatch~FD_CC[FD_CC$OM=="BH",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="BH",]$ProbRec~FD_CC[FD_CC$OM=="BH",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="BH",]$ProbRec~FD_CC[FD_CC$OM=="BH",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="BH",]$ProbRec~FD_CC[FD_CC$OM=="BH",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="BH",]$SSB2115~FD_CC[FD_CC$OM=="BH",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="BH",]$SSB2115~FD_CC[FD_CC$OM=="BH",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="BH",]$SSB2115~FD_CC[FD_CC$OM=="BH",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="BH",]$POF~FD_CC[FD_CC$OM=="BH",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="BH",]$POF~FD_CC[FD_CC$OM=="BH",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="BH",]$POF~FD_CC[FD_CC$OM=="BH",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="BH",]$AAV~FD_CC[FD_CC$OM=="BH",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="BH",]$AAV~FD_CC[FD_CC$OM=="BH",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="BH",]$AAV~FD_CC[FD_CC$OM=="BH",]$FRQ) )
boxplot(FD_CC[FD_CC$OM=="BH",]$F2115~FD_CC[FD_CC$OM=="BH",]$FRQ)
abline(lm(FD_CC[FD_CC$OM=="BH",]$F2115~FD_CC[FD_CC$OM=="BH",]$FRQ)); summary(lm(FD_CC[FD_CC$OM=="BH",]$F2115~FD_CC[FD_CC$OM=="BH",]$FRQ) )



## SSB
SSB=glm(SSB2115~as.factor(FRQ)+Imp+OM, data=FD_C, family=gaussian(link="identity"))
summary(SSB)
par(mfrow=c(2,2))
plot(SSB)

par(mfrow=c(1,1))
plot(predict(SSB, type="response")~FD_C$SSB2115)
abline(a=0,b=1)
boxplot(predict(SSB, type="response")~FD_C$FRQ)
abline(h=1)
boxplot(predict(SSB, type="response")~FD_C$Imp)
abline(h=1)
boxplot(predict(SSB, type="response")~FD_C$OM)
abline(h=1)

SSB_C=glm(SSB2115~FRQ+Imp+OM, data=FD_C, family=gaussian(link="identity"))
summary(SSB_C)
par(mfrow=c(2,2))
plot(SSB_C)

par(mfrow=c(1,1))
plot(predict(SSB_C, type="response")~FD_C$SSB2115)
abline(a=0,b=1)
boxplot(predict(SSB_C, type="response")~FD_C$FRQ)
boxplot(predict(SSB_C, type="response")~FD_C$Imp)
boxplot(predict(SSB_C, type="response")~FD_C$OM)

## AAV?


## F ######### DO SOMETHIGN ABOUT THE MAJOR OUTLIERS IN THIS ONE.
FM=glm(log(F2115+0.01)~as.factor(FRQ)+Imp+OM, data=FD_C, family=gaussian(link="identity"))
summary(FM)
par(mfrow=c(2,2))
plot(FM)

par(mfrow=c(1,1))
plot(predict(FM, type="response")~FD_C$SSB2115)
abline(a=0,b=1)
boxplot(predict(FM, type="response")~FD_C$FRQ)
abline(h=0)
boxplot(predict(FM, type="response")~FD_C$Imp)
abline(h=0)
boxplot(predict(FM, type="response")~FD_C$OM)
abline(h=0)
