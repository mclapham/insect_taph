#Run data_acq.R first!
#Logistic regression for six suprafamilial groups, using just age and age+environment
#code to plot fig. 2

#####Logistic regression using age AND environment as the predictors####
odon_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=odon_occs) #odonatoptera

col_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=col_occs) #coleoptera

blatt_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=blatt_occs) #blattodea

orth_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=orth_occs) #orthoptera

hem_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=hem_occs) #hemiptera

dip_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=dip_occs) #diptera

#####Logistic regression using age as the only predictor##### 
odon_glm1<-glm(type_part_binom~age_myr, family=binomial,data=odon_occs) #odonatoptera

col_glm1<-glm(type_part_binom~age_myr, family=binomial,data=col_occs) #coleoptera

blatt_glm1<-glm(type_part_binom~age_myr, family=binomial,data=blatt_occs) #blattodea

orth_glm1<-glm(type_part_binom~age_myr, family=binomial,data=orth_occs) #orthoptera

hem_glm1<-glm(type_part_binom~age_myr, family=binomial,data=hem_occs) #hemiptera

dip_glm1<-glm(type_part_binom~age_myr, family=binomial,data=dip_occs) #diptera

#####PLOTS#####
#dummy variables for glm prediction
age_pred <- seq(0, 320, 0.1)
age_pred_pts<- seq(6,320,10)

#starts plot
pdf("fig2.pdf",width=8)

par(mfrow = c(2,3))
par(mar=c(3,2.8,4,0))
par(oma=c(0,0,0,1))

#Plots Odonatoptera with logistic regression fits
plot(odon_occs$age_myr,jitter(odon_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='', xlab='', main="Odonatoptera",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(age_pred, predict.glm(odon_glm, data.frame(age_myr=age_pred, env_binom=rep(1,length(age_pred))),type = "response"),lty=3)
lines(age_pred, predict.glm(odon_glm, data.frame(age_myr=age_pred, env_binom=rep(0,length(age_pred))),type = "response"), lty="38")
points(age_pred_pts, predict.glm(odon_glm, data.frame(age_myr=age_pred_pts, env_binom=rep(0,length(age_pred_pts))),type = "response"), pch=4, cex=0.6)
lines(age_pred, predict.glm(odon_glm1, data.frame(age_myr=age_pred),type = "response"))
mtext("A",adj=0)

#Plots Blattodea with logistic regression fits
plot(blatt_occs$age_myr,jitter(blatt_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="Blattodea",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_pred, predict.glm(blatt_glm, data.frame(age_myr=age_pred, env_binom=rep(1,length(age_pred))),type = "response"),lty=3)
lines(age_pred, predict.glm(blatt_glm, data.frame(age_myr=age_pred, env_binom=rep(0,length(age_pred))),type = "response"), lty='38')
points(age_pred_pts, predict.glm(blatt_glm, data.frame(age_myr=age_pred_pts, env_binom=rep(0,length(age_pred_pts))),type = "response"), pch=4, cex=0.6)
lines(age_pred, predict.glm(blatt_glm1, data.frame(age_myr=age_pred),type = "response"))
mtext("B",adj=0)

#Plots Orthoptera with logistic regression fits
plot(orth_occs$age_myr,jitter(orth_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab="", main="Orthoptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_pred, predict.glm(orth_glm, data.frame(age_myr=age_pred, env_binom=rep(1,length(age_pred))),type = "response"),lty=3)
lines(age_pred, predict.glm(orth_glm, data.frame(age_myr=age_pred, env_binom=rep(0,length(age_pred))),type = "response"), lty='38')
points(age_pred_pts, predict.glm(orth_glm, data.frame(age_myr=age_pred_pts, env_binom=rep(0,length(age_pred_pts))),type = "response"), pch=4, cex=0.6)
lines(age_pred, predict.glm(orth_glm1, data.frame(age_myr=age_pred),type = "response"))
mtext("C",adj=0)

par(mar=c(5,2.8,2,0))

#Plots Hemiptera with logistic regression fits
plot(hem_occs$age_myr,jitter(hem_occs$type_part_binom,.2),xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)",main="Hemiptera",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(age_pred, predict.glm(hem_glm, data.frame(age_myr=age_pred, env_binom=rep(1,length(age_pred))),type = "response"),lty=3)
lines(age_pred, predict.glm(hem_glm, data.frame(age_myr=age_pred, env_binom=rep(0,length(age_pred))),type = "response"), lty='38')
points(age_pred_pts, predict.glm(hem_glm, data.frame(age_myr=age_pred_pts, env_binom=rep(0,length(age_pred_pts))),type = "response"), pch=4, cex=0.6)
lines(age_pred, predict.glm(hem_glm1, data.frame(age_myr=age_pred),type = "response"))
mtext("D",adj=0)

#Plots Diptera with logistic regression fits
plot(dip_occs$age_myr,jitter(dip_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)", main="Diptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_pred, predict.glm(dip_glm, data.frame(age_myr=age_pred, env_binom=rep(1,length(age_pred))),type = "response"),lty=3)
lines(age_pred, predict.glm(dip_glm, data.frame(age_myr=age_pred, env_binom=rep(0,length(age_pred))),type = "response"), lty='38')
points(age_pred_pts, predict.glm(dip_glm, data.frame(age_myr=age_pred_pts, env_binom=rep(0,length(age_pred_pts))),type = "response"), pch=4, cex=0.6)
lines(age_pred, predict.glm(dip_glm1, data.frame(age_myr=age_pred),type = "response"))
mtext("E",adj=0)

#Plots Coleoptera with logistic regression fits
plot(col_occs$age_myr,jitter(col_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='Age (Ma)', main="Coleoptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_pred, predict.glm(col_glm, data.frame(age_myr=age_pred, env_binom=rep(1,length(age_pred))),type = "response"),lty=3)
lines(age_pred, predict.glm(col_glm, data.frame(age_myr=age_pred, env_binom=rep(0,length(age_pred))),type = "response"), lty="38")
points(age_pred_pts, predict.glm(col_glm, data.frame(age_myr=age_pred_pts, env_binom=rep(0,length(age_pred_pts))),type = "response"), pch=4, cex=0.6)
lines(age_pred, predict.glm(col_glm1, data.frame(age_myr=age_pred),type = "response"), lty=1)
mtext("F",adj=0)

dev.off()
