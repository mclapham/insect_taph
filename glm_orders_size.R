#Run data_acq.R first1
#Logistic regression for six suprafamilial groups, using size as the predictor
#code to plot fig. 5

#####Logistic regression using size as the only predictor##### 
odon_size_glm1<-glm(type_part_binom~log_length, family=binomial,data=odon_occs) #odonatoptera

col_size_glm1<-glm(type_part_binom~log_length, family=binomial,data=col_occs) #coleoptera

blatt_size_glm1<-glm(type_part_binom~log_length, family=binomial,data=blatt_occs) #blattodea

orth_size_glm1<-glm(type_part_binom~log_length, family=binomial,data=orth_occs) #orthoptera

hem_size_glm1<-glm(type_part_binom~log_length, family=binomial,data=hem_occs) #hemiptera

dip_size_glm1<-glm(type_part_binom~log_length, family=binomial,data=dip_occs) #diptera

#####Logistic regression using size and environment as the only predictor##### 
odon_size_glm<-glm(type_part_binom~log_length+env_binom, family=binomial,data=odon_occs) #odonatoptera

col_size_glm<-glm(type_part_binom~log_length+env_binom, family=binomial,data=col_occs) #coleoptera

blatt_size_glm<-glm(type_part_binom~log_length+env_binom, family=binomial,data=blatt_occs) #blattodea

orth_size_glm<-glm(type_part_binom~log_length+env_binom, family=binomial,data=orth_occs) #orthoptera

hem_size_glm<-glm(type_part_binom~log_length+env_binom, family=binomial,data=hem_occs) #hemiptera

dip_size_glm<-glm(type_part_binom~log_length+env_binom, family=binomial,data=dip_occs) #diptera

#####PLOTS#####
#dummy variable for prediction
size_prd <- seq(-0.5,2.5, 0.05)
size_prd_pts<-seq(-0.5,2.5, 0.1)

pdf("fig5.pdf",width=8)

par(mfrow = c(2, 3))
par(mar=c(3,2.8,4,0))
par(oma=c(0,0,0,1))

#Plots Odonatoptera with logistic regression fits
plot(odon_occs$log_length,jitter(odon_occs$type_part_binom,.2),yaxt="n", ylab='', xlab='',main="Odonatoptera",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(size_prd, predict.glm(odon_size_glm, data.frame(log_length=size_prd, env_binom=rep(1,length(size_prd))),type = "response"),lty=3)
lines(size_prd, predict.glm(odon_size_glm, data.frame(log_length=size_prd, env_binom=rep(0,length(size_prd))),type = "response"), lty="38")
points(size_prd_pts, predict.glm(odon_size_glm, data.frame(log_length=size_prd_pts, env_binom=rep(0,length(size_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(size_prd, predict.glm(odon_size_glm1, data.frame(log_length=size_prd),type = "response"))
mtext("A",adj=0)

#Plots Blattodea with logistic regression fits
plot(blatt_occs$log_length,jitter(blatt_occs$type_part_binom,.2), yaxt="n", ylab='',xlab='', main="Blattodea",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(size_prd, predict.glm(blatt_size_glm, data.frame(log_length=size_prd, env_binom=rep(1,length(size_prd))),type = "response"),lty=3)
lines(size_prd, predict.glm(blatt_size_glm, data.frame(log_length=size_prd, env_binom=rep(0,length(size_prd))),type = "response"), lty='38')
points(size_prd_pts, predict.glm(blatt_size_glm, data.frame(log_length=size_prd_pts, env_binom=rep(0,length(size_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(size_prd, predict.glm(blatt_size_glm1, data.frame(log_length=size_prd),type = "response"))
mtext("B",adj=0)

#Plots Orthoptera with logistic regression fits
plot(orth_occs$log_length,jitter(orth_occs$type_part_binom,.2), yaxt="n", ylab='',xlab="", main="Orthoptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(size_prd, predict.glm(orth_size_glm, data.frame(log_length=size_prd, env_binom=rep(1,length(size_prd))),type = "response"),lty=3)
lines(size_prd, predict.glm(orth_size_glm, data.frame(log_length=size_prd, env_binom=rep(0,length(size_prd))),type = "response"), lty='38')
points(size_prd_pts, predict.glm(orth_size_glm, data.frame(log_length=size_prd_pts, env_binom=rep(0,length(size_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(size_prd, predict.glm(orth_size_glm1, data.frame(log_length=size_prd),type = "response"))
mtext("C",adj=0)

par(mar=c(5,2.8,2,0))

#Plots Hemiptera with logistic regression fits
plot(hem_occs$log_length,jitter(hem_occs$type_part_binom,.2), yaxt="n", ylab='',xlab=expression("Wing element length (log"[10]*" mm)"),main="Hemiptera",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(size_prd, predict.glm(hem_size_glm, data.frame(log_length=size_prd, env_binom=rep(1,length(size_prd))),type = "response"),lty=3)
lines(size_prd, predict.glm(hem_size_glm, data.frame(log_length=size_prd, env_binom=rep(0,length(size_prd))),type = "response"), lty='38')
points(size_prd_pts, predict.glm(hem_size_glm, data.frame(log_length=size_prd_pts, env_binom=rep(0,length(size_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(size_prd, predict.glm(hem_size_glm1, data.frame(log_length=size_prd),type = "response"))
mtext("D",adj=0)

#Plots Diptera with logistic regression fits
plot(dip_occs$log_length,jitter(dip_occs$type_part_binom,.2), yaxt="n", ylab='',xlab=expression("Wing element length (log"[10]*" mm)"), main="Diptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(size_prd, predict.glm(dip_size_glm, data.frame(log_length=size_prd, env_binom=rep(1,length(size_prd))),type = "response"),lty=3)
lines(size_prd, predict.glm(dip_size_glm, data.frame(log_length=size_prd, env_binom=rep(0,length(size_prd))),type = "response"), lty='38')
points(size_prd_pts, predict.glm(dip_size_glm, data.frame(log_length=size_prd_pts, env_binom=rep(0,length(size_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(size_prd, predict.glm(dip_size_glm1, data.frame(log_length=size_prd),type = "response"))
mtext("E",adj=0)

#Plots Coleoptera with logistic regression fits
plot(col_occs$log_length,jitter(col_occs$type_part_binom,.2),yaxt="n", ylab='',xlab=expression("Wing element length (log"[10]*" mm)"), main="Coleoptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(size_prd, predict.glm(col_size_glm, data.frame(log_length=size_prd, env_binom=rep(1,length(size_prd))),type = "response"),lty=3)
lines(size_prd, predict.glm(col_size_glm, data.frame(log_length=size_prd, env_binom=rep(0,length(size_prd))),type = "response"), lty="38")
points(size_prd_pts, predict.glm(col_size_glm, data.frame(log_length=size_prd_pts, env_binom=rep(0,length(size_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(size_prd, predict.glm(col_size_glm1, data.frame(log_length=size_prd),type = "response"))
mtext("F",adj=0)

dev.off()
