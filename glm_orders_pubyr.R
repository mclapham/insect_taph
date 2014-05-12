#Run data_acq.R first!
#Logistic regression for the six suprafamilial groups, comparing all and only post-1950
#code to plot supplementary figure

#extracts occurrences of post-1950 Odonatoptera
post_1950_odon<-subset(odon_occs,odon_occs$pubyr>1950)

#extracts occurrences of post-1950 Blattodea
post_1950_blatt<-subset(blatt_occs,blatt_occs$pubyr>1950)

#extracts occurrences of post-1950 Orthoptera
post_1950_orth<-subset(orth_occs,orth_occs$pubyr>1950)

#extracts occurrences of post-1950 Hemiptera
post_1950_hem<-subset(hem_occs,hem_occs$pubyr>1950)

#extracts occurrences of post-1950 Diptera
post_1950_dip<-subset(dip_occs,dip_occs$pubyr>1950)

#extracts occurrences of post-1950 Coleoptera
post_1950_col<-subset(col_occs,col_occs$pubyr>1950)


#####Logistic regression using age AND environment as the predictors, for all and post-1950 taxa####
odon_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=odon_occs) #all Odonatoptera
p1950_odon_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=post_1950_odon) #post-1950 Odonatoptera

blatt_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=blatt_occs) #all Blattodea
p1950_blatt_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=post_1950_blatt) #post-1950 Blattodea

orth_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=orth_occs) #all Orthoptera
p1950_orth_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=post_1950_orth) #post-1950 Orthoptera

hem_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=hem_occs) #all Hemiptera
p1950_hem_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=post_1950_hem) #post-1950 Hemiptera

dip_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=dip_occs) #all Diptera
p1950_dip_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=post_1950_dip) #post-1950 Diptera

col_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=col_occs) #all coleoptera
p1950_col_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=post_1950_col) #post-1950 coleoptera

#####Logistic regression using just age as the predictors, for all and post-1950 taxa####
odon_glm1<-glm(type_part_binom~age_myr, family=binomial,data=odon_occs) #all Odonatoptera
p1950_odon_glm1<-glm(type_part_binom~age_myr, family=binomial,data=post_1950_odon) #post-1950 Odonatoptera

blatt_glm1<-glm(type_part_binom~age_myr, family=binomial,data=blatt_occs) #all Blattodea
p1950_blatt_glm1<-glm(type_part_binom~age_myr, family=binomial,data=post_1950_blatt) #post-1950 Blattodea

orth_glm1<-glm(type_part_binom~age_myr, family=binomial,data=orth_occs) #all Orthoptera
p1950_orth_glm1<-glm(type_part_binom~age_myr, family=binomial,data=post_1950_orth) #post-1950 Orthoptera

hem_glm1<-glm(type_part_binom~age_myr, family=binomial,data=hem_occs) #all Hemiptera
p1950_hem_glm1<-glm(type_part_binom~age_myr, family=binomial,data=post_1950_hem) #post-1950 Hemiptera

dip_glm1<-glm(type_part_binom~age_myr, family=binomial,data=dip_occs) #all Diptera
p1950_dip_glm1<-glm(type_part_binom~age_myr, family=binomial,data=post_1950_dip) #post-1950 Diptera

col_glm1<-glm(type_part_binom~age_myr, family=binomial,data=col_occs) #all coleoptera
p1950_col_glm1<-glm(type_part_binom~age_myr, family=binomial,data=post_1950_col) #post-1950 coleoptera


#####PLOTS#####
age_prd <- seq(0, 320, 0.1)
age_prd_pts<- seq(6,320,10)

pdf("supp_fig1.pdf",width=11,height=7)

par(mfrow = c(2,6))
par(mar=c(3,2.8,3,0))
par(oma=c(0,0,0,1))

#Plots All Odonatoptera with logistic regression fits
plot(odon_occs$age_myr,jitter(odon_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="All Odonatoptera",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(age_prd, predict.glm(odon_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(odon_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(odon_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(odon_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("A",adj=0)

#Plots All Blattodea with logistic regression fits
plot(blatt_occs$age_myr,jitter(blatt_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="All Blattodea",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(blatt_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(blatt_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(blatt_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(blatt_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("B",adj=0)

#Plots All Orthoptera with logistic regression fits
plot(orth_occs$age_myr,jitter(orth_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="All Orthoptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(orth_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(orth_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(orth_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(orth_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("C",adj=0)

#Plots All Hemiptera with logistic regression fits
plot(hem_occs$age_myr,jitter(hem_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="All Hemiptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(hem_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(hem_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(hem_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(hem_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("D",adj=0)


#Plots All Diptera with logistic regression fits
plot(dip_occs$age_myr,jitter(dip_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="All Diptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(dip_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(dip_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(dip_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(dip_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("E",adj=0)

#Plots All Coleoptera with logistic regression fits
plot(col_occs$age_myr,jitter(col_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="All Coleoptera",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(col_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(col_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(col_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(col_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("F",adj=0)

par(mar=c(5,2.8,2,0))

#Plots post-1950 Odonatoptera with logistic regression fits
plot(post_1950_odon$age_myr,jitter(post_1950_odon$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)", main="Post-1950 Odon.",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(age_prd, predict.glm(p1950_odon_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(p1950_odon_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(p1950_odon_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(p1950_odon_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("G",adj=0)

#Plots post-1950 Blattodea with logistic regression fits
plot(post_1950_blatt$age_myr,jitter(post_1950_blatt$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)", main="Post-1950 Blatt.",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(p1950_blatt_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(p1950_blatt_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(p1950_blatt_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(p1950_blatt_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("H",adj=0)

#Plots post-1950 Orthoptera with logistic regression fits
plot(post_1950_orth$age_myr,jitter(post_1950_orth$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)", main="Post-1950 Orth.",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(p1950_orth_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(p1950_orth_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(p1950_orth_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(p1950_orth_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("I",adj=0)

#Plots post-1950 Hemiptera with logistic regression fits
plot(post_1950_hem$age_myr,jitter(post_1950_hem$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)", main="Post-1950 Hemip.",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(p1950_hem_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(p1950_hem_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(p1950_hem_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(p1950_hem_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("J",adj=0)

#Plots post-1950 Diptera with logistic regression fits
plot(post_1950_dip$age_myr,jitter(post_1950_dip$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)", main="Post-1950 Dip.",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(p1950_dip_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(p1950_dip_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(p1950_dip_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(p1950_dip_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("K",adj=0)

#Plots Post-1950 Coleoptera with logistic regression fits
plot(post_1950_col$age_myr,jitter(post_1950_col$type_part_binom,.2),xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)",main="Post-1950 Coleo.",col='dark grey')
axis(2, at=c(0,1),label=F, tick=T)
lines(age_prd, predict.glm(p1950_col_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(p1950_col_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty='38')
points(age_prd_pts, predict.glm(p1950_col_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(p1950_col_glm1, data.frame(age_myr=age_prd),type = "response"))
mtext("L",adj=0)

dev.off()
