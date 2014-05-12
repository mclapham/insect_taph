#Run data_acq.R first!
#Logistic regression for beetles, including all and only species named post-1950
#separate regression models for age and age+environment
#code to plot fig. 8

#extracts occurrences of post-1950 Coleoptera
post_1950_col<-subset(col_occs,col_occs$pubyr>1950)

#####Logistic regression using age+environment as the predictors####
col_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=col_occs) #all coleoptera

p1950_col_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=post_1950_col) #post-1950 coleoptera

#####Logistic regression using just age as the predictors####
col_glm1<-glm(type_part_binom~age_myr, family=binomial,data=col_occs) #all coleoptera

p1950_col_glm1<-glm(type_part_binom~age_myr, family=binomial,data=post_1950_col) #post-1950 coleoptera

#####PLOTS#####
#dummy variables for prediction
age_prd <- seq(0, 320, 0.1)
age_prd_pts<- seq(6,320,10)

pdf("fig8.pdf",width=5,height=8)

par(mfrow = c(2,1))
par(mar=c(3,2.8,3,0))
par(oma=c(0,0,0,1))

#Plots All Coleoptera with logistic regression fits
plot(col_occs$age_myr,jitter(col_occs$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='',xlab='', main="All Coleoptera",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(age_prd, predict.glm(col_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(col_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty="38")
points(age_prd_pts, predict.glm(col_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(col_glm1, data.frame(age_myr=age_prd),type = "response"), lty=1)
mtext("A",adj=0)

par(mar=c(4,2.8,2,0))

#Plots Post-1950 Coleoptera with logistic regression fits
plot(post_1950_col$age_myr,jitter(post_1950_col$type_part_binom,.2),xlim=c(320,0), yaxt="n", ylab='',xlab="Age (Ma)",main="Post-1950 Coleoptera",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(age_prd, predict.glm(p1950_col_glm, data.frame(age_myr=age_prd, env_binom=rep(1,length(age_prd))),type = "response"),lty=3)
lines(age_prd, predict.glm(p1950_col_glm, data.frame(age_myr=age_prd, env_binom=rep(0,length(age_prd))),type = "response"), lty='38')
points(age_prd_pts, predict.glm(p1950_col_glm, data.frame(age_myr=age_prd_pts, env_binom=rep(0,length(age_prd_pts))),type = "response"), pch=4, cex=0.6)
lines(age_prd, predict.glm(p1950_col_glm1, data.frame(age_myr=age_prd),type = "response"))
mtext("B",adj=0)

dev.off()
