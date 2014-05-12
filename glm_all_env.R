#Run data_acq.R first!
#Logistic regression for all insects, using just age and age+environment
#code to plot fig. 3

#Logistic regression using age AND environment as the predictors for all insects
ins_glm<-glm(type_part_binom~age_myr+env_binom, family=binomial,data=occs_env) 

#Logistic regression using age as the only predictor for all insects 
ins_glm1<-glm(type_part_binom~age_myr, family=binomial,data=occs_env)


#####PLOTS#####
#dummy variables for glm prediction
age_pred <- seq(0, 320, 0.1)
age_pred_pts<- seq(6,320,10)

#starts plot
pdf("fig3.pdf",width=7,height=5)

par(mar=c(5,3,1,5))

#Plots all insects with logistic regression fits
plot(occs_env$age_myr,jitter(occs_env$type_part_binom,.2), xlim=c(320,0), yaxt="n", ylab='', xlab="Age (Ma)",main="",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
axis(4,at=seq(0,1,0.2))
mtext("Modeled articulation proportion",side=4,line=2.5)
lines(age_pred, predict.glm(ins_glm, data.frame(age_myr=age_pred, env_binom=rep(1,length(age_pred))),type = "response"),lty=3)
lines(age_pred, predict.glm(ins_glm, data.frame(age_myr=age_pred, env_binom=rep(0,length(age_pred))),type = "response"), lty="38")
points(age_pred_pts, predict.glm(ins_glm, data.frame(age_myr=age_pred_pts, env_binom=rep(0,length(age_pred_pts))),type = "response"), pch=4, cex=0.6)
lines(age_pred, predict.glm(ins_glm1, data.frame(age_myr=age_pred),type = "response"))

dev.off()
