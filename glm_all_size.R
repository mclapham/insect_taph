#Run data_acq.R first!
#Logistic regression for all insects, using size as the predictor
#code to plot fig. 4

#Logistic regression using size as the predictors for all insects
size_glm<-glm(type_part_binom~log_length, family=binomial,data=occs_env) 

#####PLOTS#####
#dummy variable for prediction
size.prd <- seq(-0.5,2.5, 0.05)

pdf("fig4.pdf",width=7,height=5)

par(mar=c(5,3,1,5))

#Plots all insects with logistic regression fits
plot(occs_env$log_length,jitter(occs_env$type_part_binom,.2),yaxt="n", ylab='', xlab=expression("Wing element length (log"[10]*" mm)"),main="",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
axis(4,at=seq(0,1,0.2))
mtext("Modeled articulation proportion",side=4,line=2.5)
lines(size.prd, predict.glm(size_glm, data.frame(log_length=size.prd),type = "response"))

dev.off()
