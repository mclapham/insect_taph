#Data for figure 6: logistic regressions with complete data set and while holding parameters constant

#logistic regression with complete data set
all_param_glm<-glm(type_part_binom~age_myr+is_beetle+log_length+env_binom, family=binomial,data=occs_env)

#age sequence for calculating predicted values
all_age_prd <- seq(0, 320, 0.1)

##CREATES SYNTHETIC DATA THAT VARIES SMOOTHLY FROM CENOZOIC TO PALEOZOIC VALUES
#FOR BEETLE ABUNDANCE, AVERAGE SIZE, AND PROPORTION OF DEEP LAKES

#calculates occurrence frequency of beetles in Cenozoic and Carboniferous
ceno_col_abund<-length(subset(occs_env$is_beetle,occs_env$is_beetle==1 & occs_env$age_myr<65))/length(subset(occs_env$is_beetle,occs_env$age_myr<65))
carb_col_abund<-length(subset(occs_env$is_beetle,occs_env$is_beetle==1 & occs_env$age_myr>300))/length(subset(occs_env$is_beetle,occs_env$age_myr>300))

#generates data ranging from Cenozoic to Carboniferous average
coleo_model <-seq(ceno_col_abund,carb_col_abund,length.out=3201)

#calculates mean log10 size in Cenozoic and Carboniferous
ceno_size<-mean(subset(occs_env$log_length,occs_env$age_myr<65),na.rm=T)
carb_size<-mean(subset(occs_env$log_length,occs_env$age_myr>300),na.rm=T)

#generates data ranging from Cenozoic to Carboniferous average
size_model<-seq(ceno_size,carb_size,length.out=3201)

#calculates proportion of occurrences from deep lakes in each 10 Myr bin
ceno_dl_abund<-length(subset(occs_env$is_beetle,occs_env$env_binom==1 & occs_env$age_myr<65))/length(subset(occs_env$is_beetle,occs_env$age_myr<65))
carb_dl_abund<-length(subset(occs_env$is_beetle,occs_env$env_binom==1 & occs_env$age_myr>300))/length(subset(occs_env$is_beetle,occs_env$age_myr>300))

#generates data ranging from Cenozoic to Carboniferous average
env_model <-seq(ceno_dl_abund,carb_dl_abund,length.out=3201)

#PREDICTS ARTICULATION BASED ON LOGISTIC REGRESSION AND SYNTHETIC DATA
#small, beetles, non-deep lake
s_b_nd_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(min(size_model),length(all_age_prd)),is_beetle=rep(1,length(all_age_prd)),env_binom=rep(0,length(all_age_prd))),type = "response")

#large, beetles, non-deep lake
l_b_nd_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(max(size_model),length(all_age_prd)),is_beetle=rep(1,length(all_age_prd)),env_binom=rep(0,length(all_age_prd))),type = "response")

#small, non-beetles, non-deep lake
s_nb_nd_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(min(size_model),length(all_age_prd)),is_beetle=rep(0,length(all_age_prd)),env_binom=rep(0,length(all_age_prd))),type = "response")

#large, non-beetles, non-deep lake
l_nb_nd_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(max(size_model),length(all_age_prd)),is_beetle=rep(0,length(all_age_prd)),env_binom=rep(0,length(all_age_prd))),type = "response")

#small, beetles, deep lake
s_b_d_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(min(size_model),length(all_age_prd)),is_beetle=rep(1,length(all_age_prd)),env_binom=rep(1,length(all_age_prd))),type = "response")

#large, beetles, deep lake
l_b_d_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(max(size_model),length(all_age_prd)),is_beetle=rep(1,length(all_age_prd)),env_binom=rep(1,length(all_age_prd))),type = "response")

#small, non-beetles, deep lake
s_nb_d_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(min(size_model),length(all_age_prd)),is_beetle=rep(0,length(all_age_prd)),env_binom=rep(1,length(all_age_prd))),type = "response")

#large, non-beetles, deep lake
l_nb_d_prd<-predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=rep(max(size_model),length(all_age_prd)),is_beetle=rep(0,length(all_age_prd)),env_binom=rep(1,length(all_age_prd))),type = "response")


pdf("fig6.pdf",width=8)
par(mar=c(5,4,2,2))


plot(occs_env$age_myr,jitter(occs_env$type_part_binom,.3), xlim=c(320,-30), yaxt="n", ylab='', xlab='Age (Ma)', main="",col="dark grey")
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"),tick=T)

#all parameter prediction
lines(all_age_prd, predict.glm(all_param_glm, data.frame(age_myr=all_age_prd, log_length=size_model,is_beetle=coleo_model,env_binom=env_model,age_myr=all_age_prd),type = "response"),lwd=3)

lines(all_age_prd,s_b_nd_prd,lty=2,col="gray",lwd=1.5) #small, beetles, non-deep lake
lines(all_age_prd,l_b_nd_prd,lty=2,col="gray",lwd=1.5) #large, beetles, non-deep lake
lines(all_age_prd,s_nb_nd_prd,lty=2,col="gray",lwd=1.5) #small, non-beetles, non-deep lake
lines(all_age_prd, l_nb_nd_prd,lty=2,col="gray",lwd=1.5) #large, non-beetles, non-deep lake
lines(all_age_prd, s_b_d_prd,lty=2,col="gray",lwd=1.5) #small, beetles, deep lake
lines(all_age_prd,l_b_d_prd,lty=2,col="gray",lwd=1.5) #large, beetles, deep lake
lines(all_age_prd,s_nb_d_prd,lty=2,col="gray",lwd=1.5) #small, non-beetles, deep lake
lines(all_age_prd,l_nb_d_prd,lty=2,col="gray",lwd=1.5) #large, non-beetles, deep lake

rect(c(-2,-16,-30),c(-1,-1,-1),c(-16,-30,-44),c(2,2,2))

par(lend=1)

segments(-2,max(s_b_d_prd),-44,lwd=2)
segments(-2,max(s_nb_d_prd),-16,lwd=2)
segments(-30,max(s_nb_d_prd),-44,lwd=2)
segments(-2,max(l_b_d_prd),-30,lwd=2)
segments(-2,max(l_nb_d_prd),-16,lwd=2)

segments(-16,max(s_b_nd_prd),-44,lwd=2)
segments(-30,max(s_nb_nd_prd),-44,lwd=2)
segments(-16,max(l_b_nd_prd),-30,lwd=2)

text(-9,1,"A",cex=1.3)
text(-23,1,"B",cex=1.3)
text(-37,1,"C",cex=1.3)

dev.off()
