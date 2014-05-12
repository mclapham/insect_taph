#Run data_acq.R first!
#Logistic regression to investigate effect of publication year on articulation
#in all insects and in beetles
#code to plot fig. 9

#####Logistic regression using publication year as the predictors####
all_yr_glm<-glm(type_part_binom~pubyr, family=binomial,data=occs_env) #all taxa

col_yr_glm<-glm(type_part_binom~pubyr, family=binomial,data=col_occs) #coleoptera

#####PLOTS#####
pubyr_prd <- seq(1820,2015,1)

pdf("fig9.pdf",width=7,height=5)

par(mar=c(5,3,1,5))

#Plots all insects with logistic regression fits
plot(occs_env$pubyr,jitter(occs_env$type_part_binom,.2),yaxt="n", ylab='', xlab="Age (Ma)",main="",col='dark grey')
axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
axis(4,at=seq(0,1,0.2))
mtext("Modeled articulation proportion",side=4,line=2.5)
lines(pubyr_prd, predict.glm(all_yr_glm, data.frame(pubyr=pubyr_prd),type = "response"),lwd=2)
lines(pubyr_prd, predict.glm(col_yr_glm, data.frame(pubyr=pubyr_prd),type = "response"),lty=2,lwd=2)
legend(1970,0.25,c("Coleoptera","Insecta"),lty=c(2,1),lwd=2,cex=0.8)

dev.off()
