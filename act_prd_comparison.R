#Run data_acq.R first!
#Comparing actual articulation with values predicted from beetles, size, and lakes
#code to plot figure 7

#logistic regression with complete data set
all_param_glm<-glm(type_part_binom~age_myr+is_beetle+log_length+env_binom, family=binomial,data=occs_env)

#calculates occurrence frequency of beetles in each 10 Myr bin
coleo_abund<-sapply(split(occs_env$is_beetle,occs_env$age_myr),mean)

#calculates mean log10 size in each 10 Myr bin
size_trend<-sapply(split(occs_env$log_length,occs_env$age_myr),function(x) mean(x,na.rm=T))

#calculates proportion of occurrences from deep lakes in each 10 Myr bin
env_count<-sapply(split(occs_env$env_binom,occs_env$age_myr),mean)

#Age sequence, based on midpoint of actual 10 Myr bins, for prediction
age_prd<-as.numeric(names(coleo_abund))

#Predicted articulation based on actual beetle, size, and deep lake value for each 10 Myr bin
predicted_art<-predict.glm(all_param_glm, data.frame(age_myr=age_prd,log_length=size_trend,is_beetle=coleo_abund,env_binom=env_count),type = "response")

#function to calculate proportion of articulated specimens 
ratio.function<-function(x) length(subset(x,x=="exoskeleton"))/length(x)

#Calcuates articulation ratios for each 10 Mil Bin
bin_art<-sapply(split(occs_env$type_body_part,occs_env$age_myr),ratio.function)

#Calculates specimen counts for each 10 Mil Bin
bin_ct<-sapply(split(occs_env$type_body_part,occs_env$age_myr),length)

#Combines actual articulation, specimen cts, and predicted articulation
ct_data<-data.frame(bin_art,bin_ct,predicted_art)
ct_data<-subset(ct_data,ct_data$bin_ct>100) #removes bins with <100 specimens


pdf("fig7.pdf",width=8)

plot(rownames(ct_data),ct_data$bin_art,type="n",xlab="Age (Ma)",ylab="Proportion articulated",xlim=c(320,0),ylim=c(0,1))

#Finds 10Myr bin boundaries for plotting rectangles
box_edge<-rowMeans(cbind(as.numeric(names(bin_ct))[1:length(names(bin_ct))-1],as.numeric(names(bin_ct))[2:length(names(bin_ct))]))
rect(box_edge[1:length(box_edge)-1],0,box_edge[2:length(box_edge)],1,col=rep(c("light gray",NA),15),border=NA)

points(rownames(ct_data),ct_data$bin_art,cex=2.5)

points(rownames(ct_data),ct_data$predicted_art, cex=1.5,pch=16)

legend(320,1,c("Actual","Modeled"),pch=c(1,16),cex=1.25,bg="white")

dev.off()

