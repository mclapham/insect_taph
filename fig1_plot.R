#Script to generate figure 1
#Run data acquisition and filtering (data_acq.R) first!
#Calculates proportion of articulated specimens in each "10 Myr bin"
#plots articulation proportion over time, adding geological timescale
#plot is saved as a pdf (fig1.pdf) in your working directory (e.g., getwd())

#reads time intervals
time_ints<-read.csv("http://paleobiodb.org/data1.1/intervals/list.txt?scale=1")

stages<-subset(time_ints,time_ints$level==5 & time_ints$early_age<340)
periods<-subset(time_ints,time_ints$level==3 & time_ints$early_age<360)

#function to calculate proportion of articulated specimens 
ratio.function<-function(x) length(subset(x,x=="exoskeleton"))/length(x)

#Gives ratios for each 10 Mil Bin
bin_art<-sapply(split(occs_env$type_body_part,occs_env$age_myr),ratio.function)

#Gives specimen counts for each 10 Mil Bin
bin_ct<-sapply(split(occs_env$type_body_part,occs_env$age_myr),length)

pdf("fig1.pdf",width=9)

#CEX is 10*(X/(X+500))
#Plots bubbles for whole database
plot(names(bin_art),bin_art,cex=10*bin_ct/(bin_ct+500),xlab="Age (Ma)",bg="light grey",pch=21,ylab="Proportion articulated",xlim=c(320,10), ylim=c(-0.1,1))

segments(stages$early_age[which(stages$interval_name=="Callovian")],0,y1=1,lty=2)
rect(stages$early_age,-0.07,stages$late_age,0)
rect(periods$early_age,-0.07,periods$late_age,-0.14)

text((periods$early_age+periods$late_age)/2-c(rep(0,7),13),-0.078,strtrim(periods$interval_name,c(0,4,rep(15,5),4)),pos=1)

text((stages$early_age+stages$late_age)/2,-0.008,strtrim(stages$interval_name,floor((stages$early_age-stages$late_age)/4)),pos=1,cex=0.9)

dev.off()
