#Run data_acq.R first!

#CALCULATES LOGISTIC REGRESSION FITS FOR ALL-INSECT DATASET
#three parameter model
all_param_glm<-glm(type_part_binom~log_length+env_binom+is_beetle,family="binomial",data=occs_env)
#two parameter models
size_env_glm<-glm(type_part_binom~log_length+env_binom,family="binomial",data=occs_env) #size and environment
morph_env_glm<-glm(type_part_binom~is_beetle+env_binom,family="binomial",data=occs_env) #morphology and environment
morph_size_glm<-glm(type_part_binom~is_beetle+log_length,family="binomial",data=occs_env) #size and morphology
#single parameter models
env_glm<-glm(type_part_binom~env_binom,family="binomial",data=occs_env) #environment
size_glm<-glm(type_part_binom~log_length,family="binomial",data=occs_env) #size
morph_glm<-glm(type_part_binom~is_beetle,family="binomial",data=occs_env) #morphology

#AIC values for each logistic regression model
aic_values<-rbind("all"=summary(all_param_glm)$aic,"size+env"=summary(size_env_glm)$aic,"morph+env"=summary(morph_env_glm)$aic,"morph+size"=summary(morph_size_glm)$aic,
           "env"=summary(env_glm)$aic,"size"=summary(size_glm)$aic,"morph"=summary(morph_glm)$aic)

#orders AIC values from smallest to largest
aic_values<-aic_values[order(aic_values),]

#finds difference between each model's AIC value and smallest AIC value
delta_aic<-aic_values-aic_values[1]

#Table 1: model comparison using AIC values
data.frame(aic_values,delta_aic)

#CALCULATES LOGISTIC REGRESSION FOR SIX SUPRAFAMILIAL GROUPS
odon_glm<-glm(type_part_binom~log_length+env_binom,family="binomial",data=odon_occs) #Odonatoptera
blatt_glm<-glm(type_part_binom~log_length+env_binom,family="binomial",data=blatt_occs) #Blattodea
orth_glm<-glm(type_part_binom~log_length+env_binom,family="binomial",data=orth_occs) #Orthoptera
hem_glm<-glm(type_part_binom~log_length+env_binom,family="binomial",data=hem_occs) #Hemiptera
dip_glm<-glm(type_part_binom~log_length+env_binom,family="binomial",data=dip_occs) #Diptera
coleo_glm<-glm(type_part_binom~log_length+env_binom,family="binomial",data=col_occs) #Coleoptera

#function to present odds ratios, confidence intervals, and p-values for size, environment and morphology
glm.summary<-function(glm_res) {
  size_odds<-round(exp(summary(glm_res)$coefficients[2]),3) #odds ratio: size (log_length)
  size_low95<-round(exp(summary(glm_res)$coefficients[2]-1.96*summary(glm_res)$coefficients[2,2]),3) #lower 95% CI
  size_upp95<-round(exp(summary(glm_res)$coefficients[2]+1.96*summary(glm_res)$coefficients[2,2]),3) #upper 95% CI
  if(summary(glm_res)$coefficients[2,4]>0.001) {size_p<-round(summary(glm_res)$coefficients[2,4],3)} else {size_p<-"<0.001"}
  
  env_odds<-round(exp(summary(glm_res)$coefficients[3]),3) #odds ratio: environment
  env_low95<-round(exp(summary(glm_res)$coefficients[3]-1.96*summary(glm_res)$coefficients[3,2]),3) #lower 95% CI
  env_upp95<-round(exp(summary(glm_res)$coefficients[3]+1.96*summary(glm_res)$coefficients[3,2]),3) #upper 95% CI
  if(summary(glm_res)$coefficients[3,4]>0.001) {env_p<-round(summary(glm_res)$coefficients[3,4],3)} else {env_p<-"<0.001"}
  
  if(nrow(summary(glm_res)$coefficients)>3) {
    morph_odds<-round(exp(summary(glm_res)$coefficients[4]),3) #odds ratio: morphology
    morph_low95<-round(exp(summary(glm_res)$coefficients[4]-1.96*summary(glm_res)$coefficients[4,2]),3) #lower 95% CI
    morph_upp95<-round(exp(summary(glm_res)$coefficients[4]+1.96*summary(glm_res)$coefficients[4,2]),3) #upper 95% CI
    if(summary(glm_res)$coefficients[4,4]>0.001) {morph_p<-round(summary(glm_res)$coefficients[4,4],3)} else {morph_p<-"<0.001"}
    
    c(size_odds,size_low95,size_upp95,size_p,env_odds,env_low95,env_upp95,env_p,morph_odds,morph_low95,morph_upp95,morph_p)
  } else {
    c(size_odds,size_low95,size_upp95,size_p,env_odds,env_low95,env_upp95,env_p,NA,NA,NA,NA)
  }
}

#combines glm results into table (Table 2)
glm_results<-t(data.frame(glm.summary(all_param_glm),glm.summary(odon_glm),glm.summary(blatt_glm),glm.summary(orth_glm),glm.summary(hem_glm),glm.summary(dip_glm),glm.summary(coleo_glm)))
rownames(glm_results)<-c("Insecta","Odonatoptera","Blattodea","Orthoptera","Hemiptera","Diptera","Coleoptera")
colnames(glm_results)<-c("size odds-ratio","size 2.5% CI","size 97.5% CI","size p-value","env odds-ratio","env 2.5% CI","env 97.5% CI","env p-value","morph odds-ratio","morph 2.5% CI","morph 97.5% CI","morph p-value")
