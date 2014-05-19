#Run data_acq.R first!

#Function to calculate proportion of occurrences per paleoenvironment group
env.count<-function(occs) {
  round(sapply(split(occs,occs$broad_env),nrow)/nrow(occs),2)[c(1,5,3,2,4)]
}

#Produces table 3 with proportional counts of occurrences per paleoenvironment group for six major clades
rbind(Odonatoptera=env.count(odon_occs),Blattodea=env.count(blatt_occs),Orthoptera=env.count(orth_occs),Hemiptera=env.count(hem_occs),Diptera=env.count(dip_occs),Coleoptera=env.count(col_occs))
