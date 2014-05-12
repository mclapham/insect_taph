#DATA ACQUISITION AND FILTERING
#loads data files and filters occurrences based on criteria described in paper
#filtered output is in data frame occs_env and six data frames for suprafamilial groups:
#col_occs (Coleoptera), odon_occs (Odonatoptera), blatt_occs (Blattodea excluding termites),
#orth_occs (Orthoptera), hem_occs (Hemiptera), dip_occs (Diptera)

#Reads insect occurrences
occs_raw<-read.csv("https://github.com/mclapham/insect_taph/blob/master/data_files/insect_occs.csv?raw=true")

#Filters occurrences assigned to 10 Myr bin
occs_time<-subset(occs_raw,occs_raw$X10_my_bin!="")

#Filters occurrences where holotype has body part and is not a nymph/larva
occs_body<-subset(occs_time,occs_time$type_body_part %in% c("wing","forewing","hindwing","tegmen","elytra","exoskeleton"))

#Filters holotype occurrences only
occs_holo<-subset(occs_body,occs_body$occurrence.species_reso=="n. sp.")

#Reads size data file
sizes<-read.csv("https://github.com/mclapham/insect_taph/blob/master/data_files/insect_size.csv?raw=true")

#Filters occurrences with measured wing element
occs_size<-subset(occs_holo,occs_holo$occurrence_no %in% sizes$occurrence_no)

#Removes width measurements
sizes<-subset(sizes,sizes$measurement_type=="length")

#Adds sizes to occurrences
for (i in 1:nrow(occs_size)) {
  occs_size$length[i]<-mean(subset(sizes$average,sizes$is_type=="holotype" & sizes$occurrence_no==occs_size$occurrence_no[i]))
}

#Adds log10 sizes to occurrences
occs_size$log_length<-log10(occs_size$length)

#Reads environment matching file
env_match<-read.csv("https://github.com/mclapham/insect_taph/blob/master/data_files/env_match.csv?raw=true")

#Extracts occurrences with good environments
occs_env<-subset(occs_size,occs_size$environment %in% env_match$specific_env)

#Matches specific PBDB environment with broader categories
occs_env$broad_env<-apply(occs_env,1,function(x) env_match$broad_env[which(env_match$specific_env==x[17])])

#Age midpoints of 10 Myr bins
bin_midpts<-c(327.1,312.5,302.9,60.6, 48.1, 37.1, 28.5,17.3,5.8, 140.9, 130.9, 118.7,105.8, 96.5, 88.5, 77,68, 195.6, 186.3, 177.3, 168.1, 157.8, 148.2, 294.5, 281.3, 265.8, 255.7, 249.8, 241.1, 228.2, 211.6)

names(bin_midpts)<-c("Carboniferous 3","Carboniferous 4","Carboniferous 5","Cenozoic 1","Cenozoic 2","Cenozoic 3","Cenozoic 4","Cenozoic 5","Cenozoic 6","Cretaceous 1","Cretaceous 2","Cretaceous 3","Cretaceous 4","Cretaceous 5","Cretaceous 6","Cretaceous 7","Cretaceous 8","Jurassic 1","Jurassic 2","Jurassic 3","Jurassic 4","Jurassic 5","Jurassic 6","Permian 1","Permian 2","Permian 3","Permian 4","Triassic 1","Triassic 2","Triassic 3","Triassic 4")

#Adds age in Myr to each occurrence
occs_env$age_myr<-apply(occs_env,1,function(x) bin_midpts[which(names(bin_midpts)==x[16])])

#Reads occurrences of Coleoptera, Odonatoptera, Blattodea (excluding termites), Orthoptera, Hemiptera, Diptera
coleop<-read.csv("http://www.paleobiodb.org/data1.1/occs/list.txt?base_name=Coleoptera&limit=9999999")
odonat<-read.csv("http://www.paleobiodb.org/data1.1/occs/list.txt?base_name=Odonatoptera&limit=9999999")
blatt<-read.csv("http://www.paleobiodb.org/data1.1/occs/list.txt?base_name=Blattodea&exclude_id=277547&limit=9999999")
orthop<-read.csv("http://www.paleobiodb.org/data1.1/occs/list.txt?base_name=Orthoptera&limit=9999999")
hemip<-read.csv("http://www.paleobiodb.org/data1.1/occs/list.txt?base_name=Hemiptera&limit=9999999")
dipt<-read.csv("http://www.paleobiodb.org/data1.1/occs/list.txt?base_name=Diptera&limit=9999999")

#Adds column to indicate whether occurrence is beetle or not
occs_env$is_beetle<-ifelse(occs_env$occurrence_no %in% coleop$occurrence_no,1,0)

#Adds binomial column for type body part (1=articulated exoskeleton, 0=disarticulated wing element)
occs_env$type_part_binom<-ifelse(occs_env$type_body_part=="exoskeleton",1,0)

#Adds binomial column for environment (1=deep lake, 0=non-deep lake)
occs_env$env_binom<-ifelse(occs_env$broad_env=="deep lake",1,0)


#read valid insect species names
ins_names<-read.csv("http://paleobiodb.org/data1.1/taxa/list.txt?name=Insecta&rel=all_children&status=senior&rank=species&show=attr&limit=999999")

#extracts publication years
ins_names$pubyr<-gsub("[^0-9]","",ins_names$attribution)

#add publication years to occurrences
occs_env$pubyr<-apply(occs_env,1,function(x) ins_names$pubyr[which(ins_names$taxon_name==paste(x[5],x[7]))])

#Creates data files for suprafamilial groups
col_occs<-subset(occs_env,occs_env$occurrence_no %in% coleop$occurrence_no)
odon_occs<-subset(occs_env,occs_env$occurrence_no %in% odonat$occurrence_no)
blatt_occs<-subset(occs_env,occs_env$occurrence_no %in% blatt$occurrence_no)
orth_occs<-subset(occs_env,occs_env$occurrence_no %in% orthop$occurrence_no)
hem_occs<-subset(occs_env,occs_env$occurrence_no %in% hemip$occurrence_no)
dip_occs<-subset(occs_env,occs_env$occurrence_no %in% dipt$occurrence_no)
