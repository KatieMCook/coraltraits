###CORAL TRAITS EXTRACTION ####
library(dplyr)

setwd("S:/Beger group/Katie Cook/Japan_data/coraltraits")

#read in trait name and code data
traits_list<-read.csv('traits.csv')

#what traits do I want?
#e.g age at maturtiy (id 47), mode of larval development (id 5), 
#life his strat  (id 233), depth lower (92), growth form (206)


# make a df 
traits<- data.frame(traits=c('age_mat', 'larval_dev', 'life_his','depth_l','growth_form'),
                    id=c(47,5,233,92,206))
traits$traits<-as.character(traits$traits)

#make the https strings for extraction

strings<-data.frame(trait='trait', string='string')
strings$string<-as.character(strings$string)
strings$trait<-as.character(strings$trait)


for (i in 1:nrow(traits)){
 strings[i,1]<-traits[i,1]
 strings[i,2]<-paste0("https://coraltraits.org/traits/", traits[i,2],".csv")  
}

#extract using these traits

for (i in 1:nrow(traits)){
  name<- read.csv(strings[i,2], as.is=TRUE)
  assign(paste0("data_", traits[i,1]), name)
}


## OR DOWNLOAD USING SPECIES----
# extract species names from results- can't do this yet- put into a vector

#load in coral traits species list for ID 
species_list<-read.csv('species.csv')

species$master_species<-as.character(species$master_species)

#dummy data
species<- read.csv('survey_species.csv')
species<-species[,2]
species<-as.character(species)

#filter species by species list 
species_filter<- species_list[which(species_list$master_species %in% species),]

species<-species_filter[,c(1,2)]




#make the https strings for extraction
strings_sp<-data.frame(species='species', string='string')
strings_sp$string<-as.character(strings_sp$string)
strings_sp$species<-as.character(strings_sp$species)

for (i in 1:nrow(species)){
  strings_sp[i,1]<-species[i,2]
  strings_sp[i,2]<-paste0("https://coraltraits.org/species/", species[i,1],".csv")  
}


#extract 
for (i in 1:nrow(species)){
  name<- read.csv(strings_sp[i,2], as.is=TRUE)
  assign(paste0("data_", species[i,2]), name)
}

#merge
# group into list so more mangagable 
sp_trait_list<-lapply(ls(pattern='data_'),get)

#bind list together
length(sp_trait_list)

x<-data.frame(nrow=1, ncol=25)

for (i in 1:length(sp_trait_list)){
  
}


  