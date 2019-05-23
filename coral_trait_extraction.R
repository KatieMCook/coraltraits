###CORAL TRAITS EXTRACTION ####
library(dplyr)
library(reshape2)
library(tidyr)



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

species_list$master_species<-as.character(species_list$master_species)

#japan MoE data
species<- read.csv('moe_list_1103.csv')
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
View(sp_trait_list[[1]])
View(sp_trait_list[[2]])

View(sp_trait_list[[3]])

#bind list together
length(sp_trait_list)

df_all<-do.call('rbind', sp_trait_list)

head(df_all)

#get the neccessary cols 

head(df_all)
df_all<-df_all[,c(5,7,14,16,19,20)]
head(df_all)

#which values are categories and which are numbers
df_all$numeric<-!is.na(as.numeric(df_all$value))  #all the trues are numbers 

head(df_all$numeric)


numeric_df<-df_all[df_all$numeric==TRUE,]
cat_df<-df_all[df_all$numeric==FALSE,]

#now we have split the numeric and the categories
unique(df_all$location_name)

#japan location names   #ok don't do this for now
"Sesoko Island, Okinawa"
"Kushimotoâ€\u0090Shirahama, Japan"
"Tsushima, Japan"
"Sekisei Lagon, Okinawa"
"Amami, Japan"
"Kuroshima Island"
"Amakusa, Japan"
"Ishigaki Island"
"Shikoku Island, Japan"
"Okinawa, Japan"

head(df_all)
df_all<-df_all[,c(5,7,14,16,19,20)]
head(df_all)

sesoko<-which(df_all$location_name=="Sesoko Island, Okinawa")
df_all[sesoko,] # Acropora aspera Acropora digitifera Acropora gemmifera  Coelastrea aspera Ctenactis crassa  Goniastrea retiformis Montipora aequituberculata Montipora digitata
df_all[df_all$specie_name=='Acropora aspera',]


#summarise numeric df
head(numeric_df)
numeric_df$value <-as.numeric(numeric_df$value)

numeric_df_mean <- numeric_df %>% group_by(specie_name, trait_name, standard_unit) %>% summarise(value= mean(value))

#summarise categorical df 
cat_df$cat_val<-1
head(cat_df)
cat_df_sum<- cat_df %>% group_by(specie_name, trait_name, value) %>% summarise(cat_no=sum(cat_val))
cat_df_sum$cat_no                                                                               #ok now take the value of the col with the highest number

#get rid of the traits we dont want
unique(cat_df_sum$trait_name)
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Genus fossil stage")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Indo-Pacific faunal province")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Zooxanthellate")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Symbiodinium clade")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Symbiodinium subclade")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Month")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Year")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Abundance GBR")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Season")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Geographical region")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Growth form Veron")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Fish association controlled")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Plankton controlled")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$trait_name=="Habitat transplanted from")),]
cat_df_sum<-cat_df_sum[-c(which(cat_df_sum$value=='laboratory')),]

#remove habitat type= lab

#check for duplicates
cat_df_sum$dups<-1
duplicates<- cat_df_sum %>% group_by(specie_name, trait_name) %>% mutate(dups=sum(dups))
duplicates<-duplicates[c(which(duplicates$dups>1)),]

#ignore veron growth form, multiple habitat type= generalist
water_depth<-duplicates[c(which(duplicates$trait_name=='Water depth')),]

#write csv for water depth, can add back in as min and max depth on excel if needed
write.csv(water_depth, 'water_depth.csv')

#same for temp
water_temp<-duplicates[c(which(duplicates$trait_name=='Water temperature')),]
write.csv(water_temp, 'water_temp.csv')

#now sort out habitat type 
duplicates<-duplicates[-c(which(duplicates$trait_name=='Water depth')),]
duplicates<-duplicates[-c(which(duplicates$trait_name=='Water temperature')),]


#write the cats ands the num dfs
write.csv(cat_df_sum, 'categorical_trait_df.csv')
write.csv(numeric_df_mean, 'numeric_df_mean.csv')


#now combine the two 
numeric_df_mean$value<-as.character(numeric_df_mean$value)
df_all<-bind_rows(cat_df_sum, numeric_df_mean)
View(df_all)
df_all<-df_all[,-c(4,5)]

#now go wide.. huh? not working
head(df_all)
head(df_all$value)
df_all<-df_all[,-c(4)]

df_wide_all<-acast(df_all, specie_name~trait_name, value, drop=TRUE, subset=NULL)
View(df_wide_all)

#check for duplicates again? 
df_all$dups<-1

duplicates_all<- df_all %>% group_by(specie_name, trait_name) %>% mutate(dups=sum(dups))
duplicates2<-duplicates_all[c(which(duplicates_all$dups>1)),]

#remove duplicate rows from df_all
df_all<-df_all[-c(which(duplicates_all$dups>1)),]

df_all<-df_all[,-4]

#try again
df_wide_all<-spread(df_all, key=trait_name, value=value )

rm(df_wide_all)

#ok all good

#remove cols with more than 200NA (half)
length(which(is.na(df_wide_all[,4])))

NA_cols<-sapply(df_wide_all, function(x) sum(is.na(x)))

wide_all<-(df_wide_all[,c(which(NA_cols<200))])

wide_all$Sexual_system<- df_wide_all$`Sexual system`
wide_all$life_hist_strat<-df_wide_all$`Life history strategy`
wide_all$larval_development<-df_wide_all$`Mode of larval development`
wide_all$growth_rate<-df_wide_all$`Growth rate`

#save the big df with all traits
write.csv(df_wide_all, 'alltraits_sp.csv')
write.csv(wide_all, 'complete_traits_sp.csv')

#now check depths are ok 
which(is.na(wide_all$`Depth lower`))

wide_all$specie_name[19]
wide_all$specie_name[c(57,62,89, 101, 113, 126 ,215, 307, 308 ,349 ,367, 383 ,401)] #which species don't have depth info available for depth lower/upper 

which(duplicates2$specie_name=="Acropora copiosa")
which(duplicates2$specie_name=="Acropora parilis")
which(duplicates2$specie_name=="Alveopora japonica" )
which(duplicates2$specie_name=="Astreopora explanata"  )
which(duplicates2$specie_name=="Boninastrea boninensis")
which(duplicates2$specie_name=="Coscinaraea hahazimaensis")
which(duplicates2$specie_name=="Goniopora polyformis")
which(duplicates2$specie_name=="Mycedium mancaoi"   )
which(duplicates2$specie_name=="Mycedium robokaki")
which(duplicates2$specie_name=="Plerogyra eurysepta" )
which(duplicates2$specie_name=="Porites cocosensis")
which(duplicates2$specie_name=="Porites okinawensis")
which(duplicates2$specie_name=="Stylaraea punctata"  )     #ok they all have no depth info in both depth lower/upper and depths. 


#check growth rates
growthrate_dup<-duplicates2[c(which(duplicates2$trait_name=='Growth rate')),]
head(growthrate_dup)

#FINISH!!! 
growthrate_dup$value<-as.numeric(growthrate_dup$value)
growthrate_dup$specie_name<-as.factor(growthrate_dup$specie_name)

levels(growthrate_dup$specie_name)

growthrate<- growthrate_dup %>% group_by(specie_name) %>% summarise(growth= mean(value))

#ok now do by genus?
#process: make long form again, split species into genus, sort out the acropora corymbose etc, split by numeric/ categories./ repeat

#START HERE FOR GENUS LEVEL
#now read in file edited in excel----
species_traits<-read.csv('complete_traits_sp.csv')

species_traits<-data.frame( lapply(species_traits, as.character), stringsAsFactors=FALSE)

#actually lets work with wide for now
#split species into genus and sp.
genus<-data.frame(do.call(rbind, strsplit(species_traits$specie_name, " ")))

species_traits$genus<- genus$X1
species_traits$species<-genus$X2

#now split acropora into growth form
species_traits$genus<-as.character(species_traits$genus)

ac<-species_traits[species_traits$genus=='Acropora',]

unique(ac$Growth.form.typical)

branching<- species_traits[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=='branching_open')),]

species_traits$genus[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=='branching_open'))]<-'Acropora_arborescent'

species_traits$genus[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=="branching_closed"))]<-'Acropora_arborescent'

species_traits$genus[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=="digitate"))]<-'Acropora_digitate'

species_traits$genus[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=="corymbose"))]<-'Acropora_corymbose'

species_traits$genus[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=="hispidose"))]<-'Acropora_thickets'

species_traits$genus[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=="tables_or_plates"))]<-'Acropora_plate'

which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=='encrusting') #only 1 should be fine

species_traits$genus[c(which(species_traits$genus=='Acropora' & species_traits$Growth.form.typical=="encrusting"))]<-'Acropora_encrusting'

ac2<-species_traits[species_traits$genus=='Acropora',] #one species without growth form, rare leave this

#now split montipora
mont<-species_traits[species_traits$genus=='Montipora',]
unique(mont$Growth.form.typical)

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="laminar"))]<-'Montipora_lam'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="branching_open"))]<-'Montipora_branching'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="branching_closed"))]<-'Montipora_branching'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="columnar"))]<-'Montipora_columnar'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="encrusting_long_uprights"))]<-'Montipora_enc_mass'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="massive"))]<-'Montipora_enc_mass'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="submassive"))]<-'Montipora_enc_mass'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="encrusting"))]<-'Montipora_enc_mass'

species_traits$genus[c(which(species_traits$genus=='Montipora' & species_traits$Growth.form.typical=="digitate" ))]<-'Montipora_branching'

#now astreopora
ast<-species_traits[species_traits$genus=='Astreopora',]
unique(ast$Growth.form.typical)

species_traits$genus[c(which(species_traits$genus== 'Astreopora' & species_traits$Growth.form.typical=="laminar"))]<-'Astreopora_laminar'

species_traits$genus[c(which(species_traits$genus== 'Astreopora' & species_traits$Growth.form.typical=="encrusting"))]<-'Astreopora_enc_mass'

species_traits$genus[c(which(species_traits$genus== 'Astreopora' & species_traits$Growth.form.typical=="massive"))]<-'Astreopora_enc_mass'

#hynophora
hyd<-species_traits[species_traits$genus=='Hydnophora',]

unique(hyd$Growth.form.typical)

species_traits$genus[c(which(species_traits$genus== 'Hydnophora' & species_traits$Growth.form.typical=="digitate"))]<-'Hydnophora_branching'

species_traits$genus[c(which(species_traits$genus== 'Hydnophora' & species_traits$Growth.form.typical=="branching_closed"))]<-'Hydnophora_branching'

species_traits$genus[c(which(species_traits$genus== 'Hydnophora' & species_traits$Growth.form.typical=="massive"))]<-'Hydnophora_enc_mass'

species_traits$genus[c(which(species_traits$genus== 'Hydnophora' & species_traits$Growth.form.typical=="encrusting_long_uprights"))]<-'Hydnophora_enc_mass'

species_traits$genus[c(which(species_traits$genus== 'Hydnophora' & species_traits$Growth.form.typical=="submassive"))]<-'Hydnophora_enc_mass'

#porites 
por<-species_traits[species_traits$genus=='Porites',]

unique(por$Growth.form.typical)

species_traits$genus[c(which(species_traits$genus=='Porites'  & species_traits$Growth.form.typical=="digitate"))]<-'Porites_branching'

species_traits$genus[c(which(species_traits$genus=='Porites'  & species_traits$Growth.form.typical=="branching_closed"))]<-'Porites_branching'

species_traits$genus[c(which(species_traits$genus=='Porites'  & species_traits$Growth.form.typical=="massive"))]<-'Porites_massive'

species_traits$genus[c(which(species_traits$genus=='Porites'  & species_traits$Growth.form.typical=="encrusting_long_uprights"))]<-'Porites_lam_enc'

species_traits$genus[c(which(species_traits$genus=='Porites'  & species_traits$Growth.form.typical=="laminar"))]<-'Porites_lam_enc'

species_traits$genus[c(which(species_traits$genus=='Porites'  & species_traits$Growth.form.typical=="encrusting"))]<-'Porites_lam_enc'

species_traits$genus[c(which(species_traits$genus=='Porites'  & species_traits$Growth.form.typical=="columnar"))]<-'Porites_massive'


#now make all fungidaes into one family
fun<-species_traits[species_traits$genus=='Cantharellus',]

species_traits$genus[c(which(species_traits$genus=='Cantharellus'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Ctenactis',]

species_traits$genus[c(which(species_traits$genus=='Ctenactis'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Cycloseris',]

species_traits$genus[c(which(species_traits$genus=='Cycloseris'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Danafungia ',] #none

fun<-species_traits[species_traits$genus=='Halomitra',] 

species_traits$genus[c(which(species_traits$genus=='Halomitra'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Heliofungia',] 

species_traits$genus[c(which(species_traits$genus=='Heliofungia'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Herpolitha',] 

species_traits$genus[c(which(species_traits$genus=='Herpolitha'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Lobactis',] #none

fun<-species_traits[species_traits$genus=='Pleuractis',] #none

fun<-species_traits[species_traits$genus=='Polyphyllia',]

species_traits$genus[c(which(species_traits$genus=='Polyphyllia'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Sandalolitha',]

species_traits$genus[c(which(species_traits$genus=='Sandalolitha'))]<-'Fungiidae_family'

fun<-species_traits[species_traits$genus=='Sinuorota',] #none

fun<-species_traits[species_traits$genus=='Zoopilus',]

species_traits$genus[c(which(species_traits$genus=='Zoopilus'))]<-'Fungiidae_family'

#ok now it matches the 'genus' groups in brigittes data


#make it long form again

#species_traits_long<- gather(species_traits, key='trait_name', value='value') doesnt work
library(reshape2)

species_traits_long<-melt(species_traits, id.vars = c('specie_name', 'genus') )

#ok now make categories #min and max???? #also mean do both to check 

#first remove the rows with NA in the value col #then split into categorical and numeric and group by genus

traits_naless<-species_traits_long[c(!(is.na(species_traits_long$value))),]

#split into numeric and categorical again 
#which values are categories and which are numbers
traits_num<-!is.na(as.numeric(traits_naless$value)) #trues are numbers

g_traits_num<-traits_naless[traits_num==TRUE,]

g_traits_cat<-traits_naless[traits_num==FALSE,]

#work on the numeric traits
#group by and then take min, max and mean
g_traits_num$genus<-as.factor(g_traits_num$genus)
g_traits_num$value<-as.numeric(g_traits_num$value)

levels(g_traits_num$genus)


genus_traits_num<- g_traits_num %>% group_by(genus, variable) %>% summarise( mean=mean(value), max=max(value), min=min(value))
genus_traits_num$range<-genus_traits_num$max - genus_traits_num$min


#categorical 
g_traits_cat$cat_val<-1
head(g_traits_cat)

g_cat_sum<-g_traits_cat %>% group_by(genus, variable, value) %>% summarise(cat_no=sum(cat_val))

#now find repeated cats
g_cat_sum$dups<-1

head(g_cat_sum)

g_duplicates<- g_cat_sum %>% group_by(genus, variable) %>% mutate(dups=sum(dups))

#check for duplicates
duplicates<-g_duplicates[c(which(g_duplicates$dups>1)),]

#remove non-duplicates?
non_dups<-g_duplicates[c(which(!(g_duplicates$dups>1))),]

#merge non_dups with numeric, then export to add in duplicates on excel?
#make value of numeric traits mean
head(genus_traits_num)
names(genus_traits_num)<-c('genus', 'variable', 'value', 'max', 'min', 'range')

genus_traits_num$value<-as.character(genus_traits_num$value)

genus_traits_all<-rbind(non_dups, genus_traits_num)


#write long non duplicated traits 
write.csv(genus_traits_all, 'long_nondup_genus_traits.csv')

#now make it wide
library(tidyr)
genus_traits_all$genus<-as.factor(genus_traits_all$genus)
genus_traits_all$variable<-as.factor(genus_traits_all$variable)
genus_traits_clean<-genus_traits_all[,c(1:3)]

genus_wide_nondup<-spread(genus_traits_clean, key=variable, value=value)


write.csv(genus_wide_nondup, 'genus_trait_uniquecases.csv')
