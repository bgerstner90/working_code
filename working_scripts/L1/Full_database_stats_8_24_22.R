#Project: Montane Frugivoria - analyzing the montane database 

#Purpose: To analyze and assess what is in the montane frugivore database for the data paper.

#Date: August 24th, 2022

#By: Beth E. Gerstner

#read in mammal database
mam <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2022/databases_2022/Frugivoria_mammal_database_2022.csv")

bird<- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2022/databases_2022/Frugivoria_bird_database_2022.csv")


#Needed stats
#1) how many new traits in total
#a) for birds

#These are from previous databases
body_mass_b<- length(which(!is.na(bird$body_mass_e)))
diet_categ_b <- length(which(!is.na(bird$diet_cat_e))) 
nocturnal_b <- length(which(!is.na(bird$activity_nocturnal_e)))
for_strat_b <- length(which(!is.na(bird$for_strat_aerial_e)))
diet_inv_b <- length(which(!is.na(bird$diet_inv_e)))
#diet_vend_b <- length(which(!is.na(bird$diet_vend_e)))  
#diet_vect_b <- length(which(!is.na(bird$diet_vect_e)))   
#diet_fish_b <- length(which(!is.na(bird$diet_vfish_e)))  
#diet_vunk_b <- length(which(!is.na(bird$diet_vunk_e))) 
#diet_scav_b <- length(which(!is.na(bird$diet_scav_e)))  
#diet_fruit_b <-length(which(!is.na(bird$diet_fruit_e)))  
#diet_nect_b <-length(which(!is.na(bird$diet_nect_e)))                 
#diet_seed_b <-length(which(!is.na(bird$diet_seed_e)))                 
#diet_plant_b <-length(which(!is.na(bird$diet_plant_e)))                    

elton_traits_b <- body_mass_b + diet_categ_b + nocturnal_b + for_strat_b + diet_inv_b # 691 is %diet trait & for strat. Did not count each of the % columns seperately as this is part of a single trait (% diet makeup and % time in strata)
#5716 from Elton Traits

# count new bird traits that we have added
longevity_b <- length(which(!is.na(bird$longevity)))
home_range_b <- length(which(!is.na(bird$home_range_size)))
habitat_special_b <- length(which(!is.na(bird$habitat_specialization)))
gen_time_b <- length(which(!is.na(bird$generation_time)))
body_size_b <- length(which(!is.na(bird$body_size_mm)))
sexual_dim_b <- length(which(!is.na(bird$sexual_dimorphism)))
range_1_b <- length(which(!is.na(bird$observed_range_sqkm)))
range_2_b <-  length(which(!is.na(bird$inferred_range_sqkm)))

new_bird_traits <-  longevity_b + habitat_special_b + gen_time_b + body_size_b + sexual_dim_b + range_1_b + range_2_b # 6842 new traits

all_bird_traits <-  elton_traits_b + new_bird_traits #12,558 traits

#b) for mammals

#These are from previous databases (need to add Pantheria in here), try and sum NAs for x:x for Pantheria traits

diurnal_m <-length(which(!is.na(mam$activity_diurnal_e)))
crepuscular_m <-length(which(!is.na(mam$activity_crepuscular_e)))
nocturnal_m <- length(which(!is.na(mam$activity_nocturnal_e)))
body_mass_m<- length(which(!is.na(mam$body_mass_e)))
for_strat_m <- length(which(!is.na(mam$for_strat_value_e))) 
diet_inv_m <- length(which(!is.na(mam$diet_inv_e)))  #% diet will count as one trait

elton_traits_m <- diurnal_m + crepuscular_m + nocturnal_m + body_mass_m + for_strat_m + diet_inv_m #3594

#count Pantheria traits from within the database by looking at the citations saying "PanTHERIA".
in_db_pantheria <- length(which(mam=="PanTHERIA")) #8 in database. Should remove these from the new count.

#count the number of traits in PanTHERIA but remove the reference column
pantheria <- mam[,79:132]
pantheria$references_p <- NULL
pantheria_traits <- length(which(!is.na(pantheria))) #14,517

#count the number of newly added traits
diet_categ_m <- length(which(!is.na(mam$diet_cat)))
longevity_m <- length(which(!is.na(mam$longevity)))
home_range_m <- length(which(!is.na(mam$home_range_size)))
habitat_special_m <- length(which(!is.na(mam$habitat_specialization)))
gen_time_m <- length(which(!is.na(mam$generation_time)))
body_size_m<- length(which(!is.na(mam$body_size_mm)))
sexual_dim_m <- length(which(!is.na(mam$sexual_dimorphism)))
range_1_m <- length(which(!is.na(mam$observed_range_sqkm)))
range_2_m <-  length(which(!is.na(mam$inferred_range_sqkm)))

#all new traits added minus those within database from PanTHERIA
#add 3 ranges to be included
new_mam_traits <- diet_categ_m + longevity_m +home_range_m +range_1_m +range_2_m + habitat_special_m +gen_time_m +body_size_m + sexual_dim_m  # 4404

#Total number of traits in the database
all_mam_traits <- new_mam_traits + pantheria_traits + elton_traits_m #22,515

#Total number of traits in the entire dataset
all_mam_traits + all_bird_traits #35,073
#2) # of genera
#a) birds 
#b) mammals


library(tidyr)

#unique genera birds
length(unique(bird$genus)) #329 genera

#unique genera mammals
length(unique(mam$genus)) #160 genera

#Unique mammal species 
length(unique(mam$IUCN_species_name)) #584

#Unique Bird species 
length(unique(bird$IUCN_species_name)) #1148

#3) #New species/name changes
#New species - have either been reclassified or recently discovered <-- these are the original numbers. I need to write code that can quantify name changes. It should look at mismatches between the IUCN_name and the scientific_name (EltonTraits). These species can be new species that we've then imputed to a close relative, species that have been reclassified, species with misspellings that we've standardized to the IUCN name.

#42 new mammal species
#132 mammal species reclassified

# 2 new bird species
# 182 birds reclassified
# 8 Spelling errors

#Need to quantify mismatches between the elton_species_name and IUCN_species_name. 
#mammals

#how many mammal species names are mismatched?
length(mam$taxonomic_disparity[mam$taxonomic_disparity==1]) #174

#how many bird species names are mismatched?
length(bird$taxonomic_disparity[bird$taxonomic_disparity==1]) #195

#names mismatched
174+195 # 369 

#need to figure out how to count imputations from 42 new species for Elton Traits/PanTHERIA - based on close relatives for many traits
#subset to "new species" - run elton trait imputes again - all of those traits are then imputed (including PanTHERIA) - make sure not counting an imputed trait twice. 
#5) How many traits were imputed to family? (not including EltonTraits, will have to add this later on)
library (tidyr)
mam_genus_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed to genus: new traits
#home_range 73
#longevity 75
#generation_time 100
#body_size 95
#sexual dimorphism 77
#diet_cat #42 new species. need to know how many were reclassified to figure out what's imputed to genus

#total imputed for newly added traits
impute_mam_genus_new <-73+75+100+95+77 # 420


#percent imputed in total for genus
genus_impute_m <-impute_mam_genus_new/new_mam_traits #9.54% to genus for mammals

##imputed to family: new traits

#longevity 111
#generation_time 181
#body_size 5
#sexual dimorphism 34
#home range 55

mam_fam_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==-1)

impute_mam_fam_new <-111+181+5+34+55 # 386


#total traits imputed
total_mam_impute <-impute_mam_genus_new+impute_mam_fam_new #806

#%imputed for mammals
total_mam_impute/new_mam_traits # 18.3% new traits imputed to family or genus


#Bird imputation
bird_genus_level_impute<-bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed for birds
#home_range 47
#longevity 37
#sexual dim 1
#generation_time 1
#body_size 1

#Total imputed for newly added traits to genus
impute_bird_genus_new <-47+37+1+1+1 # 87

#total traits imputed
genus_impute_b <-impute_bird_genus_new/new_bird_traits #1.27%

#Bird imputation to family
bird_family_level_impute<-bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==-1)

#imputed for birds
#home_range 3
#longevity 75
#sexual dim 1


#Total imputed for newly added traits to genus
impute_bird_family_new <-1 + 75 + 1 # 1 

#total traits imputed
family_impute_b <-impute_bird_family_new/new_bird_traits # 1.12%

#total imputed for birds
total_bird_impute <-impute_bird_genus_new+impute_bird_family_new # 164

#%imputed for birds
total_bird_impute/new_bird_traits # 2.4% imputed to family or genus

#all database imputations
full_impute_b_m <- impute_bird_family_new + impute_bird_genus_new + impute_mam_fam_new + impute_mam_genus_new # 970

#all new traits for birds and mammals
all_new_traits_b_m <-new_mam_traits + new_bird_traits #11,246

#percent imputed in total for new traits (birds and mammals)
full_impute_b_m/all_new_traits_b_m #= 8.62% imputed for newly added traits

#Total genus impute for new traits
genus_impute <- impute_bird_genus_new + impute_mam_genus_new #507

#total family impute for new traits 
family_impute <- impute_mam_fam_new + impute_bird_family_new #463

#impute for EltonTraits - they include phylogenetically imputed data with a value of 2.
bird_phylo_level_impute <- bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==2) #

#impute for bird diet
length(bird$diet_certainty_e[bird$diet_certainty_e=="D1"]) #genus level diet info, 170
length(bird$diet_certainty_e[bird$diet_certainty_e=="D2"]) #family level diet info 33



mam_phylo_level_impute <- mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==2) #9 body mass

#impute for diet
length(mam$diet_certainty_e[mam$diet_certainty_e=="D1"]) #genus level diet info, 98
length(mam$diet_certainty_e[mam$diet_certainty_e=="D2"]) #family level diet info, 34

#impute for activity
length(mam$activity_certainty_e[mam$activity_certainty_e=="D1"]) #genus level diet info, 33
length(mam$activity_certainty_e[mam$activity_certainty_e=="D2"]) #family level diet info, 74

##Total family impute for Elton Traits. They do not distinguish between genus and family level imputations
##birds
#body mass: 56 genus or fam
#for strat: 53 genus or fam
#bird diet: 203 
##mam
#body mass: 78 genus or fam
#mam_diet: 132
#activity pattern: 107
#           9 phylogenetic imputations

all_elton_impute <-56 + 53 + 203 + 78 + 132 +107 + 9 #638

# Total # of imputations in database 
total_impute <- all_elton_impute + full_impute_b_m #1608


#How many species in the database are data deficient
length(mam[mam$IUCN_category=="DD",]) #132
length(bird[bird$IUCN_category=="DD",]) #78

#mammals with data deficiency
129/599 #21.5

#birds with data deficiency
78/1148 #6.79

#Additional new species imputes

new_bird <-read.csv("/Users/bethgerstner/Desktop/database_lowland_edits/lookup_table_frugivore_bird_species.csv")

new_mam <- read.csv("/Users/bethgerstner/Desktop/database_lowland_edits/lookup_table_frugivore_mammal_species.csv")

#Mammals
mam_new_sp_subset <- new_mam %>%
  filter(new_species==1) 

mam_database_new_sp <- mam %>%
  filter(IUCN_species_name %in% mam_new_sp_subset$IUCN_species_name) 

write.csv(mam_database_new_sp, "mam_database_new_sp.csv")

mam_phylo_level_impute_nsp <- mam_database_new_sp %>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==2) #0 body mass

#impute for diet
length(mam_database_new_sp$diet_certainty_e[mam_database_new_sp$diet_certainty_e=="D1"]) #genus level diet info, 5
length(mam_database_new_sp$diet_certainty_e[mam_database_new_sp$diet_certainty_e=="D2"]) #family level diet info, 3

#impute for activity
length(mam_database_new_sp$activity_certainty_e[mam_database_new_sp$activity_certainty_e=="D1"]) #genus level diet info, 0
length(mam_database_new_sp$activity_certainty_e[mam_database_new_sp$activity_certainty_e=="D2"]) #family level diet info, 4
 total_previously_included_imputes <- 5 + 3 + 4 #12 
 
 pantheria_new <- mam_database_new_sp[,79:132]
 pantheria_new$references_p <- NULL
 pantheria_traits_new <- length(which(!is.na(pantheria_new))) #1,020 imputed PanTHERIA traits for new species
 
 #42 new species so all Elton Traits and PanTHERIA traits will have been imputed
 #These are from previous databases (need to add Pantheria in here), try and sum NAs for x:x for Pantheria traits
 
 diurnal_m_new <-length(which(!is.na(mam_database_new_sp$activity_diurnal_e)))
 crepuscular_m_new <-length(which(!is.na(mam_database_new_sp$activity_crepuscular_e)))
 nocturnal_m_new <- length(which(!is.na(mam_database_new_sp$activity_nocturnal_e)))
 body_mass_m_new<- length(which(!is.na(mam_database_new_sp$body_mass_e)))
 for_strat_m_new <- length(which(!is.na(mam_database_new_sp$for_strat_value_e))) 
 diet_inv_m_new <- length(which(!is.na(mam_database_new_sp$diet_inv_e)))  #% diet will count as one trait
 
 elton_traits_m_new <- diurnal_m_new + crepuscular_m_new + nocturnal_m_new + body_mass_m_new + for_strat_m_new + diet_inv_m_new #252
 
 #subtract the imputes for Elton Traits that were already included in the total database numbers
 new_mam_imputes <- 252 - 12 + 1020
 
 total_mam_impute
 
#birds
 bird_new_sp_subset <- new_bird %>%
   filter(new_species==1) 
 
 bird_database_new_sp <- bird %>%
   filter(IUCN_species_name %in% bird_new_sp_subset$IUCN_species_name) 
 
 write.csv(bird_database_new_sp, "bird_database_new_sp.csv")
 
 bird_phylo_level_impute_nsp <- bird_database_new_sp %>%
   gather(x, value)%>%
   group_by(x)%>%
   tally(value==2) #0 body mass
 
 #impute for diet
 length(bird_database_new_sp$diet_certainty_e[bird_database_new_sp$diet_certainty_e=="D1"]) #genus level diet info, 1
 length(bird_database_new_sp$diet_certainty_e[bird_database_new_sp$diet_certainty_e=="D2"]) #family level diet info, 0
 
 #impute for activity
 length(bird_database_new_sp$activity_certainty_e[bird_database_new_sp$activity_certainty_e=="D1"]) #genus level diet info, 0
 length(bird_database_new_sp$activity_certainty_e[bird_database_new_sp$activity_certainty_e=="D2"]) #family level diet info, 0
 total_previously_included_imputes <- 1
 
 
 #2 new species so all Elton Traits have been imputed
 #These are from previous databases 
 

 nocturnal_b_new <- length(which(!is.na(bird_database_new_sp$activity_nocturnal_e)))
 body_mass_b_new<- length(which(!is.na(bird_database_new_sp$body_mass_e)))
 for_strat_b_new <- length(which(!is.na(bird_database_new_sp$for_strat_value_e))) 
 diet_inv_b_new <- length(which(!is.na(bird_database_new_sp$diet_inv_e)))  #% diet will count as one trait
 
 elton_traits_b_new <- nocturnal_b_new + body_mass_b_new + for_strat_b_new + diet_inv_b_new #6
 
 #subtract the imputes for Elton Traits that were already included in the total database numbers
 new_bird_imputes <-  6-1
 
 total_impute_with_new <- total_impute + new_bird_imputes + new_mam_imputes
 
# % imputed full database
 
 perc_impute_full_database <- total_impute_with_new/(all_mam_traits + all_bird_traits) #8.2%


#_____________________________________________________________________

#Figures:
#stacked barplot
library(ggplot2)

#Mammals
#pantheria %
pantheria_traits/all_mam_traits #64.47
#elton %
elton_traits_m/all_mam_traits #15.96
#new %
new_mam_traits/all_mam_traits #19.56

#birds
#elton
elton_traits_b/all_bird_traits #45.50
#new
new_bird_traits/all_bird_traits #54.49

category<- c("Mammals","Mammals","Mammals","Birds","Birds","Birds")
condition<-c("PanTHERIA","EltonTraits","New traits","PanTHERIA","EltonTraits","New traits")
percent <- c(64.47,15.96,19.56,0,45.50,54.49) # change these to the new values 
data <- data.frame(category,condition,percent)

# Stacked + percent
plot <-ggplot(data, aes(fill=condition, y=percent, x=category)) + 
  geom_bar(position="stack", stat="identity") + geom_text(aes(label = percent),size = 3, hjust = 0.6, vjust = 3, position =  "stack")  + scale_fill_manual(values=c("#999999", "#99CC00", "#488A99"), 
                                                                                                                                                           name="Data Source",
                                                                                                                                                           breaks=c("EltonTraits","New traits", "PanTHERIA"), labels=c("EltonTraits","New Traits in Frugivoria","PanTHERIA"))+ labs(x="Taxa")  + labs(y="Percent contribution") + theme(plot.background = element_rect(fill = "white"))

plot + theme(panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "dark gray", 
                                                                                       size = .5, linetype = "solid"))
