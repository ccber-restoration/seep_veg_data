## upload csv files that were downloaded from survey123 quadrat veg survey

library(tidyverse)
library(tidyselect)
library(janitor)

#This is the main data that has sum of native, nonnative, bareground, thatch and other

parent<- read_csv("original_data/CCBER_VegMonitoring_QuadratTransects_0.csv") %>%
  clean_names()
parent_colnames <- as.data.frame(colnames(parent))

#combine Project site and unlisted project site into one column
parent <- parent %>%
  mutate(project_site = case_when(
    grepl(pattern = "UNLISTED", x = project_site) ~ unlisted_project_site,
    TRUE ~ project_site)
    ) %>%
  select(global_id:monitor_names, project_site,  transect_name_number,
         seep_transect_name_number, total_transect_length:side_of_transect_left_or_right,
         quadrat_notes_optional:sum_of_all_cover, x, y) %>%
  rename(
    MasterID = global_id,
    Date = monitoring_date,
    Monitors = monitor_names,
    Site = project_site,
    Transect_length = total_transect_length,
    Start_Time = transect_start_time,
    Finish_Time = transect_finish_time,
    Notes = transect_notes,
    Transect_Distance = transect_distance_of_quadrat,
    Transect_Side = side_of_transect_left_or_right,
    Quadrat_Notes = quadrat_notes_optional,
    p_bare_ground = percent_bare_ground,
    p_Natural_thatch = percent_natural_thatch,
    p_mowed_thatch = percent_mowed_trimmed_thatch,
    Thatch_Notes = thatch_notes_optional,
    Count_Native_species = count_of_native_species,
    Sum_Native_cover = sum_of_native_cover,
    Count_nonnative_species = count_of_non_native_species,
    Sum_nonnative_cover = sum_of_non_native_cover,
    Count_Unknown_species = count_of_unknown_species,
    Sum_Unknown_cover = sum_of_unknown_cover,
    Sum_Thatch = sum_of_thatch,
    Sum_Other = sum_of_other_cover,
    Bare_Ground = bare_ground,
    Sum_all_cover = sum_of_all_cover,
    Seep_Transect_Name = seep_transect_name_number,
    Transect_Name = transect_name_number
  ) %>%
  mutate(
    Date = as.Date(Date, format<-"%m/%d/%Y"),
    Year = format(Date, format<-"%Y")
  ) %>%
  filter(Site == "harder_seep" | Site == "Seep")

parent_colnames <- as.data.frame(colnames(parent))

unique(parent$Transect_Name)

## Now create data sets for native, non natives, unknown and other cover to do statistical analysis on.  
natives<-read.csv("native_plants_begin_1.csv")
natives<-natives[,2:7]
str(natives)
colnames(natives)<-c("GlobalID","Native_species","unlisted_native","Native_pcover","Native_Notes","MasterID")
Natives<-merge(parent,natives, by="MasterID", all.x=TRUE, all.y=TRUE)
Natives$count<-1

nonnatives<-read.csv("nonnative_plants_begin_2.csv")
nonnatives<-nonnatives[,c(2:7)]
colnames(nonnatives)<-c("GlobalID","Nonnative_species","unlisted_nonnative","Nonnative_pcover","Nonnative_Notes","MasterID")
Nonnatives<-merge(parent,nonnatives, by="MasterID", all.x=TRUE, all.y=TRUE)

othercover<-read.csv("other_cover_repeat_begin_3.csv")
othercover<-othercover[,c(2:7)]
colnames(othercover)<-c("GlobalID","Othercover_type","unlisted_othercover","p_othercover","othercover_Notes","MasterID")
othercover<-merge(parent,othercover, by="MasterID", all.x=TRUE, all.y=TRUE)
othercover<-subset(othercover,othercover$Othercover_type!="")


unknownplants<-read.csv("unknown_plants_begin_2.csv")
unknownplants<-unknownplants[,c(2:5)]
colnames(unknownplants)<-c("GlobalID","Unknown_Species_description","Unknown_species_pcover","MasterID")
Unknown<-merge(parent,unknownplants, by="MasterID",all.x=TRUE,all.y=TRUE)
Unknown<-subset(Unknown,Unknown$Unknown_Species_description!="")

##rbind files into 1. First get the same column headings
#Bare ground
str(masterbareground)
colnames(parent)
masterbareground<-parent[,c(2,3,5,7,8,9,10,11,13,14,28,29,30)]
print(masterbareground)
masterbareground$PSOC<-"Bare Ground"
masterbareground$Cover_Category<-"BARE GROUND"
colnames(masterbareground)[10]<-"Percent_Cover"
print(masterbareground)
colnames(masterbareground)

#Natives
colnames(Natives)
print(Natives)
masterNatives<-Natives[,c(2,3,5,7,8,9,10,11,13,34,28,29,30,32)]
masterNatives$Cover_Category<-"NATIVE COVER"
colnames(masterNatives)[10]<-"Percent_Cover"
colnames(masterNatives)[14]<-"PSOC"
colnames(masterbareground)

mastersumNC<-parent[,c(2,3,5,7,8,9,10,11,13,19,28,29,30)]
mastersumNC$PSOC<-"Sum of Native Cover"
mastersumNC$Cover_Category<-"NATIVE COVER"
colnames(mastersumNC)[10]<-"Percent_Cover"
colnames(mastersumNC)

#Natural Thatch
masterNaturalthatch<-parent[,c(2,3,5,7,8,9,10,11,13,15,28,29,30)]
masterNaturalthatch$PSOC<-"Natural Thatch"
masterNaturalthatch$Cover_Category<-"THATCH"
colnames(masterNaturalthatch)[10]<-"Percent_Cover"
colnames(masterNaturalthatch)

mastersumthatch<-parent[,c(2,3,5,7,8,9,10,11,13,24,28,29,30)]
mastersumthatch$PSOC<-"Sum of Thatch Cover"
mastersumthatch$Cover_Category<-"THATCH"
colnames(mastersumthatch)[10]<-"Percent_Cover"
colnames(mastersumthatch)

#Non-Natives
colnames(Nonnatives)
masterNonnatives<-Nonnatives[,c(2,3,5,7,8,9,10,11,13,34,28,29,30,32)]
masterNonnatives$Cover_Category<-"NON-NATIVE COVER"
colnames(masterNonnatives)[10]<-"Percent_Cover"
colnames(masterNonnatives)[14]<-"PSOC"


mastersumNNC<-parent[,c(2,3,5,7,8,9,10,11,13,21,28,29,30)]
mastersumNNC$PSOC<-"Sum of Non-Native Cover"
mastersumNNC$Cover_Category<-"NON-NATIVE COVER"
colnames(mastersumNNC)[10]<-"Percent_Cover"

#Other Cover
colnames(othercover)
masterother<-othercover[,c(2,3,5,7,8,9,10,11,13,34,28,29,30,32)]
masterother$Cover_Category<-"OTHER COVER"
colnames(masterother)[10]<-"Percent_Cover"
colnames(masterother)[14]<-"PSOC"

#Add sum of other cover
mastersumother<-parent[,c(2,3,5,7,8,9,10,11,13,25,28,29,30)]
mastersumother$PSOC<-"Sum of Other Cover"
mastersumother$Cover_Category<-"OTHER COVER"
colnames(mastersumother)[10]<-"Percent_Cover"

#R bind the above sheets
df<-rbind(masterbareground,masterNatives,masterNonnatives,masterNaturalthatch,masterother,mastersumNC,mastersumNNC,mastersumother,mastersumthatch)

## Add habitat column to the files
df<-df %>%
  mutate(Habitat = case_when(
    grepl(pattern = "VP", x = Transect_Name) ~ "Vernal Pool",
    grepl(pattern = "GL", x = Transect_Name) ~ "Perennial Grassland",
    grepl(pattern = "BM", x = Transect_Name) ~ "Seasonal Brackish Marsh",
    grepl(pattern = "FSP", x = Transect_Name) ~ "Seasonal Freshwater Pond",
    grepl(pattern = "PU", x = Transect_Name) ~ "Peripheral Uplands",
    grepl(pattern = "SA", x = Transect_Name) ~ "Sandy Annuals",
    grepl(pattern = "SF", x = Transect_Name) ~ "Sand Flat",
    grepl(pattern = "SML", x = Transect_Name) ~ "Salt Marsh",
    grepl(pattern = "SMM", x = Transect_Name) ~ "Salt Marsh",
    grepl(pattern = "SMR", x = Transect_Name) ~ "Remnant Salt Marsh",
    grepl(pattern = "SMT", x = Transect_Name) ~ "Transition/High Salt Marsh",
    grepl(pattern = "SMTH", x = Transect_Name) ~ "Transition/High Salt Marsh",
    grepl(pattern = "U", x = Transect_Name) ~ "Upland",
    grepl(pattern = "WL", x = Transect_Name) ~ "Wetland"
    
  ))

unique(df$Transect_Name)

write.csv(df,"Seep_MasterQuadratdata_3_2025.csv")








#Cleaning up dataset
unique(df$Site)
df<- within(df, Site[Site == "ncosNCOS"] <- 'ncos')
df$Transect_Name<-gsub("VP_ ","VP-",as.character(df$Transect_Name))
df$Transect_Name<-gsub("Vp","VP-",as.character(df$Transect_Name))
df$Transect_Name<-gsub("VP-VP-","VP-",as.character(df$Transect_Name))
df$Transect_Name<-gsub("_","-",as.character(df$Transect_Name))

df$Transect_Name<-gsub("-1","-01",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-2","-02",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-3","-03",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-4","-04",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-5","-05",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-6","-06",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-7","-07",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-8","-08",as.character(df$Transect_Name))

df$Transect_Name<-gsub("-010","-10",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-011","-11",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-012","-12",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-014","-14",as.character(df$Transect_Name))
df$Transect_Name<-gsub("-015","-15",as.character(df$Transect_Name))

unique(df$Transect_Name)

NCOS<-df[df$Site=="ncos",]
unique(NCOS$Transect_Name)


df<- within(df, Site[Site == "ncosNCOS"] <- 'ncos')
NCOS$Transect_Name_clean<-NCOS$Transect_Name
NCOS$Transect_Name<-gsub("VP-Wetland 1 /vp7","",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("VP-Chumash Wetland","",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("VP-Santa Cruz","",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("V-","",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("--","-",as.character(NCOS$Transect_Name))

NCOS$Transect_Name<-gsub("SMM-05SMM05","SMM-05",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("SMT-05SMT5","SMT-05",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("VP03","03",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("VP-Santa Cruz","",as.character(NCOS$Transect_Name))

NCOS$Transect_Name<-gsub("PU-05VP-01","PU-05",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("PU-04VP-01","PU-04",as.character(NCOS$Transect_Name))
NCOS$Transect_Name<-gsub("PU-02VP-01","PU-02",as.character(NCOS$Transect_Name))


write.csv(df,"Seep_MasterQuadratdata_3_2025.csv")


write.csv(NCOS,"CCBER-Vegetation-Monitoring/Data/NCOS_2024.csv")













