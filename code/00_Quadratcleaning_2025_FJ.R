## This script imports csv files downloaded from the survey123 quadrat veg survey (CCBER VegMonitoring QuadratTransects)
#and processes into analyzable data

#Provenance:
#This script was originally uploaded to Box by Alison Rickard in November 2024
#Modified by Claire WS in September 2025
#Then revised by Francis J in Sep & November 2025

# load packages ----
library(tidyverse)
#note that this already loads dplyr and tidyr, so no need to load those separately
#install.packages should really only be run in the console, not baked into scripts

#tidyselect is part of the tidyverse, but not a core package, so needs to be loaded separately
library(tidyselect)

#don't need this unless actually writing to excel. this script only write to csv...
#library(writexl)

#read in main ("parent") data ---- 

# This is the main data that has sum of native, nonnative, bareground, thatch and
# other

# could use use read_csv() from the tidyverse, rather than read.csv() 
parent <- read.csv("original_data/CCBER_VegMonitoring_QuadratTransects_0.csv", strip.white = TRUE)
#11022 observations

#save list of column names as data frame
parent_colnames <- as.data.frame(colnames(parent))

#this won't run unless the parent object has already been defined.
str(parent)

#Fix cases where vernal pool name or number is blank 

#if_else arguments: condition (logical conditional), true, false (vectors to use), missing 

#here, if vernal pool name or number is not blank, add VP_ prefix; if blank, leave blank
parent$Vernal.Pool.Name.or.Number<- ifelse(parent$Vernal.Pool.Name.or.Number != "",
                                           paste("VP_", parent$Vernal.Pool.Name.or.Number), "")

#how many unique vernal pool codes?
unique(parent$Vernal.Pool.Name.or.Number)
#27 codes

#check column names again
colnames(parent)

#combine Project site and unlisted project site into one column, with no separator
#using unite works because each row only ever has a value in one column?

#FJ- I would consider avoiding overwriting this column, and instead create a new column using mutate and case_when()
parent<-parent %>% unite(col = "Project.Site", Project.Site:Unlisted.Project.Site, sep = "")
#Combining columns to include unlisted sites
#drops to 44 columns

#create new column for Transect Name or Number using unite() function 

#Only one column will actually have a value, 
#so using no separator means you just keep the column value that is not blank. 
#The advantage of this is that because remove = TRUE by default, this consolidates columns
parent<-parent %>% unite(col = "Transect.Name.Number", 
                         c(Lagoon.Transect.Name.Number:NP.Transect.Name.Number,
                           Vernal.Pool.Name.or.Number,Transect.Name.Number,
                           Seep.Transect.Name.Number), sep = "")
#drops to 38 columns

#Remove UNLISTED from transect names (substitute "UNLISTED" with blank)
parent$Transect.Name.Number<-gsub("UNLISTED","",parent$Transect.Name.Number)

#Remove UNLISTED from the project site names
parent$Project.Site<-gsub("UNLISTED","",parent$Project.Site)

parent

#save list of column names as data frame
parent_colnames <- as.data.frame(colnames(parent))

#Remove unnecessary columns
parent<-parent %>% 
  select(GlobalID:Project.Site, Total.Transect.Length:SUM.of.ALL.COVER,x, y) 
#drops to 31 columns, as expected

#rename columns using underscores 
#FIXME- would be better to use clean_names() from janitor package, in combination with 
#rename function 
colnames(parent)<-c("MasterID","Date","Monitors","vernal_pool","Transect_Name",
                    "VP_axis","Site","Transect_length","Start_Time","Finish_Time",
                    "Notes","Transect_Distance","Transect_Side","VP_zone",
                    "Quadrat_Notes","p_bare_ground","p_Natural_thatch",
                    "p_mowed_thatch","Thatch_Notes","Count_Native_species",
                    "Sum_Native_cover","Count_nonnative_species",
                    "Sum_nonnative_cover", "Count_Unknown_species",
                    "Sum_Unknown_cover","Sum_Thatch","Sum_Other","Bare_Ground",
                    "Sum_all_cover","x","y")

#format dates properly
parent$Date<-as.Date(parent$Date,format<-"%m/%d/%Y %I:%M:%S %p")

#create year column
parent$Year<-format(parent$Date,format<-"%Y")

#Cleaning up transect names in dataset
#how many unique site names?
unique(parent$Site)
#34 codes

#replace incorrect site names 
#better to use mutate & case_when() here
parent<- within(parent, Site[Site == "ncosNCOS"] <- 'ncos')
parent<- within(parent, Site[Site == "harder_seepEast Storke Wetland"] <- 'harder_seep')
parent<- within(parent, Site[Site == "Seep"] <- 'harder_seep')
parent<- within(parent, Site[Site == "harder_seepHarder Seep"] <- 'harder_seep')
parent<- within(parent, Site[Site == "West Storke"] <- 'west_storke_wetland')
parent<- within(parent, Site[Site == "WSW"] <- 'west_storke_wetland')

unique(parent$Site)
#now 30 site codes

#clean transect names
parent$Transect_Name<-gsub("VP_ ","VP-",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("Vp","VP-",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("VP-VP-","VP-",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("_","-",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-1","-01",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-2","-02",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-3","-03",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-4","-04",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-5","-05",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-6","-06",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-7","-07",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-8","-08",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-010","-10",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-011","-11",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-012","-12",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-014","-14",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("-015","-15",as.character(parent$Transect_Name))

#list unique transect codes
unique(parent$Transect_Name)
#302 transects

#rename transects
parent$Transect_Name<-gsub("VP-Wetland 1 /vp7","",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("V-","",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("--","-",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("SMM-05SMM05","SMM-05",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("SMT-05SMT5","SMT-05",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("VP03","03",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("PU-05VP-01","PU-05",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("PU-04VP-01","PU-04",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("PU-02VP-01","PU-02",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("VP-VP-04","VP-04",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("VP-VP-03","VP-03",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("WL4WWL4W","WL4W",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("WL3WWL3W","WL3W",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("U2WU2W","U2W",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("SMTH-018","SMTH-18",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("SMTH-021","SMTH-21",as.character(parent$Transect_Name))
parent$Transect_Name<-gsub("SMTH-022","SMTH-22",as.character(parent$Transect_Name))

#filter within parent and rename ncosNCOS transect
parent<- within(parent, Site[Site == "ncosNCOS"] <- 'ncos')

#filter to site == harder_seep
seep<-parent[parent$Site=="harder_seep",]
#432 observations

#list unique transects within Seep
unique(seep$Transect_Name)
#9 transects

#save as vector
seep_transects <- unique(seep$Transect_Name)

unique(parent$Transect_Name)
#284 transects in overall data set

# Check for missing surveys this year ----
df25<-parent[parent$Year == "2025" & parent$Site=="harder_seep",]
#240 rows

#check column names
colnames(df25)

#group rows by transect name, then summarize quadrat count
df25grp<-df25%>% 
  #group by transect name
  group_by(Transect_Name) %>%
  #summarize by quadrat count
  mutate(quad_Count = n())%>%
  #filter to the first rownumber?
  filter(row_number()==1) 

# looks like we did not complete WL4W, WL3W, and U2W in the fall


#select subset of columns
df25grp<-df25grp %>% 
  select(c(vernal_pool, Transect_Name, Transect_length, quad_Count)) %>% 
  #new column "quadrat_freq" equal to transect length divided by 1 less than the quadrat count
  mutate(quadrat_freq = Transect_length/(quad_Count-1))
  #drops to 5 columns


#old subsetting code:
#[,c(4,5,8,33)]

#old code instead of mutate  
#df25grp <- transform(df25grp, quadrat_freq = Transect_length / (quad_Count - 1))

df25grp
## quadrat_frequency should be 2 for vernal pools and 3 for all other transects;
## if the number is incorrect, check the data in survey123 for missing/duplicate
## quadrats.

#Two vernal pool transects have an irregular interval so the frequency may not 
#equal 2 (VP-08 and VP-05)

#natives ----

## Now create data sets for native, non natives, unknown and other cover to do 
## statistical analysis on.

natives<-read.csv("original_data/native_plants_begin_1.csv") %>% 
#33,533 rows
  #select a subset of columns
  select(c(GlobalID:ParentGlobalID)) %>% 
  #rename columns
  rename(GlobalID = GlobalID,
         Native_species = Native.Species,
         unlisted_native = Unlisted.Native.Species,
         Native_pcover = Native.Species.Percent.Cover,
         Native_Notes = Native.Species.Notes,
         MasterID = ParentGlobalID) %>% 
  mutate(Native_species = case_when(
          unlisted_native != ""  ~ unlisted_native,
          TRUE ~ Native_species)
  )


str(natives)


#join (full) the species names into the parent data frame
Natives<-merge(parent, natives, by="MasterID", all.x=TRUE, all.y=TRUE)
# check if number of rows matches

unmatched_natives <- anti_join(parent, natives, by = "MasterID")
# currently zero unmatched natives
# CWS: Catches common error in the original Survey123 data. Users may 
# accidentally delete all native repeats, resulting in NA for native percent 
# cover, rather than 0. If unmatched_natives is more than 0, edit the original 
# survey to have at least one repeat in each cover category. Use 
# unmatched_natives to identify the culprits. Same for nonnatives below.

# non-natives ----
nonnatives<-read.csv("original_data/nonnative_plants_begin_2.csv") %>% 
  #subset columns
  select(GlobalID:ParentGlobalID) %>%
  rename(GlobalID = GlobalID,
         Nonnative_species = Non_Native.Species,
         unlisted_nonnative = Unlisted.Non.Native.Species,
         Nonnative_pcover = Non.Native.Species.Percent.Cover,
         Nonnative_Notes = Non.Native.Species.Notes,
         MasterID = ParentGlobalID) %>% 
  mutate(Nonnative_species = case_when(
    unlisted_nonnative != ""  ~ unlisted_nonnative,
    TRUE ~ Nonnative_species)
  )

#33,565 rows

#merge (full join)
Nonnatives<-merge(parent,nonnatives, by="MasterID", all.x=TRUE, all.y=TRUE)
# number of rows matches at 33,565

unmatched_non_natives <- anti_join(parent, nonnatives, by = "MasterID")
#no unmatched non-natives

#other cover ----
othercover<-read.csv("original_data/other_cover_repeat_begin_4.csv") %>% 
#11,140 rows
  #subset to 6 columns
  select(c(GlobalID:ParentGlobalID)) %>% 
  #rename columns (new name = old name)
  rename(GlobalID = GlobalID,
         Othercover_type = Other.Cover.Type,
         unlisted_othercover = Unlisted.Other.Cover,
         p_othercover = Percent.Other.Cover,
         othercover_Notes = Other.Cover.Notes,
         MasterID = ParentGlobalID)

#merge (full join)
Othercover<-merge(parent,othercover, by="MasterID", all.x=TRUE, all.y=TRUE)
# number of rows matches!
# discrepancy caused by a similar error to previous with natives and nonnatives.
# When prompted to enter other cover, users sometimes enter the percent value in
# the "other cover description" field on accident, resulting in NA values in the 
# percent field; requires some cleanup in the original surveys.

#check what records are in parent that aren't present in othercover for a given MasterID
unmatched_othercover <- anti_join(parent, othercover, by = "MasterID")
#0 rows, all good

#subset to non-blank values for other cover
othercover<-subset(othercover,othercover$Othercover_type!="")
#drops to 1825

#unknown plants
unknownplants<-read.csv("original_data/unknown_plants_begin_3.csv") %>% 
  #11038 rows
  #subset to 4 columns
  select(c(GlobalID:ParentGlobalID)) %>% 
  #rename columns 
  rename(GlobalID = GlobalID,
         Unknown_Species_description = Unknown.Species.Description,
         Unknown_species_pcover = Unknown.Species.Percent.Cover,
         MasterID = ParentGlobalID)

#join
Unknown<-merge(parent,unknownplants, by="MasterID",all.x=TRUE,all.y=TRUE)
#also 11038 rows 

#anything fail to match?
unmatched_unknown <- anti_join(parent, unknownplants, by = "MasterID")
#0 rows, all good

#filter out blank rows (i.e. quadrat rows that did not contain unknown species)
Unknown<-subset(Unknown,Unknown$Unknown_Species_description!="")
#just 75 observations

#combine data frames ----

## rbind files into 1. 

## 1. First ensure the column headings are the same among dataframes ----

### Bare ground ----

#old code, replaced
#masterbareground<-parent[,c(2,3,5,6,7,8,13,14,15,28)]

masterbareground <- parent %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length,
         Transect_Distance, Quadrat_Notes, Bare_Ground) %>% 
  #11034 rows
  #rename column
  rename(Percent_Cover = Bare_Ground)

str(masterbareground)

#create new column always equal to "Bare Ground"
masterbareground$PSOC<-"Bare Ground"

#make cover category for all rows "BARE GROUND
masterbareground$Cover_Category<-"BARE GROUND"

### Natives ----
#check column names
colnames(parent)

colnames(Natives)


#old code for selecting columns  
#Natives[,c(2,3,5,6,7,8,13,14,15,34,36)]

#subset columns
masterNatives<- Natives %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance,
         Quadrat_Notes, Native_species, Native_pcover) %>% 
  mutate(Cover_Category = "NATIVE COVER") %>% 
  rename(PSOC = Native_species,
         Percent_Cover = Native_pcover)

#sum native cover
mastersumNC <- parent %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance,
         Quadrat_Notes, Sum_Native_cover) %>% 
  mutate(PSOC = "Sum of Native Cover") %>% 
  mutate(Cover_Category = "NATIVE COVER") %>% 
  rename(Percent_Cover = Sum_Native_cover)

#old code specifying which columns to select
#mastersumNC<-parent[,c(2,3,5,6,8,13,14,15,22,7)]

### Natural Thatch ----

#select columns
masterNaturalthatch <- parent %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance, 
                Quadrat_Notes, p_Natural_thatch) %>% 
  mutate(PSOC = "Natural Thatch",
         Cover_Category = "THATCH") %>%
  rename(Percent_Cover = p_Natural_thatch)
  

#old code for selecting sumthatch columns
#parent[,c(2,3,5,6,8,13,14,15,27,7)]

mastersumthatch<- parent %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance,
         Quadrat_Notes, Sum_Thatch) %>%
  mutate(PSOC = "Sum of Thatch Cover",
         Cover_Category = "THATCH") %>%
  rename(Percent_Cover = Sum_Thatch)
  
### Non-Natives ----
colnames(Nonnatives)

# old code for selecting columns
#masterNonnatives<-Nonnatives[,c(2,3,5,6,7,8,13,14,15,34,36)] 

masterNonnatives <- Nonnatives %>% 
  #select subset of columns
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance,
         Quadrat_Notes, Nonnative_species, Nonnative_pcover) %>% 
  mutate(Cover_Category = "NON-NATIVE COVER") %>% 
  rename(Percent_Cover = Nonnative_pcover,
         PSOC = Nonnative_species)
  
#sum non-native cover

#old code for selecting columns
# parent[,c(2,3,5,6,8,13,14,15,24,7)]

mastersumNNC <- parent %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance, 
         Quadrat_Notes,Sum_nonnative_cover) %>% 
  mutate(PSOC = "Sum of Non-Native Cover",
         Cover_Category = "NON-NATIVE COVER") %>%
  rename(Percent_Cover = Sum_nonnative_cover)
  

### Other Cover ----
colnames(Othercover)

#old code for selecting columns
#othercover[,c(2,3,5,6,8,13,14,15,34,36,7)]

masterother <- Othercover %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance, 
         Quadrat_Notes, Othercover_type, p_othercover) %>% 
  mutate(Cover_Category = "OTHER COVER") %>% 
  rename(Percent_Cover = p_othercover,
         PSOC = Othercover_type)

#Add sum of other cover

#old selecting code
# mastersumother<-parent[,c(2,3,5,6,8,13,14,15,28,7)]

mastersumother <- parent %>% 
  select(Date, Monitors, Transect_Name, Site, Transect_length, Transect_Distance,
         Quadrat_Notes,Sum_Other) %>% 
  #new columns
  mutate(PSOC = "Sum of Other Cover",
         Cover_Category = "OTHER COVER") %>% 
  #rename column
  rename(Percent_Cover = Sum_Other)

## 2. Combine data frames ----

#R bind the above sheets (tidyverse function is bindrows())

df <- rbind(
  masterbareground,
  masterNatives,
  masterNonnatives,
  masterNaturalthatch,
  masterother,
  mastersumNC,
  mastersumNNC,
  mastersumother,
  mastersumthatch
) %>% 
#144,086 rows
## Add habitat column to the files
# grepl returns a logical vector (match pattern or not for each element of x)
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
    grepl(pattern = "SMT", x = Transect_Name) ~ "Transition - High Salt Marsh",
    grepl(pattern = "SMTH", x = Transect_Name) ~ "Transition - High Salt Marsh",
    grepl(pattern = "CSS", x = Transect_Name) ~ "Coastal Sage Scrub",
    grepl(pattern = "U", x = Transect_Name) ~ "Upland",
    grepl(pattern = "WL", x = Transect_Name) ~ "Wetland"
  )) 
  #now 144,442 rows

unique(df$Site)
#29 unique sites, no NA values

#Write processed data to file ----

#write to csv
write.csv(df,"processed_data/MasterQuadratdata_2026-02-05.csv")

#filter to just the Seep
seep<-df %>% 
  filter(Site == "harder_seep")
# 4,601 observations


#write processed data to file
write_csv(seep,"processed_data/Seep_2025.csv")

