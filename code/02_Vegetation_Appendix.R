getwd()
#install.packages("reshape2")
#library(reshape2)
veg<-read_csv("processed_data/Seep_2025.csv")
veg$Date<-as.Date(veg$Date,format<-"%m/%d/%Y")
veg$Year<-format(veg$Date,format<-"%Y")
print(veg)
colnames(veg)
start_date <- as.Date("2025-01-01")
end_date <- as.Date("2025-02-24")
veg <- veg %>%
  mutate(Year = case_when(
    between(Date, start_date, end_date) ~ "2024",
    TRUE ~ Year
  )) %>%
  filter(Transect_Name != "U2W")
unique(veg$Year)
veg<-aggregate(`Percent_Cover`~`PSOC`+Year+Habitat+`Cover_Category`,veg,FUN = sum)
veg$`PSOC`<-gsub("_"," ",veg$`PSOC`)
unique(veg$`PSOC`)
veg<-veg %>% drop_na(`PSOC`)

  

#Natives
veg.nat<-veg[veg$`Cover_Category`=="NATIVE COVER",]
veg.nat<-veg.nat[veg.nat$`PSOC`!="Sum of Native Cover"&veg.nat$`PSOC`!="",]
unique(veg.nat$`PSOC`)
veg.nat<- within(veg.nat, PSOC[PSOC == "Cardamine_olidosperna"] <- 'Cardamine_oligosperma')
veg.nat<- within(veg.nat, PSOC[PSOC == "Sparganium_Eurycarpum"] <- 'Sparganium_eurycarpum')
veg.nat<- within(veg.nat, PSOC[PSOC == "Spergula_marina"] <- 'Spergularia_marina')
veg.nat<-aggregate(`Percent_Cover`~Year+`PSOC`+Habitat+`Cover_Category`,veg.nat,FUN=sum)
unique(veg.nat$`PSOC`)
print(veg.nat)

#Native Uplands
nat.ul<-veg.nat[veg.nat$Habitat=="Upland",]
nat.ul<-nat.ul[,c(1,2,5)]
nat.ul<-nat.ul %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
nat.ul[is.na(nat.ul)] <- 0
nat.ul

nat.ul.l<-nat.ul %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Presence")
nat.ul.l<-nat.ul.l[nat.ul.l$`PSOC`!="NA",]

nat.ul.l$Presence<-as.logical(nat.ul.l$Presence)
ggplot(nat.ul.l, aes(x=Year, y=`PSOC`, fill=Presence)) + geom_tile() +
  scale_y_discrete(limits = rev) +
  ggtitle("Native species recorded in \nupland habitat each year") + 
  ylab("Species") + theme_bw()

#Native Wetplands
nat.wl<-veg.nat[veg.nat$Habitat=="Wetland",]
nat.wl<-nat.wl[,c(1,2,5)]
nat.wl<-nat.wl %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
nat.wl[is.na(nat.wl)] <- 0
nat.wl

nat.wl.l<-nat.wl %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Presence")
nat.wl.l<-nat.wl.l[nat.wl.l$`PSOC`!="NA",]

nat.wl.l$Presence<-as.logical(nat.wl.l$Presence)
nat_list <- ggplot(nat.wl.l, aes(x=Year, y=`PSOC`, fill=Presence))+
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_y_discrete(limits = rev) +
  ggtitle("Native species recorded in \nupland habitat each year") + 
  ylab("Species")
nat_list
ggsave("figures/Native_Plant_Presence_By_Year.png", nat_list, width=8 , 
       height=6 , units="in" , dpi=300)

unique(veg$Habitat)
#Native Peripheral Uplands
veg.nat.pg<-veg.nat[veg.nat$Habitat=="Perennial Grassland" ,]
veg.nat.pg<-veg.nat.pg[,c(1,2,4,5)]
nat.pg<-veg.nat.pg %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.pg[is.na(nat.pg)] <- 0
nat.pg
nat.pg.l<-nat.pg %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.pg.l$presence[nat.pg.l$presence > 0.1] <- 1
nat.pg.l<-nat.pg.l[nat.pg.l$`PSOC`!="NA",]

nat.pg.l$presence<-as.logical(nat.pg.l$presence)
ggplot(nat.pg.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Perennial Grassland Species")+theme_bw()


unique(veg$Habitat)
#Native "Remnant Salt Marsh"
veg.nat.RSM<-veg.nat[veg.nat$Habitat=="Remnant Salt Marsh" ,]
veg.nat.RSM<-veg.nat.RSM[,c(1,2,4,5)]
str(veg.nat.pg)
unique(veg.nat.RSM$`Percent_Cover`)
str(veg.nat.RSM)
nat.RSM<-veg.nat.RSM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.RSM[is.na(nat.RSM)] <- 0
nat.RSM.l<-nat.RSM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.RSM.l$presence[nat.RSM.l$presence > 0.1] <- 1
nat.RSM.l<-nat.RSM.l[nat.RSM.l$`PSOC`!="NA",]

nat.RSM.l$presence<-as.logical(nat.RSM.l$presence)
ggplot(nat.RSM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Remnant Salt Marsh Species")+theme_bw()


unique(veg$Habitat)
#Native "Salt Marsh" 
veg.nat.SM<-veg.nat[veg.nat$Habitat=="Salt Marsh" ,]
veg.nat.SM<-veg.nat.SM[,c(1,2,4,5)]
nat.SM<-veg.nat.SM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.SM[is.na(nat.SM)] <- 0
nat.SM
nat.SM.l<-nat.SM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.SM.l$presence[nat.SM.l$presence > 0.1] <- 1
nat.SM.l<-nat.SM.l[nat.SM.l$`PSOC`!="NA",]

nat.SM.l$presence<-as.logical(nat.SM.l$presence)
ggplot(nat.SM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Salt Marsh Species")+theme_bw()


unique(veg$Habitat)
#Native "Sand Flat"
veg.nat.SF<-veg.nat[veg.nat$Habitat=="Sand Flat" ,]
veg.nat.SF<-veg.nat.SF[,c(1,2,4,5)]
nat.SF<-veg.nat.SF %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.SF[is.na(nat.SF)] <- 0
nat.SF
nat.SF.l<-nat.SF %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.SF.l$presence[nat.SF.l$presence > 0.1] <- 1
nat.SF.l<-nat.SF.l[nat.SF.l$`PSOC`!="NA",]

nat.SF.l$presence<-as.logical(nat.SF.l$presence)
ggplot(nat.SF.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Sand Flat Species")+theme_bw()


unique(veg$Habitat)
#Native "Sandy Annuals" 
veg.nat.SA<-veg.nat[veg.nat$Habitat=="Sandy Annuals"  ,]
veg.nat.SA<-veg.nat.SA[,c(1,2,4,5)]
nat.SA<-veg.nat.SA %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.SA[is.na(nat.SA)] <- 0
nat.SA
nat.SA.l<-nat.SA %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.SA.l$presence[nat.SA.l$presence > 0.1] <- 1
nat.SA.l<-nat.SA.l[nat.SA.l$`PSOC`!="NA",]

nat.SA.l$presence<-as.logical(nat.SA.l$presence)
ggplot(nat.SA.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Sandy Annuals Species")+theme_bw()


unique(veg$Habitat)
#Native "Seasonal Brackish Marsh" 
veg.nat.SBM<-veg.nat[veg.nat$Habitat=="Seasonal Brackish Marsh"  ,]
veg.nat.SBM<-veg.nat.SBM[,c(1,2,4,5)]
nat.SBM<-veg.nat.SBM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.SBM[is.na(nat.SBM)] <- 0
nat.SBM
nat.SBM.l<-nat.SBM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.SBM.l$presence[nat.SBM.l$presence > 0.1] <- 1
nat.SBM.l<-nat.SBM.l[nat.SBM.l$`PSOC`!="NA",]

nat.SBM.l$presence<-as.logical(nat.SBM.l$presence)
ggplot(nat.SBM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Seasonal Brackish Marsh Species")+theme_bw()


unique(veg$Habitat)
#Native "Seasonal Freshwater Pond"
veg.nat.SFP<-veg.nat[veg.nat$Habitat=="Seasonal Freshwater Pond"  ,]
veg.nat.SFP<-veg.nat.SFP[,c(1,2,4,5)]
nat.SFP<-veg.nat.SFP %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.SFP[is.na(nat.SFP)] <- 0
nat.SFP
nat.SFP.l<-nat.SFP %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.SFP.l$presence[nat.SFP.l$presence > 0.1] <- 1
nat.SFP.l<-nat.SFP.l[nat.SFP.l$`PSOC`!="NA",]

nat.SFP.l$presence<-as.logical(nat.SFP.l$presence)
ggplot(nat.SFP.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Seasonal Freshwater Pond Species")+theme_bw()

unique(veg$Habitat)
#Native "Transition/High Salt Marsh"
veg.nat.THSM<-veg.nat[veg.nat$Habitat=="Transition/High Salt Marsh"  ,]
veg.nat.THSM<-veg.nat.THSM[,c(1,2,4,5)]
nat.THSM<-veg.nat.THSM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.THSM[is.na(nat.THSM)] <- 0
nat.THSM
nat.THSM.l<-nat.THSM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.THSM.l$presence[nat.THSM.l$presence > 0.1] <- 1
nat.THSM.l<-nat.THSM.l[nat.THSM.l$`PSOC`!="NA",]

nat.THSM.l$presence<-as.logical(nat.THSM.l$presence)
ggplot(nat.THSM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Transition/High Salt Marsh Species")+theme_bw()

unique(veg$Habitat)
#Native "Vernal Pools"
veg.nat.VP<-veg.nat[veg.nat$Habitat=="Vernal Pools"  ,]
veg.nat.VP<-veg.nat.VP[,c(1,2,4,5)]
nat.VP<-veg.nat.VP %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
nat.VP[is.na(nat.VP)] <- 0
nat.VP
nat.VP.l<-nat.VP %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
nat.VP.l$presence[nat.VP.l$presence > 0.1] <- 1
nat.VP.l<-nat.VP.l[nat.VP.l$`PSOC`!="NA",]

nat.VP.l$presence<-as.logical(nat.VP.l$presence)
ggplot(nat.VP.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Native Vernal Pool Species")+theme_bw()



################################################################################################
################################################################################################
################################################################################################
#Nonnative Species
veg.NON<-veg[veg$`Cover_Category`=="NON-NATIVE COVER",]
veg.NON<-veg.NON[veg.NON$`PSOC`!="Sum of Non-Native Cover"&veg.NON$`PSOC`!="NA",]
veg.NON$`PSOC`<-gsub("sp.","",veg.NON$`PSOC`)
veg.NON$`PSOC`<-veg.NON[veg.NON$`PSOC`!="unlisted",]
unique(veg.NON$`PSOC`)
veg.NON<-aggregate(`Percent_Cover`~Year+`PSOC`+Habitat+`Cover_Category`,veg.NON,FUN=sum)

#Native Peripheral Uplands
veg.NON.pu<-veg.NON[veg.NON$Habitat=="Peripheral Uplands",]
veg.NON.pu<-veg.NON.pu[,c(1,2,4,5)]
NON.pu<-veg.NON.pu %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.pu[is.na(NON.pu)] <- 0
NON.pu
NON.pu.l<-NON.pu %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.pu.l$presence[NON.pu.l$presence > 0.1] <- 1
NON.pu.l<-NON.pu.l[NON.pu.l$`PSOC`!="NA",]

NON.pu.l$presence<-as.logical(NON.pu.l$presence)
ggplot(NON.pu.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Peripheral Uplands Species")+theme_bw()


unique(veg$Habitat)
#NONive perennial grassland
veg.NON.pg<-veg.NON[veg.NON$Habitat=="Perennial Grassland" ,]
veg.NON.pg<-veg.NON.pg[,c(1,2,4,5)]
NON.pg<-veg.NON.pg %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.pg[is.na(NON.pg)] <- 0
NON.pg
NON.pg.l<-NON.pg %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.pg.l$presence[NON.pg.l$presence > 0.1] <- 1
NON.pg.l<-NON.pg.l[NON.pg.l$`PSOC`!="NA",]

NON.pg.l$presence<-as.logical(NON.pg.l$presence)
ggplot(NON.pg.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Perennial Grassland Species")+theme_bw()


unique(veg$Habitat)
#NONive "Remnant Salt Marsh"
veg.NON.RSM<-veg.NON[veg.NON$Habitat=="Remnant Salt Marsh" ,]
veg.NON.RSM<-veg.NON.RSM[,c(1,2,4,5)]
NON.RSM<-veg.NON.RSM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.RSM[is.na(NON.RSM)] <- 0
NON.RSM
NON.RSM.l<-NON.RSM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.RSM.l$presence[NON.RSM.l$presence > 0.1] <- 1
NON.RSM.l<-NON.RSM.l[NON.RSM.l$`PSOC`!="NA",]

NON.RSM.l$presence<-as.logical(NON.RSM.l$presence)
ggplot(NON.RSM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Remnant Salt Marsh Species")+theme_bw()


unique(veg$Habitat)
#NONive "Salt Marsh" 
veg.NON.SM<-veg.NON[veg.NON$Habitat=="Salt Marsh" ,]
veg.NON.SM<-veg.NON.SM[,c(1,2,4,5)]
NON.SM<-veg.NON.SM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.SM[is.na(NON.SM)] <- 0
NON.SM
NON.SM.l<-NON.SM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.SM.l$presence[NON.SM.l$presence > 0.1] <- 1
NON.SM.l<-NON.SM.l[NON.SM.l$`PSOC`!="NA",]

NON.SM.l$presence<-as.logical(NON.SM.l$presence)
ggplot(NON.SM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Salt Marsh Species")+theme_bw()


unique(veg$Habitat)
#NONive "Sand Flat"
veg.NON.SF<-veg.NON[veg.NON$Habitat=="Sand Flat" ,]
veg.NON.SF<-veg.NON.SF[,c(1,2,4,5)]
NON.SF<-veg.NON.SF %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.SF[is.na(NON.SF)] <- 0
NON.SF
NON.SF.l<-NON.SF %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.SF.l$presence[NON.SF.l$presence > 0.1] <- 1
NON.SF.l<-NON.SF.l[NON.SF.l$`PSOC`!="NA",]

NON.SF.l$presence<-as.logical(NON.SF.l$presence)
ggplot(NON.SF.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Sand Flat Species")+theme_bw()


unique(veg$Habitat)
#NONive "Sandy Annuals" 
veg.NON.SA<-veg.NON[veg.NON$Habitat=="Sandy Annuals"  ,]
veg.NON.SA<-veg.NON.SA[,c(1,2,4,5)]
NON.SA<-veg.NON.SA %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.SA[is.na(NON.SA)] <- 0
NON.SA
NON.SA.l<-NON.SA %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.SA.l$presence[NON.SA.l$presence > 0.1] <- 1
NON.SA.l<-NON.SA.l[NON.SA.l$`PSOC`!="NA",]

NON.SA.l$presence<-as.logical(NON.SA.l$presence)
ggplot(NON.SA.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Sandy Annuals Species")+theme_bw()


unique(veg$Habitat)
#NONive "Seasonal Brackish Marsh" 
veg.NON.SBM<-veg.NON[veg.NON$Habitat=="Seasonal Brackish Marsh"  ,]
veg.NON.SBM<-veg.NON.SBM[,c(1,2,4,5)]
NON.SBM<-veg.NON.SBM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.SBM[is.na(NON.SBM)] <- 0
NON.SBM
NON.SBM.l<-NON.SBM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.SBM.l$presence[NON.SBM.l$presence > 0.1] <- 1
NON.SBM.l<-NON.SBM.l[NON.SBM.l$`PSOC`!="NA",]

NON.SBM.l$presence<-as.logical(NON.SBM.l$presence)
ggplot(NON.SBM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Seasonal Brackish Marsh Species")+theme_bw()


unique(veg$Habitat)
#NONive "Seasonal Freshwater Pond"
veg.NON.SFP<-veg.NON[veg.NON$Habitat=="Seasonal Freshwater Pond"  ,]
veg.NON.SFP<-veg.NON.SFP[,c(1,2,4,5)]
NON.SFP<-veg.NON.SFP %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.SFP[is.na(NON.SFP)] <- 0
NON.SFP
NON.SFP.l<-NON.SFP %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.SFP.l$presence[NON.SFP.l$presence > 0.1] <- 1
NON.SFP.l<-NON.SFP.l[NON.SFP.l$`PSOC`!="NA",]

NON.SFP.l$presence<-as.logical(NON.SFP.l$presence)
ggplot(NON.SFP.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Seasonal Freshwater Pond Species")+theme_bw()

unique(veg$Habitat)
#NONive "Transition/High Salt Marsh"
veg.NON.THSM<-veg.NON[veg.NON$Habitat=="Transition/High Salt Marsh"  ,]
veg.NON.THSM<-veg.NON.THSM[,c(1,2,4,5)]
NON.THSM<-veg.NON.THSM %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.THSM[is.na(NON.THSM)] <- 0
NON.THSM
NON.THSM.l<-NON.THSM %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.THSM.l$presence[NON.THSM.l$presence > 0.1] <- 1
NON.THSM.l<-NON.THSM.l[NON.THSM.l$`PSOC`!="NA",]

NON.THSM.l$presence<-as.logical(NON.THSM.l$presence)
ggplot(NON.THSM.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Transition/High Salt Marsh Species")+theme_bw()

unique(veg$Habitat)
#NONive "Vernal Pools"
veg.NON.VP<-veg.NON[veg.NON$Habitat=="Vernal Pools"  ,]
veg.NON.VP<-veg.NON.VP[,c(1,2,4,5)]
NON.VP<-veg.NON.VP %>%
  pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
NON.VP[is.na(NON.VP)] <- 0
NON.VP
NON.VP.l<-NON.VP %>% 
  pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
NON.VP.l$presence[NON.VP.l$presence > 0.1] <- 1
NON.VP.l<-NON.VP.l[NON.VP.l$`PSOC`!="NA",]

NON.VP.l$presence<-as.logical(NON.VP.l$presence)
ggplot(NON.VP.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Vernal Pool Species")+theme_bw()
