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
          TRUE ~ Year),
         PSOC = case_when(
           grepl(pattern = "Aparagus asparagoides", x = PSOC) ~ "Asparagus_asparagoides",
           grepl(pattern = "Asparagus asparagoides", x = PSOC) ~ "Asparagus_asparagoides",
           TRUE ~ PSOC)
         ) %>%
  filter(Transect_Name != "U2W")
unique(veg$Year)
## FIX ME: aggregated totals too high
veg<-aggregate(`Percent_Cover`~`PSOC`+Year+Habitat+`Cover_Category`,veg,FUN = sum)
veg$`PSOC`<-gsub("_"," ",veg$`PSOC`)
unique(veg$`PSOC`)
veg<-veg %>% drop_na(`PSOC`)

  

#Natives -----
veg.nat<-veg[veg$`Cover_Category`=="NATIVE COVER",] %>%
  filter(PSOC!="Sum of Native Cover" & PSOC!="NA")
unique(veg.nat$`PSOC`)
veg.nat<-aggregate(Percent_Cover ~ Year+`PSOC`+Habitat, veg.nat, FUN=mean)
unique(veg.nat$`PSOC`)
print(veg.nat)

#Native Uplands ----
nat.ul<-veg.nat[veg.nat$Habitat=="Upland",]
nat.ul<-nat.ul[,c(1,2,5)]
nat.ul<-nat.ul %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
nat.ul[is.na(nat.ul)] <- 0
nat.ul

nat.ul.l<-nat.ul %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Percent_Cover")
nat.ul.l<-nat.ul.l[nat.ul.l$`PSOC`!="NA",] %>%
  mutate(Percent_Cover = round(Percent_Cover, 1))

nat.ul.l$Presence<-as.logical(nat.ul.l$Percent_Cover)
nat_ul_list <-ggplot(nat.ul.l, aes(x=Year, y=`PSOC`, fill=Percent_Cover)) +
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "darkseagreen1", "chartreuse4"),
                       values = scales::rescale(c(0,1,78)),
                       limits=c(0, 100)) +
  geom_text(data = nat.ul.l %>% filter(Percent_Cover > 0),
    aes(label = label_percent(accuracy = 1, scale = 1)(Percent_Cover)),
        color = "black", size = 3) +
  scale_y_discrete(limits = rev) +
  ggtitle("Native species recorded in upland habitat each year") + 
  labs(fill = "Average absolute \npercent cover") +
  ylab("Species")
nat_ul_list
ggsave("figures/Native_Plant_Presence_By_Year_Uplands.png", nat_ul_list, width=8 , 
       height=6 , units="in" , dpi=300)


#Native Wetlands ----
nat.wl<-veg.nat[veg.nat$Habitat=="Wetland",]
nat.wl<-nat.wl[,c(1,2,5)]
nat.wl<-nat.wl %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
nat.wl[is.na(nat.wl)] <- 0
nat.wl

nat.wl.l<-nat.wl %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Percent_Cover")
nat.wl.l<-nat.wl.l[nat.wl.l$`PSOC`!="NA",] %>%
  mutate(Percent_Cover = round(Percent_Cover, 1))

nat.wl.l$Presence<-as.logical(nat.wl.l$Percent_Cover)
nat_wl_list <-ggplot(nat.wl.l, aes(x=Year, y=`PSOC`, fill=Percent_Cover)) +
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "darkseagreen1", "chartreuse4"),
                       values = scales::rescale(c(0,1,90)),
                       limits=c(0, 100)) +
  geom_text(data = nat.wl.l %>% filter(Percent_Cover > 0),
            aes(label = label_percent(accuracy = 1, scale = 1)(Percent_Cover)),
            color = "black", size = 3) +
  scale_y_discrete(limits = rev) +
  ggtitle("Native species recorded in wetland habitat each year") + 
  labs(fill = "Average absolute \npercent cover") +
  ylab("Species")
nat_wl_list
ggsave("figures/Native_Plant_Presence_By_Year_Wetlands.png", nat_wl_list, width=8 , 
       height=6.5 , units="in" , dpi=300)


################################################################################################
################################################################################################
################################################################################################
#Nonnative Species ----

veg.NON<-veg[veg$`Cover_Category`=="NON-NATIVE COVER",] %>%
  filter(PSOC!="Sum of Non-Native Cover" & PSOC!="NA")
veg.NON<-aggregate(`Percent_Cover`~ Year+`PSOC`+Habitat+`Cover_Category`,veg.NON,FUN=mean)
unique(veg.NON$PSOC)
print(veg.NON)

#Non-native Uplands
NON.ul<-veg.NON[veg.NON$Habitat=="Upland",]
NON.ul<-NON.ul[,c(1,2,5)]
NON.ul<-NON.ul %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
NON.ul[is.na(NON.ul)] <- 0
NON.ul

NON.ul.l<-NON.ul %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Percent_Cover")
NON.ul.l<-NON.ul.l[NON.ul.l$`PSOC`!="NA",] %>%
  mutate(Percent_Cover = round(Percent_Cover, 1))

NON.ul.l$Presence<-as.logical(NON.ul.l$Percent_Cover)
NON_ul_list <-ggplot(NON.ul.l, aes(x=Year, y=`PSOC`, fill=Percent_Cover)) +
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "darkseagreen1", "chartreuse4"),
                       values = scales::rescale(c(0,1,78)),
                       limits=c(0, 100)) +
  geom_text(data = NON.ul.l %>% filter(Percent_Cover > 0),
            aes(label = label_percent(accuracy = 1, scale = 1)(Percent_Cover)),
            color = "black", size = 3) +
  scale_y_discrete(limits = rev) +
  ggtitle("Non-native species recorded in upland habitat each year") + 
  labs(fill = "Average absolute \npercent cover") +
  ylab("Species")
NON_ul_list
ggsave("figures/Nonnative_Plant_Presence_By_Year_Uplands.png", NON_ul_list, width=8 , 
       height=6 , units="in" , dpi=300)

# veg.NON.pu<-veg.NON[veg.NON$Habitat=="Peripheral Uplands",]
# veg.NON.pu<-veg.NON.pu[,c(1,2,4,5)]
# NON.pu<-veg.NON.pu %>%
#   pivot_wider(!`Cover_Category`,names_from = Year, values_from = `Percent_Cover`)
# NON.pu[is.na(NON.pu)] <- 0
# NON.pu
# NON.pu.l<-NON.pu %>% 
#   pivot_longer(!`PSOC`, names_to = "Year",values_to = "presence")
# NON.pu.l$presence[NON.pu.l$presence > 0.1] <- 1
# NON.pu.l<-NON.pu.l[NON.pu.l$`PSOC`!="NA",]
# 
# NON.pu.l$presence<-as.logical(NON.pu.l$presence)
# ggplot(NON.pu.l, aes(x=Year, y=`PSOC`, fill=presence)) + geom_tile()+ggtitle("Non-Native Peripheral Uplands Species")+theme_bw()
# 

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
