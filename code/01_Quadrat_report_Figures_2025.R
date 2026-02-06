getwd()
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
vegNNN<-veg[veg$Cover_Category=="NATIVE COVER"|veg$Cover_Category=="NON-NATIVE COVER",]
unique(vegNNN$Cover_Category)

vegNNN<-vegNNN[vegNNN$PSOC!="Sum of Native Cover" & vegNNN$PSOC!="Sum of Non-Native Cover",]
vegNNN<-aggregate(Percent_Cover~Habitat+Year+Cover_Category+Transect_Distance+
                  Transect_Name,vegNNN,FUN=sum)

vegNNNagg<-aggregate(Percent_Cover~Habitat+Year+Cover_Category,vegNNN,FUN=mean)
unique(vegNNNagg$Habitat)

vegNNNagg$Percent_Cover<-as.character(vegNNNagg$Percent_Cover)
vegNNNagg$Percent_Cover<-as.numeric(vegNNNagg$Percent_Cover)
unique(vegNNNagg$Percent_Cover)

#Absolute
for (i in unique(vegNNNagg$Habitat)){
  p<-vegNNNagg[vegNNNagg$Habitat==i,]
  q<- ggplot(p,aes(Year,Percent_Cover,fill=Cover_Category)) +
    geom_bar(position="dodge",stat="identity", color="black")+
    ggtitle(p$Habitat, "Average Absolute Percent Cover")+ theme_bw()+ylim(0,170)+
    theme(axis.text.x = element_text(angle = 90))+ theme(axis.text.x = element_text(angle = 0, size = 14),
                                                         axis.text.y = element_text(size=14),
                                                         axis.title.y = element_text(size=14,margin = margin(t = 0, r = 10, b = 0, l = 20)),
                                                         axis.title.x=element_text(size=14,margin = margin(t = 0, r = 10, b = 0, l = 20)),
                                                         title =element_text(size=15, face='bold'))+
    scale_fill_manual("Cover Type", values = c("NATIVE COVER" = "darkgreen", "NON-NATIVE COVER" = "red"))
  ggsave(filename=paste("figures/Absolute_NNN_", i, ".jpg", sep="") , plot=q , width=8 , height=6 , units="in" , dpi=300)
  print(q)
}




#Relative
for (i in unique(vegNNNagg$Habitat)){
  p<-vegNNNagg[vegNNNagg$Habitat==i,]
  q<- ggplot(p,aes(Year,Percent_Cover,fill=Cover_Category)) +
    geom_bar(position="fill",stat="identity", color="black")+theme_bw()+
    ggtitle(p$Habitat, "Average Relative Percent Cover")+ theme(axis.text.x = element_text(angle = 0, size = 14),
                              axis.text.y = element_text(size=14),
                              axis.title.y = element_text(size=14,margin = margin(t = 0, r = 10, b = 0, l = 20)),
                              axis.title.x=element_text(size=14,margin = margin(t = 0, r = 10, b = 0, l = 20)),
                              title =element_text(size=15, face='bold'))+ 
    scale_fill_manual("Cover Type", values = c("NATIVE COVER" = "darkgreen", "NON-NATIVE COVER" = "red", "BARE GROUND" = "tan", "OTHER COVER" = "burlywood4", "THATCH COVER" = "grey"))
  print(q)
  ggsave(filename=paste("figures/Relative_cover_", i, ".jpg", sep="") , plot=q , width=12 , height=6 , units="in" , dpi=300)
}




#Relative including bare ground and thatch
unique(veg$Habitat)
veg<- within(veg, Habitat[Habitat == "Vernal Pool"] <- 'Vernal Pools')
vegsums<-veg[veg$PSOC=="Bare Ground"|veg$PSOC=="Sum of Thatch Cover"|
               veg$PSOC=="Sum of Non-Native Cover"|veg$PSOC=="Sum of Native Cover"|
               veg$PSOC=="Sum of Other Cover",]
unique(vegsums$PSOC)
vegsumsagg<-aggregate(Percent_Cover~Cover_Category+Habitat+Year,vegsums,FUN=mean)

for (i in unique(vegsumsagg$Habitat)){
  p<-vegsumsagg[vegsumsagg$Habitat==i,]
  q<- ggplot(p,aes(Year, Percent_Cover, fill=Cover_Category)) +
    geom_bar(position="fill",stat="identity", color="black") + 
    theme_bw() +
    ggtitle(p$Habitat, "Average Relative Percent Cover")+ theme(axis.text.x = element_text(angle = 0, size = 14),
                              axis.text.y = element_text(size=14),
                              axis.title.y = element_text(
                                size=14,
                                margin = margin(t = 0, r = 10, b = 0, l = 20)),
                              axis.title.x=element_text(
                                size=14,
                                margin = margin(t = 0, r = 10, b = 0, l = 20)),
                              title =element_text(size=15, face='bold')) + 
    scale_fill_manual("Cover Type", 
                      values = c("NATIVE COVER" = "darkgreen", 
                                 "NON-NATIVE COVER" = "red", 
                                 "BARE GROUND" = "tan", 
                                 "OTHER COVER" = "burlywood4", 
                                 "THATCH" = "grey"))
  print(q)
  ggsave(filename=paste("figures/Relative_allcover_", i, ".jpg", sep="") , plot=q , width=12 , height=6 , units="in" , dpi=300)
}



### for vegetation table

veg$PSOC <- sub(" ", "_", veg$PSOC)
vegNative<-veg[veg$Cover_Category=="NATIVE COVER"&veg$PSOC!="Sum_of Native Cover",]

#NATIVE ONLY
vegNative<- within(vegNative, PSOC[PSOC == "Cardamine_olidosperna"] <- 'Cardamine_oligosperma')
vegNative<- within(vegNative, PSOC[PSOC == "Sparganium_Eurycarpum"] <- 'Sparganium_eurycarpum')
vegNative<- within(vegNative, PSOC[PSOC == "Spergula_marina"] <- 'Spergularia_marina')
vegNative

# check for misspellings
unique_veg <- vegNative[!duplicated(vegNative$PSOC), ]
sorted_unique_customers <- unique_veg[order(unique_veg$PSOC), ]
print(sorted_unique_customers)

vegNativespecies<-aggregate(Percent_Cover~PSOC+Year+Habitat,vegNative,FUN=mean)
vegNativespecies$count<-1
vegNativespecies
vegNativeDiversity<-aggregate(count~Habitat+Year,vegNativespecies,FUN=sum)
vegNativeDiversity

nat25 = vegNativespecies[vegNativespecies$Year=="2025",]
nat25

#ALL VEG
# check for misspellings of sum of native and nonnative
unique_veg <- veg[!duplicated(veg$PSOC), ]
sorted_unique_customers <- unique_veg[order(unique_veg$PSOC), ]
sorted_unique_customers[220:250,]

vegonly <- veg[veg$PSOC=="Sum_of_Native_Cover"|veg$PSOC=="Sum_of_Non-Native_Cover",]
vegonlyall <- aggregate(Percent_Cover~Habitat+Transect_Name+Transect_Distance+Year,vegonly,FUN=sum)
vegonlyallmean <- aggregate(Percent_Cover~Habitat+Year,vegonlyall,FUN=mean)


#2025 Native Species Cover

