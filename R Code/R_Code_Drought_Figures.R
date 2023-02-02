### DROUGHT ANALYSIS FIGURE CODES #####

library(ggplot2)
library(ggsci)
library(Rmisc)
library(stringr)

#### FIGURE 1: Wet season rainfall at Kirindy Mitea over a 12 year period ####
rain1<-read.csv("Fig1_Data_Wet_Season_Rainfall.csv",header=T,stringsAsFactors = T,fileEncoding="UTF-8-BOM")

#Wet Season rainfall, with Percent Normal Precipitation based on average wet season rainfall
#at Kirindy Mitea (711.4 mm)
p<-ggplot(data=rain1, aes(x=as.factor(Year), y=Dec.May.Rainfall, fill=as.factor(Color.Code)))+
  geom_bar(stat="identity",position="dodge")+ theme_classic()
p+xlab("")+ylab("Wet Season Rainfall (mm)")+
  scale_fill_manual(values=c("gray25","gold1"))+theme(legend.position="none")+
  geom_hline(yintercept=462.4,linetype="dashed", color = "red")+
  geom_hline(yintercept=533.55,linetype="dashed", color = "gray50")+
  geom_hline(yintercept=889.25,linetype="dashed", color = "gray50")

#Annual Rainfall, with Percent Normal Precipitation based on 87 year average rainfall at
#Morondava weather station (767 mm)
p<-ggplot(data=rain1, aes(x=as.factor(Year), y=Annual.Rainfall, fill=as.factor(Color.Code)))+
  geom_bar(stat="identity",position="dodge")+ theme_classic()
p+xlab("")+ylab("Annual Season Rainfall (mm)")+
  scale_fill_manual(values=c("gray25","gold1"))+theme(legend.position="none")+
  geom_hline(yintercept=498.55,linetype="dashed", color = "red")+
  geom_hline(yintercept=575.25,linetype="dashed", color = "gray50")+
  geom_hline(yintercept=958.75,linetype="dashed", color = "gray50")



#### FIGURE 2: Total body fat for individual sifaka with before, during, and after drought measures ####
# Line plots of individual total body fat measures before and during drought (A)
# and during and after drought (C). Panel B is hand drawing of sifaka.

fat1<-read.csv("Fig2_Data_Individual_Total_Body_Fat.csv",header=T,stringsAsFactors = T,fileEncoding="UTF-8-BOM")
# Variables: individual ID, Year (pre, during, post drought)
# Repeat variable - color coding individuals that have measurements for panels A and C (black) vs only panel A (gold)
# 3 lemurs measured in 2 drought years, so drought measures used in before/during are different than those in during/after.
# In the dataset, these 3 individuals are indicated by the "SifakaXX" and "SifakaXX-B" IDs.

fat1A<-subset(fat1,Fig1=="Y")
fat1B<-subset(fat1,Fig2=="Y")

p<-ggplot(data=fat1A, aes(x=Year, y=Fat, group=Individual)) +
  geom_line(aes(color=Repeat)) +
  geom_point()+theme_classic() +
  theme(legend.position="none")
p+scale_x_discrete(labels = c('Before Drought','Drought'))+
  ylab(c("Total Body Fat"))+xlab(c(""))+scale_color_manual(values=c("goldenrod3","gray4"))+scale_y_continuous(limits=c(1,7),breaks = seq(1,7,0.5))

p<-ggplot(data=fat1B, aes(x=Year, y=Fat, group=Individual)) +
  geom_line(aes(color=Repeat)) +
  geom_point()+theme_classic() +
  theme(legend.position="none")+scale_color_manual(values=c("gray4"))
p+scale_x_discrete(labels = c('Drought','After Drought'))+
  ylab(c("Total Body Fat"))+xlab(c(""))+scale_y_continuous(limits=c(1,7),breaks = seq(1,7,0.5))



#### FIGURE 3: Effects of drought on reproductive success ####
# Barplots of reproductive output (A,B) and infant survival (C,D) 
# with standard deviation error bars.

##summary datasets##
# %reproductive output per year
ro<-read.csv("Fig3_Data_Reproductive_Output.csv",header=T,stringsAsFactors = T,fileEncoding="UTF-8-BOM")

# %infant survival per year
surv<-read.csv("Fig3_Data_Infant_Survival.csv",header=T,stringsAsFactors = T,fileEncoding="UTF-8-BOM")

###### Fig. 3A: Drought during Conception Year and Reproductive Output ######
conception.yr <- summarySE(ro, measurevar="Reproductive_Output", groupvars=c("Drought.Yr"))

p1<-ggplot(conception.yr,aes(x=Drought.Yr, y=Reproductive_Output,fill=Drought.Yr))+
  geom_bar(stat="identity",position="dodge") + geom_errorbar(aes(x=Drought.Yr, ymin=Reproductive_Output-sd, 
  ymax=Reproductive_Output+sd), width=0.1)+theme_classic()+ylim(0,100) + scale_x_discrete(labels=c('No Drought','Drought'))
                 
p1+ scale_fill_jama() + theme(legend.position="none") + 
  ylab("Average % of females reproducing per year") + xlab("")

###### Fig. 3B: Drought Year Prior to Conception and Reproductive Output ######
#remove 2011 because we don't have rainfall data/drought categorization for 2010
ro.2<-subset(ro,Year>2011) 

pre.conception.yr <- summarySE(ro.2, measurevar="Reproductive_Output", groupvars=c("Drought.Prior"), na.rm = TRUE)

p1<-ggplot(pre.conception.yr,aes(x=Drought.Prior, y=Reproductive_Output,fill=Drought.Prior))+
  geom_bar(stat="identity",position="dodge") + geom_errorbar(aes(x=Drought.Prior, ymin=Reproductive_Output-sd, 
                                                                 ymax=Reproductive_Output+sd), width=0.1)+theme_classic()+ylim(0,100) + scale_x_discrete(labels=c('No Drought','Drought'))
p1+ scale_fill_jama() + theme(legend.position="none") + 
  ylab("Average % of females reproducing per year") + xlab("")

###### Fig. 3C: Drought during Birth Year and Infant Survival ######
surv2<-subset(surv,Year>2010) 
birth.yr <- summarySE(surv2, measurevar="Infant_Survival", groupvars=c("Drought.Yr"))

p1<-ggplot(birth.yr,aes(x=Drought.Yr, y=Infant_Survival,fill=Drought.Yr))+
  geom_bar(stat="identity",position="dodge") + geom_errorbar(aes(x=Drought.Yr, ymin=Infant_Survival-sd, 
                                                                 ymax=Infant_Survival+sd), width=0.1)+theme_classic()+ylim(0,100) + scale_x_discrete(labels=c('No Drought','Drought'))
p1+ scale_fill_jama() + theme(legend.position="none") + 
  ylab("Average % of infants surviving per year") + xlab("")

###### Fig. 3D: Drought during Weaning Year and Infant Survival ######
wean.yr <- summarySE(surv, measurevar="Infant_Survival", groupvars=c("Drought.Wean"))

p1<-ggplot(wean.yr,aes(x=Drought.Wean, y=Infant_Survival,fill=Drought.Wean))+
  geom_bar(stat="identity",position="dodge") + geom_errorbar(aes(x=Drought.Wean, ymin=Infant_Survival-sd, 
                                                                 ymax=Infant_Survival+sd), width=0.1)+theme_classic()+ylim(0,100) + scale_x_discrete(labels=c('No Drought','Drought'))
p1+ scale_fill_jama() + theme(legend.position="none") + 
  ylab("Average % of infants surviving per year") + xlab("")



#### FIGURE 4: Relative abundance of young and mature leaves ####
### barplots with relative abundance of food items by month for drought and non-drought conditions

phen1<-read.csv("Fig4_Data_Relative_Phenology_Abundance-Leaves.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

#set order of relative abundance rank (0, 1, 2, 3, 4)
phen1$Rank<-factor(phen1$Category,order=TRUE,levels=c("No.0","No.1","No.2","No.3","No.4"))

#organize months in chronological order
phen1$Months <- factor(phen1$Month, levels = c("December", "January", "February", "March", "April", "May"))

phen1$newDrought<-str_wrap(phen1$Drought.Category, width = 7)

######Top panel: young leaves ######
young.leaves<-subset(phen1,Product=="Young.Leaves")

p<-ggplot(young.leaves, aes(x = newDrought, y=Proportion, fill = Rank)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Months, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())
p+ylab("Proportion of Trees")+xlab("")


######Bottom panel: mature leaves ######
mature.leaves<-subset(phen1,Product=="Mature.Leaves")

p<-ggplot(mature.leaves, aes(x = newDrought, y=Proportion, fill = Rank)) +
  geom_bar(position="fill",stat = "identity") + facet_wrap(~Months, nrow = 1)+
  theme_classic()+theme(strip.background = element_blank())
p+ylab("Proportion of Trees")+xlab("")
