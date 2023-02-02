#### SIFAKA DROUGHT ANALYSIS CODE #####

setwd("~/Dropbox/Kirindy_Working_Files_carrie/Manuscripts/Drought&Sifaka/Data/CleanedData_For Publication")

library(lme4)
library(lmerTest)
library(sjPlot)

#### I: BODY CONDITION ####
## body condition data (body mass, BMI, fat measures) by animal ID, age class, sex, sampling year, and drought year
data1<-read.csv("Body_Condition_Data_2023.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

###### A. Total Body Fat Models ######
fat.m1<-lmer(Total.Fat~Sex+(1|Lemur)+(1|Year),data=data1)
fat.m2<-lmer(Total.Fat~AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m3<-lmer(Total.Fat~Sex+Pregnant+(1|Lemur)+(1|Year),data=data1)
fat.m4<-lmer(Total.Fat~Drought+(1|Lemur)+(1|Year),data=data1)
fat.m5<-lmer(Total.Fat~Sex+Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m6<-lmer(Total.Fat~Sex+Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m7<-lmer(Total.Fat~Sex+Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m8<-lmer(Total.Fat~Sex+Drought*AgeClass+Pregnant+(1|Lemur)+(1|Year),data=data1)
fat.null<-lmer(Total.Fat~1+(1|Lemur)+(1|Year),data=data1)

## calculate AIC for fat models
AIC(fat.m1,fat.m2,fat.m4,fat.m3,fat.m5,fat.m6,fat.m7,fat.m8,fat.null)

##Perform models for separate fat measures (insert into model): 
#Fat.back #Fat.belly #Fat.arm #Fat.hip
fat.m1<-lmer(Fat.back~Sex+(1|Lemur)+(1|Year),data=data1)
fat.m2<-lmer(Fat.back~AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m4<-lmer(Fat.back~Sex+Pregnant+(1|Lemur)+(1|Year),data=data1)
fat.m3<-lmer(Fat.back~Drought+(1|Lemur)+(1|Year),data=data1)
fat.m5<-lmer(Fat.back~Sex+Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
fat.m6<-lmer(Fat.back~Sex+Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m7<-lmer(Fat.back~Sex+Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
fat.m8<-lmer(Fat.back~Sex+Drought*AgeClass+Pregnant+(1|Lemur)+(1|Year),data=data1)
fat.null<-lmer(Fat.back~1+(1|Lemur)+(1|Year),data=data1)

##AIC for separate fat measures
AIC(fat.m1,fat.m2,fat.m3,fat.m4,fat.m5,fat.m6,fat.m7,fat.m8,fat.null)

###### B. Body Mass Index (BMI) Models ######
bmi.m1<-lmer(BMI~Sex+(1|Lemur)+(1|Year),data=data1)
bmi.m2<-lmer(BMI~AgeClass+(1|Lemur)+(1|Year),data=data1)
bmi.m3<-lmer(BMI~Sex+Pregnant+(1|Lemur)+(1|Year),data=data1)
bmi.m4<-lmer(BMI~Drought+(1|Lemur)+(1|Year),data=data1)
bmi.m5<-lmer(BMI~Sex+Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
bmi.m6<-lmer(BMI~Sex+Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
bmi.m7<-lmer(BMI~Sex+Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
bmi.m8<-lmer(BMI~Sex*Drought+Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
bmi.null<-lmer(BMI~1+(1|Lemur)+(1|Year),data=data1)

## calculate AIC for BMI models
AIC(bmi.m1,bmi.m2,bmi.m4,bmi.m3,bmi.m5,bmi.m6,bmi.m7,bmi.m8, bmi.null)


###### C. Body mass models ###### 
mass.m1<-lmer(Weight~Sex+(1|Lemur)+(1|Year),data=data1)
mass.m2<-lmer(Weight~AgeClass+(1|Lemur)+(1|Year),data=data1)
mass.m3<-lmer(Weight~Sex+Pregnant+(1|Lemur)+(1|Year),data=data1)
mass.m4<-lmer(Weight~Drought+(1|Lemur)+(1|Year),data=data1)
mass.m5<-lmer(Weight~Sex+Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
mass.m6<-lmer(Weight~Sex+Pregnant+AgeClass+Drought+(1|Lemur)+(1|Year),data=data1)
mass.m7<-lmer(Weight~Sex+Pregnant+Drought+(1|Lemur)+(1|Year),data=data1)
mass.m8<-lmer(Weight~Sex*Drought+Pregnant+AgeClass+(1|Lemur)+(1|Year),data=data1)
mass.null<-lmer(Weight~1+(1|Lemur)+(1|Year),data=data1)

## calculate AIC for mass
AIC(mass.m1,mass.m2,mass.m4,mass.m3,mass.m5,mass.m6,mass.m7,mass.m8,mass.null)


#### II. MEASURES OF REPRODUCTIVE SUCCESS####

##### A. Infant Survival #####
data2<-read.csv("Births&Survival-9-16-22.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

#recode survival?
data2$survival[data2$X1Yr=="N"] <-0
data2$survival[data2$X1Yr=="Y"] <-1

######i. Does drought during weaning year affect infant survival? ######
#remove any infants born before 2010 (drought data from 2011-2022, 2010 infants wean in 2011)
data2<-subset(data2,Year>2009)

#Note: MomID had very little variance and caused singular fit warning, so removed from these two models. 
s.m1<-glmer(survival~Drought.WeanYr+(1|Year),data=data2, family=binomial(link = "logit"),na=na.exclude)
s.nullA<-glmer(survival~1+(1|Year),data=data2, family=binomial(link = "logit"),na=na.exclude)

#use likelihood ratio tests (LRTs) to compare test and null models. 
anova(s.m1,s.nullA)

######ii. Does drought during birth year affect infant survival? ######

#remove any infants born before 2011 (no rainfall data for 2010 wet season)
data2B<-subset(data2,Year>2010)

#Note: MomID had very little variance and caused singular fit warning, so removed from these two models.
# Year had 0 variance for s.m2 model but kept it because was larger in null.

s.m2<-glmer(survival~Drought.BirthYr+(1|Year),data=data2B, family=binomial(link = "logit"),na=na.exclude)
s.nullB<-glmer(survival~1+(1|Year),data=data2B, family=binomial(link = "logit"),na=na.exclude)

# LRT:
anova(s.m2,s.nullB)


######iii. Does drought in the year prior to conception affect infant survival? ######

#remove any infants born before 2012 (year prior to conception = 2011)
data2C<-subset(data2,Year>2011)

#Note: MomID had very little variance in s.m3 model (singular fit warning) but
#was in s.nullC model, so we kept in both.
s.m3<-glmer(survival~Drought.PreconceptionYr+(1|MomID)+(1|Year),data=data2C, family=binomial(link = "logit"),na.action=na.exclude)
s.nullC<-glmer(survival~1+(1|MomID)+(1|Year),data=data2C, family=binomial(link = "logit"),na.action=na.exclude)

#LRT:
anova(s.m3,s.nullC)


##### B. Reproductive Output #####
data3<-read.csv("Birth_Data_1-31-23.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

#remove juveniles so only subadults + adults (data3) or adults only (data3B)
data3B<-subset(data3,AgeClass=="adult")


###### i. Does drought during conception year affect reproductive output? ######

#adults + subadults models 
conception.m1<-glmer(Pregnant~Conception.Drought+AgeClass+(1|Female)+(1|Year),
                     data=data3, family=binomial(link = "logit"),na.action =na.exclude)
conception.null1<-glmer(Pregnant~AgeClass+(1|Female)+(1|Year),
                        data=data3, family=binomial(link = "logit"),na=na.exclude)
#LRT:
anova(conception.m1,conception.null1)

#adults only models
conception.m2<-glmer(Pregnant~Conception.Drought+(1|Female)+(1|Year),
                     data=data3B, family=binomial(link = "logit"),na.action =na.exclude)
conception.null2<-glmer(Pregnant~1+(1|Female)+(1|Year),data=data3B, 
                        family=binomial(link = "logit"),na=na.exclude)
#LRT:
anova(conception.m2,conception.null2)

###### ii. Does drought in the year prior to conception affect reproductive output? ######
#remove any infants born before 2012 (year prior to conception = 2011)
data3C<-subset(data3,Year>2011)
data3D<-subset(data3B,Year>2011)

#adults + subadults models
#model failed to converge with default optimizer, switched to bobyqa
output.m1<-glmer(Pregnant~Pre.ConceptionYr.Drought+AgeClass+(1|Female)+(1|Year),
                 data=data3C, family=binomial(link = "logit"),
                 control=glmerControl(optimizer="bobyqa"),na.action = na.exclude)
output.null1<-glmer(Pregnant~AgeClass+(1|Female)+(1|Year),data=data3C, 
                    control=glmerControl(optimizer="bobyqa"),
                family=binomial(link = "logit"),na=na.exclude)
#LRT:
anova(output.m1,output.null1)

#adults only models
output.m2<-glmer(Pregnant~Pre.ConceptionYr.Drought+(1|Female)+(1|Year),
                 data=data3D, family=binomial(link = "logit"),na.action = na.exclude)
output.null2<-glmer(Pregnant~1+(1|Female)+(1|Year),data=data3D, 
                    family=binomial(link = "logit"),na=na.exclude)
#LRT:
anova(output.m2,output.null2)


####III. Behavior ####

scans<-read.csv("Monthly-Instantaneous-Data_Cleaned_12-2-22.csv",header=T, stringsAsFactors = T)

#restrict data to only subadults and adults
scans<-subset(scans,AgeClass!="juvenile")
scans<-subset(scans,AgeClass!="infant")

#remove data from unidentified individuals or individuals without known age class.
scans<-subset(scans,Focal!="Unmarked female")
scans<-subset(scans,Focal!="Unmarked male")
scans<-subset(scans,Focal!="Tonic")
scans<-subset(scans,Focal!="Bob")

#set year as a factor
scans$Real.Year<-as.factor(scans$Real.Year)

# Code drought year by wet season
scans$Drought[scans$Wet.Season.Year=="2014"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2015"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2016"] <- "Yes"
scans$Drought[scans$Wet.Season.Year=="2017"] <- "Yes"
scans$Drought[scans$Wet.Season.Year=="2022"] <- "Yes"
scans$Drought[scans$Wet.Season.Year=="2018"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2019"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2020"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2021"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2011"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2012"] <- "No"
scans$Drought[scans$Wet.Season.Year=="2013"] <- "No"
scans$Drought<-as.factor(scans$Drought)

#Remove June data
scans<-subset(scans,Month!="Jun")

summary(scans$Drought)

#Combine some behavior categories (feed/forage and social/groom/play)
scans$Feed.Forage<-scans$All.Feed+scans$Forage
scans$Social.Groom.Play<-scans$Social+scans$Groom+scans$Play

library(TMB)
library(glmmTMB)

###### A. Total Activity Budget ######
# for each dependent variable, we first tested whether the glmmTMB nbinom1 or nbinom2
# distribution was more appropriate using the null models and a LRT. We then used the 
# best fit distribution.

## Feed/Forage
#null models
feed.nb2<- glmmTMB(Feed.Forage~Sex+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
feed.nb2B<- glmmTMB(Feed.Forage~Sex+AgeClass+
                      offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                    data=scans,
                    ziformula=~1,
                    family=nbinom1)
#LRT to test best distribution
anova(feed.nb2B,feed.nb2)

#drought model - #nbinom2
feed.nb1<- glmmTMB(Feed.Forage~Sex+Drought+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
#LRT:
anova(feed.nb2,feed.nb1)

## Lick
#null models
li.nb2<- glmmTMB(Lick~Sex+AgeClass+
                   offset(log(Total.Scans))+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom1)
li.nb2B<- glmmTMB(Lick~Sex+AgeClass+
                    offset(log(Total.Scans))+(1|Month)+(1|Real.Year),
                  data=scans,
                  ziformula=~1,
                  family=nbinom2)
anova(li.nb2B,li.nb2)

#drought model - #nbinom1
li.nb1<- glmmTMB(Lick~Sex+Drought+AgeClass+
                   offset(log(Total.Scans))+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom1)
#LRT:
anova(li.nb1,li.nb2)


##Rest
#null models
rest.nb2<- glmmTMB(Rest~Sex+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
rest.nb2B<- glmmTMB(Rest~Sex+AgeClass+
                      offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                    data=scans,
                    ziformula=~1,
                    family=nbinom1)
anova(rest.nb2B,rest.nb2)

#drought model - #nbinom2
rest.nb1<- glmmTMB(Rest~Sex+Drought+AgeClass+
                     offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                   data=scans,
                   ziformula=~1,
                   family=nbinom2)
#LRT:
anova(rest.nb1,rest.nb2)

##Social (social + groom + play)
#null models
so.nb2<- glmmTMB(Social.Groom.Play~Sex+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
so.nb2B<- glmmTMB(Social.Groom.Play~Sex+AgeClass+
                    offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                  data=scans,
                  ziformula=~1,
                  family=nbinom1)
anova(so.nb2B,so.nb2)

#drought model - #nbinom2
so.nb1<- glmmTMB(Social.Groom.Play~Sex+Drought+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
anova(so.nb1,so.nb2)

##Travel
#null models
tr.nb2<- glmmTMB(Travel~Sex+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
tr.nb2B<- glmmTMB(Travel~Sex+AgeClass+
                    offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                  data=scans,
                  ziformula=~1,
                  family=nbinom1)
anova(tr.nb2B,tr.nb2)

#drought model - #nbinom2
tr.nb1<- glmmTMB(Travel~Sex+Drought+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
#LRT:
anova(tr.nb1,tr.nb2)

##Vigilant
#null models
vi.nb2<- glmmTMB(Vigilant~Sex+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom1)
vi.nb2B<- glmmTMB(Vigilant~Sex+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom2)
anova(vi.nb2B,vi.nb2)
#drought models
vi.nb1<- glmmTMB(Vigilant~Sex+Drought+AgeClass+
                   offset(log(Total.Scans))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=scans,
                 ziformula=~1,
                 family=nbinom1)
#LRT
anova(vi.nb1,vi.nb2)

#Vigilance effects calculation:
exp(confint(vi.nb1,method="Wald")) ## confidence interval

###### B. Feeding Activity Budget ######
# for each dependent variable, we first tested whether the glmmTMB nbinom1 or nbinom2
# distribution was more appropriate using the null models and a LRT. We then used the 
# best fit distribution.

#subset data to only feeding scans
feed.only<-subset(scans,All.Feed>0)

##Flowers
#null models
fl.nb2 <- glmmTMB(Feed.Flowers~Sex+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom1)
fl.nb2B <- glmmTMB(Feed.Flowers~Sex+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom2)
anova(fl.nb2B,fl.nb2)
#drought models - #nbinom1
fl.nb1 <- glmmTMB(Feed.Flowers~Sex+Drought+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom1)
anova(fl.nb1,fl.nb2)


##Fruit
#null models
fr.nb2<- glmmTMB(Feed.Fruit~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
fr.nb2B<- glmmTMB(Feed.Fruit~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(fr.nb2,fr.nb2B)
#drought model - nbinom2
fr.nb1<- glmmTMB(Feed.Fruit~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(fr.nb1,fr.nb2B)

## Mature leaves
#null models
ML.nb2<- glmmTMB(Feed.Mature.Leaves~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
ML.nb2B<- glmmTMB(Feed.Mature.Leaves~Sex+AgeClass+
                    offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                  data=feed.only,
                  ziformula=~1,
                  family=nbinom2)
anova(ML.nb2B,ML.nb2)
#drought model - nbinom1
ML.nb1<- glmmTMB(Feed.Mature.Leaves~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
anova(ML.nb1,ML.nb2)

##Young leaves
#null models
YL.nb2<- glmmTMB(Feed.Young.Leaves~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom1)
YL.nb2B<- glmmTMB(Feed.Young.Leaves~Sex+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(YL.nb2,YL.nb2B)
#drought model - nbinom2
YL.nb1<- glmmTMB(Feed.Young.Leaves~Sex+Drought+AgeClass+
                   offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                 data=feed.only,
                 ziformula=~1,
                 family=nbinom2)
anova(YL.nb1,YL.nb2B)

##Seeds
#null models
seeds.nb2<- glmmTMB(Feed.Seeds~Sex+AgeClass+
                      offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                    data=feed.only,
                    ziformula=~1,
                    family=nbinom1)
seeds.nb2B<- glmmTMB(Feed.Seeds~Sex+AgeClass+
                       offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                     data=feed.only,
                     ziformula=~1,
                     family=nbinom2)
anova(seeds.nb2B,seeds.nb2)
seeds.nb1<- glmmTMB(Feed.Seeds~Sex+Drought+AgeClass+
                      offset(log(All.Feed))+(1|Focal)+(1|Month)+(1|Real.Year),
                    data=feed.only,
                    ziformula=~1,
                    family=nbinom1)
#LRT:
anova(seeds.nb1,seeds.nb2)

## Adjust p-values for multiple testing across all behavior models:
ps<-c(0.6755, 0.7497, 0.107, 0.394, 0.01783, 0.1761, 0.0105, 0.003369, 0.2657, 0.004736, 0.002834)
p.adjust(ps,method="fdr")

## Calculate effects + confidence intervals for significant drought effects:
## insert models into code:
#fruit (fr.nb1), flowers (fl.nb1), young leaves (YL.nb1), seeds (seeds.nb1)
exp(confint(seeds.nb1,method="Wald")) ## confidence interval


#### IV. Phenology ######

phen<-read.csv('New-Cleaned-Phenology-2010-22_for_Drought.csv',header=T, stringsAsFactors=T)
names(phen)

#####A. Prepare phenology data for analysis #####
#restrict data to only observations since Dec 2010 (e.g., 2011 wet season) & other QC
phen<-subset(phen,Wet.Season.Year>2010)
phen<-subset(phen,DroughtAnalysis=="1")

#categorize drought wet seasons
phen$Drought[phen$Wet.Season.Year=="2011"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2012"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2013"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2014"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2015"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2016"] <- "Yes"
phen$Drought[phen$Wet.Season.Year=="2017"] <- "Yes"
phen$Drought[phen$Wet.Season.Year=="2022"] <- "Yes"
phen$Drought[phen$Wet.Season.Year=="2018"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2019"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2020"] <- "No"
phen$Drought[phen$Wet.Season.Year=="2021"] <- "No"
phen$Drought<-as.factor(phen$Drought)
summary(phen$Drought)

#categorize Drought the previous year
phen$YrAfterDrought[phen$Wet.Season.Year=="2011"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2012"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2013"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2014"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2015"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2016"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2017"] <- "Yes"
phen$YrAfterDrought[phen$Wet.Season.Year=="2022"] <- "Yes"
phen$YrAfterDrought[phen$Wet.Season.Year=="2018"] <- "Yes"
phen$YrAfterDrought[phen$Wet.Season.Year=="2019"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2020"] <- "No"
phen$YrAfterDrought[phen$Wet.Season.Year=="2021"] <- "No"
phen$YrAfterDrought<-as.factor(phen$YrAfterDrought)
summary(phen$YrAfterDrought)

#restrict to wet season data only
phen1<-subset(phen,Month2==12)
phen2<-subset(phen,Month2<6)
wet.season<-rbind(phen1,phen2)
#check that only wet season months are in data (Dec-May)
summary(wet.season$Month)

##### B. Relative abundance analyses: drought year #####

library(ordinal) #cumulative link mixed models with ordinal data

#recode abundance data as ordinal
wet.season$YL<-factor(wet.season$Young.leaves,order=TRUE,levels=c("0","1","2","3","4"))
wet.season$Ripe.fruit2<-factor(wet.season$Ripe.fruit,order=TRUE,levels=c("0","1","2","3","4"))
wet.season$ML<-factor(wet.season$Mature.leaves,order=TRUE,levels=c("0","1","2","3","4"))
wet.season$unripe<-factor(wet.season$Unripe.fruit,order=TRUE,levels=c("0","1","2","3","4"))
wet.season$Flowers2<-factor(wet.season$Flowers,order=TRUE,levels=c("0","1","2","3","4"))

#recode tree number and species to have shorter names (so no errors in clmm)
wet.season$tree<-wet.season$Tree.number
wet.season$spp<-wet.season$Tree.species

##Young Leaves
mm1<-clmm(YL~Drought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season, na.action = na.exclude)
mm1.null<-clmm(YL~1 + (1|tree:spp) + (1|Month) + (1|Year), 
               data = wet.season, na.action = na.exclude)
#LRT:
anova(mm1,mm1.null)

##Mature Leaves
mm2<-clmm(ML~Drought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season, na.action = na.exclude)
mm2.null<-clmm(ML~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season, na.action = na.exclude)
#LRT:
anova(mm2,mm2.null)

##Ripe Fruit
mm3<-clmm(Ripe.fruit2~Drought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season, na.action = na.exclude)
mm3.null<-clmm(Ripe.fruit2~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season, na.action = na.exclude)
#LRT:
anova(mm3,mm3.null)

##Unripe Fruit
mm4<-clmm(unripe~Drought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season, na.action = na.exclude)
mm4.null<-clmm(unripe~1 + (1|tree:spp) + (1|Month) + (1|Year), 
               data = wet.season, na.action = na.exclude)
#LRT:
anova(mm4,mm4.null)

##Flowers
mm5<-clmm(Flowers2~Drought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season, na.action = na.exclude)
mm5.null<-clmm(Flowers2~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season, na.action = na.exclude)
#LRT
anova(mm5,mm5.null)


##### C. Relative abundance analyses: year after drought #####
#remove observations before 2011 (since no rainfall data for 2010)
wet.season2<-subset(wet.season,Year>2011)

#shorten name of Yr After Drought for CLMM
wet.season2$YrAfter<-wet.season2$YrAfterDrought

##Young Leaves
mm1<-clmm(YL~YrAfterDrought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season2, na.action = na.exclude)
mm1B<-clmm(YL~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season2, na.action = na.exclude)
#LRT:
anova(mm1,mm1B)

##Mature Leaves
mm2<-clmm(ML~YrAfterDrought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season2, na.action = na.exclude)
mm2B<-clmm(ML~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season2, na.action = na.exclude)
#LRT
anova(mm2,mm2B)

##Ripe Fruit
mm3<-clmm(Ripe.fruit2~YrAfterDrought + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season2, na.action = na.exclude)
mm3B<-clmm(Ripe.fruit2~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season2, na.action = na.exclude)
#LRT:
anova(mm3,mm3B)

##Unripe fruit
mm4<-clmm(unripe~YrAfter + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season2, na.action = na.exclude)
mm4B<-clmm(unripe~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season2, na.action = na.exclude)
#LRT:
anova(mm4,mm4B)

##Flowers
mm5<-clmm(Flowers2~YrAfter + (1|tree:spp) + (1|Month) + (1|Year), 
          data = wet.season2, na.action = na.exclude)
mm5B<-clmm(Flowers2~1 + (1|tree:spp) + (1|Month) + (1|Year), 
           data = wet.season2, na.action = na.exclude)
#LRT:
anova(mm5,mm5B)

##### D. Feeding tree/vine mortality and drought #####

dead<-read.csv("Tree_mortality_summary_data.csv",header=T, stringsAsFactors = T,fileEncoding="UTF-8-BOM")

# one tailed test for increased mortality during drought
wilcox.test(dead$Dead.Trees~dead$Drought, alternative="less")

#one tailed test for increased mortality during drought or year following drought
wilcox.test(dead$Dead.Trees~dead$Drought.1Yr, alternative="less")
