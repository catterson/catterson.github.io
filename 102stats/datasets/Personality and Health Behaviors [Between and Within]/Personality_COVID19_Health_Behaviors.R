################################_____Set Environment_____################################
rm(list=ls())#clear environment
library("nlme","lme4")
setwd("") #set wd

################################_____Sample A_____################################
SampleA<-read.csv("SampleA.csv") #load data 

#Recode gender, ethnic and racial identity, and political affiliation into groups specified in pre-registration 
SampleA$T1_Gender[SampleA$T1_Gender==3]<-NA
SampleA$T1_Gender<-as.factor(SampleA$T1_Gender)
levels(SampleA$T1_Gender) <- c("M", "W") 
SampleA$T1_Ethnicity[SampleA$T1_Ethnicity==1]<-1
SampleA$T1_Ethnicity[SampleA$T1_Ethnicity==2]<-2
SampleA$T1_Ethnicity[SampleA$T1_Ethnicity==5]<-3
SampleA$T1_Ethnicity[SampleA$T1_Ethnicity==4|SampleA$T1_Ethnicity==6|SampleA$T1_Ethnicity==7|SampleA$T1_Ethnicity==8]<-4
SampleA$T1_Ethnicity<-as.factor(SampleA$T1_Ethnicity)
levels(SampleA$T1_Ethnicity) <- c("AA", "W", "EA","O")
SampleA$T1_PolParty[SampleA$T1_PolParty==4]<-3
SampleA$T1_PolParty<-as.factor(SampleA$T1_PolParty)
levels(SampleA$T1_PolParty) <- c("R", "D", "I")

#Compute Health Behavior Variables
T2_Past_Behaviors<-transform(cbind(SampleA$T2_ActPast_Prevent_Self,
                                   SampleA$T2_ActPast_Prevent_Mask,
                                   SampleA$T2_ActPast_Prevent_Sanitize,
                                   SampleA$T2_ActPast_SocialDistance,
                                   SampleA$T2_ActPast_SelfIsolate),
                             MEAN=apply(cbind(SampleA$T2_ActPast_Prevent_Self,
                                              SampleA$T2_ActPast_Prevent_Mask,
                                              SampleA$T2_ActPast_Prevent_Sanitize,
                                              SampleA$T2_ActPast_SocialDistance,
                                              SampleA$T2_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T2_Past_Behaviors<-T2_Past_Behaviors$MEAN

T3_Past_Behaviors<-transform(cbind(SampleA$T3_ActPast_Prevent_Self,
                                   SampleA$T3_ActPast_Prevent_Mask,
                                   SampleA$T3_ActPast_Prevent_Sanitize,
                                   SampleA$T3_ActPast_SocialDistance,
                                   SampleA$T3_ActPast_SelfIsolate),
                             MEAN=apply(cbind(SampleA$T3_ActPast_Prevent_Self,
                                              SampleA$T3_ActPast_Prevent_Mask,
                                              SampleA$T3_ActPast_Prevent_Sanitize,
                                              SampleA$T3_ActPast_SocialDistance,
                                              SampleA$T3_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T3_Past_Behaviors<-T3_Past_Behaviors$MEAN

T4_Past_Behaviors<-transform(cbind(SampleA$T4_ActPast_Prevent_Self,
                                   SampleA$T4_ActPast_Prevent_Mask,
                                   SampleA$T4_ActPast_Prevent_Sanitize,
                                   SampleA$T4_ActPast_SocialDistance,
                                   SampleA$T4_ActPast_SelfIsolate),
                             MEAN=apply(cbind(SampleA$T4_ActPast_Prevent_Self,
                                              SampleA$T4_ActPast_Prevent_Mask,
                                              SampleA$T4_ActPast_Prevent_Sanitize,
                                              SampleA$T4_ActPast_SocialDistance,
                                              SampleA$T4_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T4_Past_Behaviors<-T4_Past_Behaviors$MEAN

T5_Past_Behaviors<-transform(cbind(SampleA$T5_ActPast_Prevent_Self,
                                   SampleA$T5_ActPast_Prevent_Mask,
                                   SampleA$T5_ActPast_Prevent_Sanitize,
                                   SampleA$T5_ActPast_SocialDistance,
                                   SampleA$T5_ActPast_SelfIsolate),
                             MEAN=apply(cbind(SampleA$T5_ActPast_Prevent_Self,
                                              SampleA$T5_ActPast_Prevent_Mask,
                                              SampleA$T5_ActPast_Prevent_Sanitize,
                                              SampleA$T5_ActPast_SocialDistance,
                                              SampleA$T5_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T5_Past_Behaviors<-T5_Past_Behaviors$MEAN

T6_Past_Behaviors<-transform(cbind(SampleA$T6_ActPast_Prevent_Self,
                                   SampleA$T6_ActPast_Prevent_Mask,
                                   SampleA$T6_ActPast_Prevent_Sanitize,
                                   SampleA$T6_ActPast_SocialDistance,
                                   SampleA$T6_ActPast_SelfIsolate),
                             MEAN=apply(cbind(SampleA$T6_ActPast_Prevent_Self,
                                              SampleA$T6_ActPast_Prevent_Mask,
                                              SampleA$T6_ActPast_Prevent_Sanitize,
                                              SampleA$T6_ActPast_SocialDistance,
                                              SampleA$T6_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T6_Past_Behaviors<-T6_Past_Behaviors$MEAN

T2_Present_Behaviors<-transform(cbind(SampleA$T2_ActPresent_Prevent_Self,
                                      SampleA$T2_ActPresent_Prevent_Mask,
                                      SampleA$T2_ActPresent_Prevent_Sanitize,
                                      SampleA$T2_ActPresent_SocialDistance,
                                      SampleA$T2_ActPresent_SelfIsolate),
                                MEAN=apply(cbind(SampleA$T2_ActPresent_Prevent_Self,
                                                 SampleA$T2_ActPresent_Prevent_Mask,
                                                 SampleA$T2_ActPresent_Prevent_Sanitize,
                                                 SampleA$T2_ActPresent_SocialDistance,
                                                 SampleA$T2_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T2_Present_Behaviors<-T2_Present_Behaviors$MEAN

T3_Present_Behaviors<-transform(cbind(SampleA$T3_ActPresent_Prevent_Self,
                                      SampleA$T3_ActPresent_Prevent_Mask,
                                      SampleA$T3_ActPresent_Prevent_Sanitize,
                                      SampleA$T3_ActPresent_SocialDistance,
                                      SampleA$T3_ActPresent_SelfIsolate),
                                MEAN=apply(cbind(SampleA$T3_ActPresent_Prevent_Self,
                                                 SampleA$T3_ActPresent_Prevent_Mask,
                                                 SampleA$T3_ActPresent_Prevent_Sanitize,
                                                 SampleA$T3_ActPresent_SocialDistance,
                                                 SampleA$T3_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T3_Present_Behaviors<-T3_Present_Behaviors$MEAN

T4_Present_Behaviors<-transform(cbind(SampleA$T4_ActPresent_Prevent_Self,
                                      SampleA$T4_ActPresent_Prevent_Mask,
                                      SampleA$T4_ActPresent_Prevent_Sanitize,
                                      SampleA$T4_ActPresent_SocialDistance,
                                      SampleA$T4_ActPresent_SelfIsolate),
                                MEAN=apply(cbind(SampleA$T4_ActPresent_Prevent_Self,
                                                 SampleA$T4_ActPresent_Prevent_Mask,
                                                 SampleA$T4_ActPresent_Prevent_Sanitize,
                                                 SampleA$T4_ActPresent_SocialDistance,
                                                 SampleA$T4_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T4_Present_Behaviors<-T4_Present_Behaviors$MEAN

T5_Present_Behaviors<-transform(cbind(SampleA$T5_ActPresent_Prevent_Self,
                                      SampleA$T5_ActPresent_Prevent_Mask,
                                      SampleA$T5_ActPresent_Prevent_Sanitize,
                                      SampleA$T5_ActPresent_SocialDistance,
                                      SampleA$T5_ActPresent_SelfIsolate),
                                MEAN=apply(cbind(SampleA$T5_ActPresent_Prevent_Self,
                                                 SampleA$T5_ActPresent_Prevent_Mask,
                                                 SampleA$T5_ActPresent_Prevent_Sanitize,
                                                 SampleA$T5_ActPresent_SocialDistance,
                                                 SampleA$T5_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T5_Present_Behaviors<-T5_Present_Behaviors$MEAN

T6_Present_Behaviors<-transform(cbind(SampleA$T6_ActPresent_Prevent_Self,
                                      SampleA$T6_ActPresent_Prevent_Mask,
                                      SampleA$T6_ActPresent_Prevent_Sanitize,
                                      SampleA$T6_ActPresent_SocialDistance,
                                      SampleA$T6_ActPresent_SelfIsolate),
                                MEAN=apply(cbind(SampleA$T6_ActPresent_Prevent_Self,
                                                 SampleA$T6_ActPresent_Prevent_Mask,
                                                 SampleA$T6_ActPresent_Prevent_Sanitize,
                                                 SampleA$T6_ActPresent_SocialDistance,
                                                 SampleA$T6_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
SampleA$T6_Present_Behaviors<-T6_Present_Behaviors$MEAN

#Compute Personality

psych::alpha(cbind(SampleA$T1c_BFI_2_XS_agreeable1,
                   (4-SampleA$T1c_BFI_2_XS_agreeable2),
                   SampleA$T1c_BFI_2_XS_agreeable3)) #.60

T1c_BFI_agree<-transform(cbind(SampleA$T1c_BFI_2_XS_agreeable1,
                               (4-SampleA$T1c_BFI_2_XS_agreeable2),
                               SampleA$T1c_BFI_2_XS_agreeable3),
                             MEAN=apply(cbind(SampleA$T1c_BFI_2_XS_agreeable1,
                                              (4-SampleA$T1c_BFI_2_XS_agreeable2),
                                              SampleA$T1c_BFI_2_XS_agreeable3),1, mean, na.rm = TRUE))
SampleA$T1c_BFI_agree<-T1c_BFI_agree$MEAN

psych::alpha(cbind(SampleA$T1c_BFI_2_XS_open1,
                  (4-SampleA$T1c_BFI_2_XS_open2),
                  SampleA$T1c_BFI_2_XS_open3)) #.66


T1c_BFI_open<-transform(cbind(SampleA$T1c_BFI_2_XS_open1,
                               (4-SampleA$T1c_BFI_2_XS_open2),
                               SampleA$T1c_BFI_2_XS_open3),
                         MEAN=apply(cbind(SampleA$T1c_BFI_2_XS_open1,
                                          (4-SampleA$T1c_BFI_2_XS_open2),
                                          SampleA$T1c_BFI_2_XS_open3),1, mean, na.rm = TRUE))
SampleA$T1c_BFI_open<-T1c_BFI_open$MEAN

psych::alpha(cbind((4-SampleA$T1c_BFI_2_XS_conscientious1),
                   (4-SampleA$T1c_BFI_2_XS_conscientious2),
                   SampleA$T1c_BFI_2_XS_conscientious3)) #.74

T1c_BFI_conscientious<-transform(cbind((4-SampleA$T1c_BFI_2_XS_conscientious1),
                                       (4-SampleA$T1c_BFI_2_XS_conscientious2),
                                       SampleA$T1c_BFI_2_XS_conscientious3),
                                       MEAN=apply(cbind((4-SampleA$T1c_BFI_2_XS_conscientious1),
                                                         (4-SampleA$T1c_BFI_2_XS_conscientious2),
                                                        SampleA$T1c_BFI_2_XS_conscientious3),1, mean, na.rm = TRUE))
                                       
SampleA$T1c_BFI_conscientious<-T1c_BFI_conscientious$MEAN

psych::alpha(cbind((4-SampleA$T1c_BFI_2_XS_extravert1),
                   SampleA$T1c_BFI_2_XS_extravert2,
                   SampleA$T1c_BFI_2_XS_extravert3)) #.62

T1c_BFI_extraversion<-transform(cbind((4-SampleA$T1c_BFI_2_XS_extravert1),
                                      SampleA$T1c_BFI_2_XS_extravert2,
                                      SampleA$T1c_BFI_2_XS_extravert3),
                                MEAN=apply(cbind((4-SampleA$T1c_BFI_2_XS_extravert1),
                                                 SampleA$T1c_BFI_2_XS_extravert2,
                                                 SampleA$T1c_BFI_2_XS_extravert3),1, mean, na.rm = TRUE))

SampleA$T1c_BFI_extraversion<-T1c_BFI_extraversion$MEAN

psych::alpha(cbind(SampleA$T1c_BFI_2_XS_neurotic1,
                   SampleA$T1c_BFI_2_XS_neurotic2,
                   (4-SampleA$T1c_BFI_2_XS_neurotic3))) #.76

T1c_BFI_neurotic<-transform(cbind(SampleA$T1c_BFI_2_XS_neurotic1,
                                  SampleA$T1c_BFI_2_XS_neurotic2,
                                  (4-SampleA$T1c_BFI_2_XS_neurotic3)),
                            MEAN=apply(cbind(SampleA$T1c_BFI_2_XS_neurotic1,
                                             SampleA$T1c_BFI_2_XS_neurotic2,
                                             (4-SampleA$T1c_BFI_2_XS_neurotic3)),1, mean, na.rm = TRUE))

SampleA$T1c_BFI_neurotic<-T1c_BFI_neurotic$MEAN

#Compute discrete time in months
SampleA$time1<-0
SampleA$time2<-1
SampleA$time3<-2
SampleA$time4<-3
SampleA$time5<-4

#pare down SampleA to composite variables and covariates 
SampleA_wide<-cbind.data.frame(SampleA$ID, 
                            SampleA$time1,
                            SampleA$time2,
                            SampleA$time3,
                            SampleA$time4,
                            SampleA$time5,
                            SampleA$T1_Age,
                            SampleA$T1_Gender,
                            SampleA$T1_Ethnicity,
                            SampleA$T1_Education,
                            SampleA$T1_PolParty,
                            SampleA$T1c_BFI_open,
                            SampleA$T1c_BFI_conscientious,
                            SampleA$T1c_BFI_extraversion,
                            SampleA$T1c_BFI_agree,
                            SampleA$T1c_BFI_neurotic,
                            SampleA$T2_Past_Behaviors,
                            SampleA$T3_Past_Behaviors,
                            SampleA$T4_Past_Behaviors,
                            SampleA$T5_Past_Behaviors,
                            SampleA$T6_Past_Behaviors,
                            SampleA$T2_Present_Behaviors,
                            SampleA$T3_Present_Behaviors,
                            SampleA$T4_Present_Behaviors,
                            SampleA$T5_Present_Behaviors,
                            SampleA$T6_Present_Behaviors,
                            SampleA$T2_ActPast_Prevent_Self,
                            SampleA$T3_ActPast_Prevent_Self,
                            SampleA$T4_ActPast_Prevent_Self,
                            SampleA$T5_ActPast_Prevent_Self,
                            SampleA$T5_ActPast_Prevent_Self,
                            SampleA$T2_ActPast_Prevent_Mask,
                            SampleA$T3_ActPast_Prevent_Mask,
                            SampleA$T4_ActPast_Prevent_Mask,
                            SampleA$T5_ActPast_Prevent_Mask,
                            SampleA$T6_ActPast_Prevent_Mask,
                            SampleA$T2_ActPast_Prevent_Sanitize,
                            SampleA$T3_ActPast_Prevent_Sanitize,
                            SampleA$T4_ActPast_Prevent_Sanitize,
                            SampleA$T5_ActPast_Prevent_Sanitize,
                            SampleA$T6_ActPast_Prevent_Sanitize,
                            SampleA$T2_ActPast_SocialDistance,
                            SampleA$T3_ActPast_SocialDistance,
                            SampleA$T4_ActPast_SocialDistance,
                            SampleA$T5_ActPast_SocialDistance,
                            SampleA$T6_ActPast_SocialDistance,
                            SampleA$T2_ActPast_SelfIsolate,
                            SampleA$T3_ActPast_SelfIsolate,
                            SampleA$T4_ActPast_SelfIsolate,
                            SampleA$T5_ActPast_SelfIsolate,
                            SampleA$T6_ActPast_SelfIsolate)

names(SampleA_wide)<-c("ID", #name variables
                    "time1",
                    "time2",
                    "time3",
                    "time4",
                    "time5",
                    "age",
                    "gender",
                    "ethnicity",
                    "education",
                    "party",
                    "O",
                    "C",
                    "E",
                    "A",
                    "N",
                    "behaviors1",
                    "behaviors2",
                    "behaviors3",
                    "behaviors4",
                    "behaviors5",
                    "fbehaviors1",
                    "fbehaviors2",
                    "fbehaviors3",
                    "fbehaviors4",
                    "fbehaviors5",
                    "self1",
                    "self2",
                    "self3",
                    "self4",
                    "self5",
                    "mask1",
                    "mask2",
                    "mask3",
                    "mask4",
                    "mask5",
                    "sanitize1",
                    "sanitize2",
                    "sanitize3",
                    "sanitize4",
                    "sanitize5",
                    "distance1",
                    "distance2",
                    "distance3",
                    "distance4",
                    "distance5",
                    "isolate1",
                    "isolate2",
                    "isolate3",
                    "isolate4",
                    "isolate5")

#Remove participants without data used in analyses 
SampleA<-subset(SampleA,!is.na(SampleA$T1c_BFI_agree)&
                        (!is.na(SampleA$T2_Past_Behaviors)
                      |!is.na(SampleA$T3_Past_Behaviors)
                      |!is.na(SampleA$T4_Past_Behaviors)
                      |!is.na(SampleA$T5_Past_Behaviors)
                      |!is.na(SampleA$T6_Past_Behaviors)))
nrow(SampleA) #N=596

#Calculate demographics
summary(SampleA$T1_Age)
sd(SampleA$T1_Age,na.rm=T)
summary(as.factor(SampleA$T1_Gender))/nrow(SampleA)
summary(as.factor(SampleA$T1_Ethnicity))/nrow(SampleA)
summary(as.factor(SampleA$T1_PolParty))/nrow(SampleA)

#Table 1 
#Descriptive Statistics
psych::describe(SampleA$T1c_BFI_agree)
psych::describe(SampleA$T1c_BFI_open)
psych::describe(SampleA$T1c_BFI_conscientious)
psych::describe(SampleA$T1c_BFI_extraversion)
psych::describe(SampleA$T1c_BFI_neurotic)
psych::describe(SampleA$T2_Past_Behaviors)
psych::describe(SampleA$T3_Past_Behaviors)
psych::describe(SampleA$T4_Past_Behaviors)
psych::describe(SampleA$T5_Past_Behaviors)
psych::describe(SampleA$T6_Past_Behaviors)

#z-standardize predictors (units of between-person SDs)
SampleA_wide$O<-scale(SampleA_wide$O)
SampleA_wide$C<-scale(SampleA_wide$C)
SampleA_wide$E<-scale(SampleA_wide$E)
SampleA_wide$A<-scale(SampleA_wide$A)
SampleA_wide$N<-scale(SampleA_wide$N)
SampleA_wide$education<-scale(SampleA_wide$education)
SampleA_wide$age<-scale(SampleA_wide$age)

#Restructure SampleA into long format 
long <- reshape(SampleA_wide,varying = c("time1",#shape in long format
                                      "time2",
                                      "time3",
                                      "time4",
                                      "time5",
                                      "behaviors1",  
                                      "behaviors2",
                                      "behaviors3",
                                      "behaviors4",
                                      "behaviors5",
                                      "fbehaviors1",  
                                      "fbehaviors2",
                                      "fbehaviors3",
                                      "fbehaviors4",
                                      "fbehaviors5",
                                      "self1",
                                      "self2",
                                      "self3",
                                      "self4",
                                      "self5",
                                      "mask1",
                                      "mask2",
                                      "mask3",
                                      "mask4",
                                      "mask5",
                                      "sanitize1",
                                      "sanitize2",
                                      "sanitize3",
                                      "sanitize4",
                                      "sanitize5",
                                      "distance1",
                                      "distance2",
                                      "distance3",
                                      "distance4",
                                      "distance5",
                                      "isolate1",
                                      "isolate2",
                                      "isolate3",
                                      "isolate4",
                                      "isolate5"),
                direction = 'long', idvar = 'ID', sep = '', timevar = 'wave')

#In the top quartile of agreeableness, 58% of participants engaged in health behaviors "often" or "very often or always," compared to 36% of participants in the bottom quartile.
lowA<-subset(long,long$A<(quantile(long$A,na.rm=T)[2]))
highA<-subset(long,long$A>(quantile(long$A,na.rm=T)[4]))
sum(highA$behaviors>=3,na.rm=T)/nrow(highA)
sum(lowA$behaviors>=3,na.rm=T)/nrow(lowA)

#z-standardize outcomes (units of total SD)
long$behaviors<-scale(long$behaviors)
long$fbehaviors<-scale(long$fbehaviors)
long$mask<-scale(long$mask)
long$self<-scale(long$self)
long$isolate<-scale(long$isolate)
long$distance<-scale(long$distance)
long$sanitize<-scale(long$sanitize)

#Table 2 
#Primary Analyses: Personality Predictors of COVID-19 Preventive Health Behaviors 
a.model <- lme(behaviors ~ 1 + A+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
c.model <- lme(behaviors ~ 1 + C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
o.model <- lme(behaviors ~ 1 + O+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
e.model <- lme(behaviors ~ 1 + E+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
n.model <- lme(behaviors ~ 1 + N+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
nc.model <- lme(behaviors ~ 1 + N*C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )

#store confidence intervals 
a.ci<-intervals(a.model, level = 0.95)
c.ci<-intervals(c.model, level = 0.95)
o.ci<-intervals(o.model, level = 0.95)
e.ci<-intervals(e.model, level = 0.95)
n.ci<-intervals(n.model, level = 0.95)
nc.ci<-intervals(nc.model, level = 0.95)

#coefficients and test statitistics 
summary(a.model)
summary(o.model)
summary(c.model)
summary(e.model)
summary(n.model)
summary(nc.model)

#95% CIs
a.ci$fixed
o.ci$fixed
c.ci$fixed
e.ci$fixed
n.ci$fixed
nc.ci$fixed

#Table 2
#Panel A. Sensitivity Analysis 1: Simple Effects without Covariate Adjustment 
o.model1<-lme(behaviors ~ 1 + O, random =~1|ID, data=long, na.action="na.exclude" )
c.model1<-lme(behaviors ~ 1 + C, random =~1|ID, data=long, na.action="na.exclude" )
e.model1<-lme(behaviors ~ 1 + E, random =~1|ID, data=long, na.action="na.exclude" )
a.model1<-lme(behaviors ~ 1 + A, random =~1|ID, data=long, na.action="na.exclude" )
n.model1<-lme(behaviors ~ 1 + N, random =~1|ID, data=long, na.action="na.exclude" )

#store confidence intervals 
a.ci<-intervals(a.model1, level = 0.95)
o.ci<-intervals(o.model1, level = 0.95)
c.ci<-intervals(c.model1, level = 0.95)
e.ci<-intervals(e.model1, level = 0.95)
n.ci<-intervals(n.model1, level = 0.95)

#coefficients and test statitistics 
summary(a.model1)
summary(o.model1)
summary(c.model1)
summary(e.model1)
summary(n.model1)

#95% CIs
a.ci$fixed
o.ci$fixed
c.ci$fixed
e.ci$fixed
n.ci$fixed

#Panel B. Sensitivity Analysis 2: Unique Effects Controlling for Other Big Five Traits 
sim.model <- lme(behaviors ~ 1 + A+O+C+E+N, random =~1|ID, data=long, na.action="na.exclude")

#store confidence intervals 
sim.ci<-intervals(sim.model, level = 0.95)

#coefficients and test statitistics 
summary(sim.model)

#95% CIs
sim.ci$fixed

#Panel C. Sensitivity Analysis 3: Predicting Intended Future Behaviors 
o.model3 <- lme(fbehaviors ~ 1 + O+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
c.model3 <- lme(fbehaviors ~ 1 + C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
e.model3 <- lme(fbehaviors ~ 1 + E+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
a.model3 <- lme(fbehaviors ~ 1 + A+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
n.model3 <- lme(fbehaviors ~ 1 + N+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )

#store confidence intervals 
a.ci<-intervals(a.model3, level = 0.95)
o.ci<-intervals(o.model3, level = 0.95)
c.ci<-intervals(c.model3, level = 0.95)
e.ci<-intervals(e.model3, level = 0.95)
n.ci<-intervals(n.model3, level = 0.95)

#coefficients and test statitistics 
summary(a.model3)
summary(o.model3)
summary(c.model3)
summary(e.model3)
summary(n.model3)

#95% CIs
a.ci$fixed
o.ci$fixed
c.ci$fixed
e.ci$fixed
n.ci$fixed

#Exploratory Effect Size Comparison
m1 <- lmer(behaviors ~ I(A + C) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ A + C + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(A + O) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ A + O + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(A + E) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ A + E + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(A + N) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ A + N + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(O + C) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ O + C + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(O + E) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ O + E + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(O + N) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ O + N + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(C + E) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ C + E + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(C + N) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ C + N + (1 | ID), data = long)
anova(m1, m2)

m1 <- lmer(behaviors ~ I(E + N) + (1 | ID), data = long)
m2 <- lmer(behaviors ~ E + N + (1 | ID), data = long)
anova(m1, m2)

#Additional Exploratory Sensitivity Analyses
time<-(lme(behaviors ~ 1 + time+age+education+gender+ethnicity+party, random =~1+time|ID, data=long, na.action="na.exclude" ))
summary(time) #main effect of time

a.time<-(lme(behaviors ~ 1 + A*time+age+education+gender+ethnicity+party, random =~1+time|ID, data=long, na.action="na.exclude" ))
a.age<-(lme(behaviors ~ 1 + A*age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
a.gender<-(lme(behaviors ~ 1 + A*gender+age+education+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
a.education<-(lme(behaviors ~ 1 + A*education+age+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
a.ethnicity<-(lme(behaviors ~ 1 + A*ethnicity+age+education+gender+party, random =~1|ID, data=long, na.action="na.exclude" ))
a.party<-(lme(behaviors ~ 1 + A*party+age+education+gender+ethnicity, random =~1|ID, data=long, na.action="na.exclude" ))

summary(a.time)
summary(a.age)
summary(a.gender)
summary(a.education)
anova(a.ethnicity)
anova(a.party)

o.time<-(lme(behaviors ~ 1 + O*time+age+education+gender+ethnicity+party, random =~1+time|ID, data=long, na.action="na.exclude" ))
o.age<-(lme(behaviors ~ 1 + O*age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
o.gender<-(lme(behaviors ~ 1 + O*gender+age+education+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
o.education<-(lme(behaviors ~ 1 + O*education+age+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
o.ethnicity<-(lme(behaviors ~ 1 + O*ethnicity+age+education+gender+party, random =~1|ID, data=long, na.action="na.exclude" ))
o.party<-(lme(behaviors ~ 1 + O*party+age+education+gender+ethnicity, random =~1|ID, data=long, na.action="na.exclude" ))

summary(o.time)
summary(o.age)
summary(o.gender)
summary(o.education)
anova(o.ethnicity) #
summary(o.ethnicity) 
anova(o.party)

c.time<-(lme(behaviors ~ 1 + C*time+age+education+gender+ethnicity+party, random =~1+time|ID, data=long, na.action="na.exclude" ))
c.age<-(lme(behaviors ~ 1 + C*age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
c.gender<-(lme(behaviors ~ 1 + C*gender+age+education+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
c.education<-(lme(behaviors ~ 1 + C*education+age+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
c.ethnicity<-(lme(behaviors ~ 1 + C*ethnicity+age+education+gender+party, random =~1|ID, data=long, na.action="na.exclude" ))
c.party<-(lme(behaviors ~ 1 + C*party+age+education+gender+ethnicity, random =~1|ID, data=long, na.action="na.exclude" ))

summary(c.time)
summary(c.age) 
summary(c.gender)
summary(c.education)
anova(c.ethnicity)
anova(c.party)

e.time<-(lme(behaviors ~ 1 + E*time+age+education+gender+ethnicity+party, random =~1+time|ID, data=long, na.action="na.exclude" ))
e.age<-(lme(behaviors ~ 1 + E*age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
e.gender<-(lme(behaviors ~ 1 + E*gender+age+education+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
e.education<-(lme(behaviors ~ 1 + E*education+age+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
e.ethnicity<-(lme(behaviors ~ 1 + E*ethnicity+age+education+gender+party, random =~1|ID, data=long, na.action="na.exclude" ))
e.party<-(lme(behaviors ~ 1 + E*party+age+education+gender+ethnicity, random =~1|ID, data=long, na.action="na.exclude" ))

summary(e.time) 
summary(e.age) 
summary(e.gender)
summary(e.education)
anova(e.ethnicity) 
anova(e.party)

n.time<-(lme(behaviors ~ 1 + N*time+age+education+gender+ethnicity+party, random =~1+time|ID, data=long, na.action="na.exclude" ))
n.age<-(lme(behaviors ~ 1 + N*age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
n.gender<-(lme(behaviors ~ 1 + N*gender+age+education+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
n.education<-(lme(behaviors ~ 1 + N*education+age+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" ))
n.ethnicity<-(lme(behaviors ~ 1 + N*ethnicity+age+education+gender+party, random =~1|ID, data=long, na.action="na.exclude" ))
n.party<-(lme(behaviors ~ 1 + N*party+age+education+gender+ethnicity, random =~1|ID, data=long, na.action="na.exclude" ))

summary(n.time) 
summary(n.age) 
summary(n.gender)
summary(n.education)
anova(n.ethnicity) 
anova(n.party) #
summary(n.party) #


#Supplementary Digital Content Table 1
#Personality Predictors of Individual COVID-19 Preventive Health Behaviors 

a.model4.mask <- lme(mask ~ 1 + A+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
o.model4.mask <- lme(mask ~ 1 + O+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
c.model4.mask <- lme(mask ~ 1 + C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
e.model4.mask<- lme(mask ~ 1 + E+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
n.model4.mask <- lme(mask ~ 1 + N+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )

a.model4.self <- lme(self ~ 1 + A+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
o.model4.self <- lme(self ~ 1 + O+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
c.model4.self <- lme(self ~ 1 + C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
e.model4.self<- lme(self ~ 1 + E+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
n.model4.self <- lme(self ~ 1 + N+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )

o.model4.distance <- lme(distance ~ 1 + O+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
c.model4.distance <- lme(distance ~ 1 + C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
e.model4.distance<- lme(distance ~ 1 + E+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
a.model4.distance <- lme(distance ~ 1 + A+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
n.model4.distance <- lme(distance ~ 1 + N+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )

o.model4.sanitize <- lme(sanitize ~ 1 + O+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
c.model4.sanitize <- lme(sanitize ~ 1 + C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
e.model4.sanitize<- lme(sanitize ~ 1 + E+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
a.model4.sanitize <- lme(sanitize ~ 1 + A+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
n.model4.sanitize <- lme(sanitize ~ 1 + N+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )

o.model4.isolate <- lme(isolate ~ 1 + O+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
c.model4.isolate <- lme(isolate ~ 1 + C+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
e.model4.isolate<- lme(isolate ~ 1 + E+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
a.model4.isolate <- lme(isolate ~ 1 + A+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )
n.model4.isolate <- lme(isolate ~ 1 + N+age+education+gender+ethnicity+party, random =~1|ID, data=long, na.action="na.exclude" )

summary(a.model4.mask) 
intervals(a.model4.mask,which="fixed") 
summary(o.model4.mask) 
intervals(o.model4.mask,which="fixed") 
summary(c.model4.mask) 
intervals(c.model4.mask,which="fixed") 
summary(e.model4.mask) 
intervals(e.model4.mask,which="fixed") 
summary(n.model4.mask) 
intervals(n.model4.mask,which="fixed") 

summary(a.model4.self) 
intervals(a.model4.self,which="fixed") 
summary(o.model4.self) 
intervals(o.model4.self,which="fixed") 
summary(c.model4.self) 
intervals(c.model4.self,which="fixed") 
summary(e.model4.self) 
intervals(e.model4.self,which="fixed") 
summary(n.model4.self) 
intervals(n.model4.self,which="fixed") 

summary(a.model4.sanitize) 
intervals(a.model4.sanitize,which="fixed") 
summary(o.model4.sanitize) 
intervals(o.model4.sanitize,which="fixed") 
summary(c.model4.sanitize) 
intervals(c.model4.sanitize,which="fixed") 
summary(e.model4.sanitize) 
intervals(e.model4.sanitize,which="fixed") 
summary(n.model4.sanitize) 
intervals(n.model4.sanitize,which="fixed") 

summary(a.model4.distance) 
intervals(a.model4.distance,which="fixed") 
summary(o.model4.distance) 
intervals(o.model4.distance,which="fixed") 
summary(c.model4.distance) 
intervals(c.model4.distance,which="fixed") 
summary(e.model4.distance) 
intervals(e.model4.distance,which="fixed") 
summary(n.model4.distance) 
intervals(n.model4.distance,which="fixed") 

summary(a.model4.isolate) 
intervals(a.model4.isolate,which="fixed") 
summary(o.model4.isolate) 
intervals(o.model4.isolate,which="fixed") 
summary(c.model4.isolate) 
intervals(c.model4.isolate,which="fixed") 
summary(e.model4.isolate) 
intervals(e.model4.isolate,which="fixed") 
summary(n.model4.isolate) 
intervals(n.model4.isolate,which="fixed") 

#Supplementary Digital Content Table 2
#Simple Associations between Demographic Characteristics and Health Behaviors   
age.model <- lme(behaviors ~ 1 + age, random =~1|ID, data=long, na.action="na.exclude" )
education.model <- lme(behaviors ~ 1 + education, random =~1|ID, data=long, na.action="na.exclude" )
gender.model <- lme(behaviors ~ 1 + gender, random =~1|ID, data=long, na.action="na.exclude" )
ethnicity.model <- lme(behaviors ~ 1 + ethnicity, random =~1|ID, data=long, na.action="na.exclude" )
party.model <- lme(behaviors ~ 1 + party, random =~1|ID, data=long, na.action="na.exclude" )

#coefficients and test statitistics 
summary(age.model)
summary(education.model)
summary(gender.model)
summary(ethnicity.model)
summary(party.model)
ethnicity.model <- lme(behaviors ~ 1 + relevel(ethnicity, ref="W"), random =~1|ID, data=long, na.action="na.exclude" )
summary(ethnicity.model)
ethnicity.model <- lme(behaviors ~ 1 + relevel(ethnicity, ref="EA"), random =~1|ID, data=long, na.action="na.exclude" )
summary(ethnicity.model)
party.model <- lme(behaviors ~ 1 + relevel(party, ref="I"), random =~1|ID, data=long, na.action="na.exclude" )
summary(party.model)


##Supplementary Digital Content Table 3
#Simple Associations between Demographic Characteristics and Agreeableness      
summary(lm(scale(SampleA$T1c_BFI_agree)~scale(SampleA$T1_Age)))
summary(lm(scale(SampleA$T1c_BFI_agree)~scale(SampleA$T1_Education)))
summary(lm(scale(SampleA$T1c_BFI_agree)~SampleA$T1_Gender))
summary(lm(scale(SampleA$T1c_BFI_agree)~SampleA$T1_Ethnicity))
summary(lm(scale(SampleA$T1c_BFI_agree)~relevel(SampleA$T1_Ethnicity, ref="W")))
summary(lm(scale(SampleA$T1c_BFI_agree)~relevel(SampleA$T1_Ethnicity, ref="EA")))
summary(lm(scale(SampleA$T1c_BFI_agree)~SampleA$T1_PolParty))
summary(lm(scale(SampleA$T1c_BFI_agree)~relevel(SampleA$T1_PolParty, ref="I")))

##Supplementary Digital Content Table 4
#Simple Associations between Demographic Characteristics and Openness      
summary(lm(scale(SampleA$T1c_BFI_open)~scale(SampleA$T1_Age)))
summary(lm(scale(SampleA$T1c_BFI_open)~scale(SampleA$T1_Education)))
summary(lm(scale(SampleA$T1c_BFI_open)~SampleA$T1_Gender))
summary(lm(scale(SampleA$T1c_BFI_open)~SampleA$T1_Ethnicity))
summary(lm(scale(SampleA$T1c_BFI_open)~relevel(SampleA$T1_Ethnicity, ref="W")))
summary(lm(scale(SampleA$T1c_BFI_open)~relevel(SampleA$T1_Ethnicity, ref="EA")))
summary(lm(scale(SampleA$T1c_BFI_open)~SampleA$T1_PolParty))
summary(lm(scale(SampleA$T1c_BFI_open)~relevel(SampleA$T1_PolParty, ref="I")))

##Supplementary Digital Content Table 5
#Simple Associations between Demographic Characteristics and Conscientiousness      
summary(lm(scale(SampleA$T1c_BFI_conscientious)~scale(SampleA$T1_Age)))
summary(lm(scale(SampleA$T1c_BFI_conscientious)~scale(SampleA$T1_Education)))
summary(lm(scale(SampleA$T1c_BFI_conscientious)~SampleA$T1_Gender))
summary(lm(scale(SampleA$T1c_BFI_conscientious)~SampleA$T1_Ethnicity))
summary(lm(scale(SampleA$T1c_BFI_conscientious)~relevel(SampleA$T1_Ethnicity, ref="W")))
summary(lm(scale(SampleA$T1c_BFI_conscientious)~relevel(SampleA$T1_Ethnicity, ref="EA")))
summary(lm(scale(SampleA$T1c_BFI_conscientious)~SampleA$T1_PolParty))
summary(lm(scale(SampleA$T1c_BFI_conscientious)~relevel(SampleA$T1_PolParty, ref="I")))

##Supplementary Digital Content Table 6
#Simple Associations between Demographic Characteristics and Extraversion      
summary(lm(scale(SampleA$T1c_BFI_extraversion)~scale(SampleA$T1_Age)))
summary(lm(scale(SampleA$T1c_BFI_extraversion)~scale(SampleA$T1_Education)))
summary(lm(scale(SampleA$T1c_BFI_extraversion)~SampleA$T1_Gender))
summary(lm(scale(SampleA$T1c_BFI_extraversion)~SampleA$T1_Ethnicity))
summary(lm(scale(SampleA$T1c_BFI_extraversion)~relevel(SampleA$T1_Ethnicity, ref="W")))
summary(lm(scale(SampleA$T1c_BFI_extraversion)~relevel(SampleA$T1_Ethnicity, ref="EA")))
summary(lm(scale(SampleA$T1c_BFI_extraversion)~SampleA$T1_PolParty))
summary(lm(scale(SampleA$T1c_BFI_extraversion)~relevel(SampleA$T1_PolParty, ref="I")))

##Supplementary Digital Content Table 7
#Simple Associations between Demographic Characteristics and Neuroticism
summary(lm(scale(SampleA$T1c_BFI_neurotic)~scale(SampleA$T1_Age)))
summary(lm(scale(SampleA$T1c_BFI_neurotic)~scale(SampleA$T1_Education)))
summary(lm(scale(SampleA$T1c_BFI_neurotic)~SampleA$T1_Gender))
summary(lm(scale(SampleA$T1c_BFI_neurotic)~SampleA$T1_Ethnicity))
summary(lm(scale(SampleA$T1c_BFI_neurotic)~relevel(SampleA$T1_Ethnicity, ref="W")))
summary(lm(scale(SampleA$T1c_BFI_neurotic)~relevel(SampleA$T1_Ethnicity, ref="EA")))
summary(lm(scale(SampleA$T1c_BFI_neurotic)~SampleA$T1_PolParty))
summary(lm(scale(SampleA$T1c_BFI_neurotic)~relevel(SampleA$T1_PolParty, ref="I")))

################################_____Replication in Sample B_____################################
SampleB<-read.csv("SampleB.csv") #load data

#Recode gender, ethnic and racial identity, and political party into groups specified in pre-registration 
SampleB$T1_Gender[SampleB$T1_Gender==6]<-NA
SampleB$gender<-as.factor(SampleB$T1_Gender)
levels(SampleB$gender) <- c("M", "W") 
SampleB$T1_Ethnicity[SampleB$T1_Ethnicity==1]<-1
SampleB$T1_Ethnicity[SampleB$T1_Ethnicity==2]<-2
SampleB$T1_Ethnicity[SampleB$T1_Ethnicity==5]<-3
SampleB$T1_Ethnicity[SampleB$T1_Ethnicity==4|SampleB$T1_Ethnicity==6|SampleB$T1_Ethnicity==7|SampleB$T1_Ethnicity==8]<-4
SampleB$ethnicity<-as.factor(SampleB$T1_Ethnicity)
levels(SampleB$ethnicity) <- c("AA", "W", "EA", "O")
SampleB$T1_PolParty[SampleB$T1_PolParty==4]<-3
SampleB$party<-as.factor(SampleB$T1_PolParty)
levels(SampleB$party) <- c("R", "D","I")

#Compute Health Behavior Variables
T6_Past_Behaviors<-transform(cbind(SampleB$T6_ActPast_Prevent_Self,
                                   SampleB$T6_ActPast_Prevent_Mask,
                                   SampleB$T6_ActPast_Prevent_Sanitize,
                                   SampleB$T6_ActPast_SocialDistance,
                                   SampleB$T6_ActPast_SelfIsolate),
                             MEAN=apply(cbind(SampleB$T6_ActPast_Prevent_Self,
                                              SampleB$T6_ActPast_Prevent_Mask,
                                              SampleB$T6_ActPast_Prevent_Sanitize,
                                              SampleB$T6_ActPast_SocialDistance,
                                              SampleB$T6_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
SampleB$behaviors<-T6_Past_Behaviors$MEAN

T6_Present_Behaviors<-transform(cbind(SampleB$T6_ActPresent_Prevent_Self,
                                      SampleB$T6_ActPresent_Prevent_Mask,
                                      SampleB$T6_ActPresent_Prevent_Sanitize,
                                      SampleB$T6_ActPresent_SocialDistance,
                                      SampleB$T6_ActPresent_SelfIsolate),
                             MEAN=apply(cbind(SampleB$T6_ActPresent_Prevent_Self,
                                              SampleB$T6_ActPresent_Prevent_Mask,
                                              SampleB$T6_ActPresent_Prevent_Sanitize,
                                              SampleB$T6_ActPresent_SocialDistance,
                                              SampleB$T6_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
SampleB$fbehaviors<-T6_Present_Behaviors$MEAN

#Compute personality

psych::alpha(cbind(SampleB$T6_BFI_2_XS_agreeable1,
                   (4-SampleB$T6_BFI_2_XS_agreeable2),
                   SampleB$T6_BFI_2_XS_agreeable3)) #.68

T6_BFI_agree<-transform(cbind(SampleB$T6_BFI_2_XS_agreeable1,
                              (4-SampleB$T6_BFI_2_XS_agreeable2),
                              SampleB$T6_BFI_2_XS_agreeable3),
                        MEAN=apply(cbind(SampleB$T6_BFI_2_XS_agreeable1,
                                         (4-SampleB$T6_BFI_2_XS_agreeable2),
                                         SampleB$T6_BFI_2_XS_agreeable3),1, mean, na.rm = TRUE))
SampleB$T6_BFI_agree<-T6_BFI_agree$MEAN

psych::alpha(cbind(SampleB$T6_BFI_2_XS_open1,
                   (4-SampleB$T6_BFI_2_XS_open2),
                   SampleB$T6_BFI_2_XS_open3)) #.65

T6_BFI_open<-transform(cbind(SampleB$T6_BFI_2_XS_open1,
                             (4-SampleB$T6_BFI_2_XS_open2),
                             SampleB$T6_BFI_2_XS_open3),
                       MEAN=apply(cbind(SampleB$T6_BFI_2_XS_open1,
                                        (4-SampleB$T6_BFI_2_XS_open2),
                                        SampleB$T6_BFI_2_XS_open3),1, mean, na.rm = TRUE))
SampleB$T6_BFI_open<-T6_BFI_open$MEAN

psych::alpha(cbind((4-SampleB$T6_BFI_2_XS_conscientious1),
                   (4-SampleB$T6_BFI_2_XS_conscientious2),
                   SampleB$T6_BFI_2_XS_conscientious3)) #.71

T6_BFI_conscientious<-transform(cbind((4-SampleB$T6_BFI_2_XS_conscientious1),
                                      (4-SampleB$T6_BFI_2_XS_conscientious2),
                                      SampleB$T6_BFI_2_XS_conscientious3),
                                MEAN=apply(cbind((4-SampleB$T6_BFI_2_XS_conscientious1),
                                                 (4-SampleB$T6_BFI_2_XS_conscientious2),
                                                 SampleB$T6_BFI_2_XS_conscientious3),1, mean, na.rm = TRUE))

SampleB$T6_BFI_conscientious<-T6_BFI_conscientious$MEAN

psych::alpha(cbind((4-SampleB$T6_BFI_2_XS_extravert1),
                  SampleB$T6_BFI_2_XS_extravert2,
                  SampleB$T6_BFI_2_XS_extravert3)) #.61

T6_BFI_extraversion<-transform(cbind((4-SampleB$T6_BFI_2_XS_extravert1),
                                     SampleB$T6_BFI_2_XS_extravert2,
                                     SampleB$T6_BFI_2_XS_extravert3),
                               MEAN=apply(cbind((4-SampleB$T6_BFI_2_XS_extravert1),
                                                SampleB$T6_BFI_2_XS_extravert2,
                                                SampleB$T6_BFI_2_XS_extravert3),1, mean, na.rm = TRUE))

SampleB$T6_BFI_extraversion<-T6_BFI_extraversion$MEAN

psych::alpha(cbind(SampleB$T6_BFI_2_XS_neurotic1,
                   SampleB$T6_BFI_2_XS_neurotic2,
                   (4-SampleB$T6_BFI_2_XS_neurotic3))) #.77

T6_BFI_neurotic<-transform(cbind(SampleB$T6_BFI_2_XS_neurotic1,
                                 SampleB$T6_BFI_2_XS_neurotic2,
                                 (4-SampleB$T6_BFI_2_XS_neurotic3)),
                           MEAN=apply(cbind(SampleB$T6_BFI_2_XS_neurotic1,
                                            SampleB$T6_BFI_2_XS_neurotic2,
                                            (4-SampleB$T6_BFI_2_XS_neurotic3)),1, mean, na.rm = TRUE))

SampleB$T6_BFI_neurotic<-T6_BFI_neurotic$MEAN

#Remove participants without data used in analyses 
SampleB<-subset(SampleB,!is.na(SampleB$behaviors))
nrow(SampleB) #N=405

#Calculate demographics
summary(SampleB$T1_Age)
sd(SampleB$T1_Age,na.rm=T)
summary(SampleB$gender)/nrow(SampleB)
summary(SampleB$ethnicity)/nrow(SampleB)
summary(SampleB$party)/nrow(SampleB)
droplevels(SampleB$party)

#Table 1
#Descriptive Statistics of Key Study Variables  
psych::describe(SampleB$T6_BFI_agree)
psych::describe(SampleB$T6_BFI_open)
psych::describe(SampleB$T6_BFI_conscientious)
psych::describe(SampleB$T6_BFI_extraversion)
psych::describe(SampleB$T6_BFI_neurotic)
psych::describe(SampleB$behaviors)

#z-standardize predictors and outcomes (units of between-person SDs)
SampleB$A<-scale(SampleB$T6_BFI_agree)
SampleB$O<-scale(SampleB$T6_BFI_open)
SampleB$C<-scale(SampleB$T6_BFI_conscientious)
SampleB$E<-scale(SampleB$T6_BFI_extraversion)
SampleB$N<-scale(SampleB$T6_BFI_neurotic)
SampleB$education<-scale(SampleB$T1_Education)
SampleB$age<-scale(SampleB$T1_Age)
SampleB$behaviors<-scale(SampleB$behaviors)
SampleB$fbehaviors<-scale(SampleB$fbehaviors)

#Table 2
#Primary Analyses: Personality Predictors of COVID-19 Preventive Health Behaviors 
a.model<-lm(behaviors ~ A+age+education+gender+ethnicity+party,data=SampleB)
o.model<-lm(behaviors ~ O+age+education+gender+ethnicity+party,data=SampleB)
c.model<-lm(behaviors ~ C+age+education+gender+ethnicity+party,data=SampleB)
e.model<-lm(behaviors ~ E+age+education+gender+ethnicity+party,data=SampleB)
n.model<-lm(behaviors ~ N+age+education+gender+ethnicity+party,data=SampleB)
nc.model<-lm(behaviors ~ N*C+age+education+gender+ethnicity+party,data=SampleB)

#coefficients and test statitistics 
summary(a.model)
summary(o.model)
summary(c.model)
summary(e.model)
summary(n.model)
summary(nc.model)

#95% CIs
confint(a.model, level = 0.95)
confint(o.model, level = 0.95)
confint(c.model, level = 0.95)
confint(e.model, level = 0.95)
confint(n.model, level = 0.95)
confint(nc.model, level = 0.95)

#Table 3
#Sensitivity Analyses: Personality Predictors of COVID-19 Preventive Health Behaviors

#Panel A. Sensitivity Analysis 1: Simple Effects without Covariate Adjustment 
a.model1<-lm(behaviors ~ A,data=SampleB)
o.model1<-lm(behaviors ~ O,data=SampleB)
c.model1<-lm(behaviors ~ C,data=SampleB)
e.model1<-lm(behaviors ~ E,data=SampleB)
n.model1<-lm(behaviors ~ N,data=SampleB)

#coefficients and test statitistics 
summary(a.model1)
summary(o.model1)
summary(c.model1)
summary(e.model1)
summary(n.model1)

#95% CIs
confint(a.model1, level = 0.95)
confint(o.model1, level = 0.95)
confint(c.model1, level = 0.95)
confint(e.model1, level = 0.95)
confint(n.model1, level = 0.95)

#Panel B. Sensitivity Analysis 2: Unique Effects Controlling for Other Big Five Traits 
sim.model<-lm(behaviors ~ A+O+C+E+N,data=SampleB)

#coefficients and test statitistics 
summary(sim.model)

#95% CIs
confint(sim.model)

#Panel C. Sensitivity Analysis 3: Predicting Intended Future Behaviors 
a.model3<-lm(fbehaviors ~ A+age+education+gender+ethnicity+party,data=SampleB)
o.model3<-lm(fbehaviors ~ O+age+education+gender+ethnicity+party,data=SampleB)
c.model3<-lm(fbehaviors ~ C+age+education+gender+ethnicity+party,data=SampleB)
e.model3<-lm(fbehaviors ~ E+age+education+gender+ethnicity+party,data=SampleB)
n.model3<-lm(fbehaviors ~ N+age+education+gender+ethnicity+party,data=SampleB)

#coefficients and test statitistics 
summary(a.model3)
summary(o.model3)
summary(c.model3)
summary(e.model3)
summary(n.model3)

#95% CIs
confint(a.model3, level = 0.95)
confint(o.model3, level = 0.95)
confint(c.model3, level = 0.95)
confint(e.model3, level = 0.95)
confint(n.model3, level = 0.95)

#Exploratory Effect Size Comparison
m1 <- lm(behaviors ~ I(O + A), data = SampleB)
m2 <- lm(behaviors ~ O + A, data = SampleB)
anova(m1, m2) #.207

m1 <- lm(behaviors ~ I(O + C), data = SampleB)
m2 <- lm(behaviors ~ O + C, data = SampleB)
anova(m1, m2) 

m1 <- lm(behaviors ~ I(O + E), data = SampleB)
m2 <- lm(behaviors ~ O + E, data = SampleB)
anova(m1, m2) 

m1 <- lm(behaviors ~ I(O + N), data = SampleB)
m2 <- lm(behaviors ~ O + N, data = SampleB)
anova(m1, m2) 

m1 <- lm(behaviors ~ I(A + C), data = SampleB)
m2 <- lm(behaviors ~ A + C, data = SampleB)
anova(m1, m2) 

m1 <- lm(behaviors ~ I(A + E), data = SampleB)
m2 <- lm(behaviors ~ A + E, data = SampleB)
anova(m1, m2) 

m1 <- lm(behaviors ~ I(A + N), data = SampleB)
m2 <- lm(behaviors ~ A + N, data = SampleB)
anova(m1, m2)

m1 <- lm(behaviors ~ I(E + C), data = SampleB)
m2 <- lm(behaviors ~ E + C, data = SampleB)
anova(m1, m2) 

m1 <- lm(behaviors ~ I(E + N), data = SampleB)
m2 <- lm(behaviors ~ E + N, data = SampleB)
anova(m1, m2) 

m1 <- lm(behaviors ~ I(C + N), data = SampleB)
m2 <- lm(behaviors ~ C + N, data = SampleB)
anova(m1, m2) 

#Additional Exploratory Analyses 
a.age<-lm(behaviors ~ A*age+education+gender+ethnicity+party,data=SampleB)
a.gender<-lm(behaviors ~ A*gender+age+education+ethnicity+party,data=SampleB)
a.education<-lm(behaviors ~ A*education+age+gender+ethnicity+party,data=SampleB)
a.ethnicity<-lm(behaviors ~ A*ethnicity+age+education+gender+party,data=SampleB)
a.party<-lm(behaviors ~ A*party+age+education+gender+ethnicity,data=SampleB)

o.age<-lm(behaviors ~ O*age+education+gender+ethnicity+party,data=SampleB)
o.gender<-lm(behaviors ~ O*gender+age+education+ethnicity+party,data=SampleB)
o.education<-lm(behaviors ~ O*education+age+gender+ethnicity+party,data=SampleB)
o.ethnicity<-lm(behaviors ~ O*ethnicity+age+education+gender+party,data=SampleB)
o.party<-lm(behaviors ~ O*party+age+education+gender+ethnicity,data=SampleB)

c.age<-lm(behaviors ~ C*age+education+gender+ethnicity+party,data=SampleB)
c.gender<-lm(behaviors ~ C*gender+age+education+ethnicity+party,data=SampleB)
c.education<-lm(behaviors ~ C*education+age+gender+ethnicity+party,data=SampleB)
c.ethnicity<-lm(behaviors ~ C*ethnicity+age+education+gender+party,data=SampleB)
c.party<-lm(behaviors ~ C*party+age+education+gender+ethnicity,data=SampleB)

e.age<-lm(behaviors ~ E*age+education+gender+ethnicity+party,data=SampleB)
e.gender<-lm(behaviors ~ E*gender+age+education+ethnicity+party,data=SampleB)
e.education<-lm(behaviors ~ E*education+age+gender+ethnicity+party,data=SampleB)
e.ethnicity<-lm(behaviors ~ E*ethnicity+age+education+gender+party,data=SampleB)
e.party<-lm(behaviors ~ E*party+age+education+gender+ethnicity,data=SampleB)

n.age<-lm(behaviors ~ N*age+education+gender+ethnicity+party,data=SampleB)
n.gender<-lm(behaviors ~ N*gender+age+education+ethnicity+party,data=SampleB)
n.education<-lm(behaviors ~ N*education+age+gender+ethnicity+party,data=SampleB)
n.ethnicity<-lm(behaviors ~ N*ethnicity+age+education+gender+party,data=SampleB)
n.party<-lm(behaviors ~ N*party+age+education+gender+ethnicity,data=SampleB)

summary(a.age)
summary(a.gender)
summary(a.education)
anova(a.ethnicity)
anova(a.party)

summary(o.age)
summary(o.gender)
summary(o.education)
anova(o.ethnicity)
anova(o.party)

summary(c.age) # 
summary(c.gender)
summary(c.education)
anova(c.ethnicity)
anova(c.party)

summary(e.age)
summary(e.gender)
summary(e.education)
anova(e.ethnicity)
anova(e.party)

summary(n.age)
summary(n.gender)
summary(n.education) #
anova(n.ethnicity)
anova(n.party)

#Supplementary Digital Content Table 1
#Personality Predictors of Individual COVID-19 Preventive Health Behaviors 

a.model4.mask<-lm(scale(T6_ActPast_Prevent_Mask) ~ A+age+education+gender+ethnicity+party,data=SampleB)
o.model4.mask<-lm(scale(T6_ActPast_Prevent_Mask) ~ O+age+education+gender+ethnicity+party,data=SampleB)
c.model4.mask<-lm(scale(T6_ActPast_Prevent_Mask) ~ C+age+education+gender+ethnicity+party,data=SampleB)
e.model4.mask<-lm(scale(T6_ActPast_Prevent_Mask) ~ E+age+education+gender+ethnicity+party,data=SampleB)
n.model4.mask<-lm(scale(T6_ActPast_Prevent_Mask) ~ N+age+education+gender+ethnicity+party,data=SampleB)

a.model4.self<-lm(scale(T6_ActPast_Prevent_Self) ~ A+age+education+gender+ethnicity+party,data=SampleB)
o.model4.self<-lm(scale(T6_ActPast_Prevent_Self) ~ O+age+education+gender+ethnicity+party,data=SampleB)
c.model4.self<-lm(scale(T6_ActPast_Prevent_Self) ~ C+age+education+gender+ethnicity+party,data=SampleB)
e.model4.self<-lm(scale(T6_ActPast_Prevent_Self) ~ E+age+education+gender+ethnicity+party,data=SampleB)
n.model4.self<-lm(scale(T6_ActPast_Prevent_Self) ~ N+age+education+gender+ethnicity+party,data=SampleB)

a.model4.sanitize<-lm(scale(T6_ActPast_Prevent_Sanitize) ~ A+age+education+gender+ethnicity+party,data=SampleB)
o.model4.sanitize<-lm(scale(T6_ActPast_Prevent_Sanitize) ~ O+age+education+gender+ethnicity+party,data=SampleB)
c.model4.sanitize<-lm(scale(T6_ActPast_Prevent_Sanitize) ~ C+age+education+gender+ethnicity+party,data=SampleB)
e.model4.sanitize<-lm(scale(T6_ActPast_Prevent_Sanitize) ~ E+age+education+gender+ethnicity+party,data=SampleB)
n.model4.sanitize<-lm(scale(T6_ActPast_Prevent_Sanitize) ~ N+age+education+gender+ethnicity+party,data=SampleB)

a.model4.distance<-lm(scale(T6_ActPast_SocialDistance) ~ A+age+education+gender+ethnicity+party,data=SampleB)
o.model4.distance<-lm(scale(T6_ActPast_SocialDistance) ~ O+age+education+gender+ethnicity+party,data=SampleB)
c.model4.distance<-lm(scale(T6_ActPast_SocialDistance) ~ C+age+education+gender+ethnicity+party,data=SampleB)
e.model4.distance<-lm(scale(T6_ActPast_SocialDistance) ~ E+age+education+gender+ethnicity+party,data=SampleB)
n.model4.distance<-lm(scale(T6_ActPast_SocialDistance) ~ N+age+education+gender+ethnicity+party,data=SampleB)

a.model4.isolate<-lm(scale(T6_ActPast_SelfIsolate) ~ A+age+education+gender+ethnicity+party,data=SampleB)
o.model4.isolate<-lm(scale(T6_ActPast_SelfIsolate) ~ O+age+education+gender+ethnicity+party,data=SampleB)
c.model4.isolate<-lm(scale(T6_ActPast_SelfIsolate) ~ C+age+education+gender+ethnicity+party,data=SampleB)
e.model4.isolate<-lm(scale(T6_ActPast_SelfIsolate) ~ E+age+education+gender+ethnicity+party,data=SampleB)
n.model4.isolate<-lm(scale(T6_ActPast_SelfIsolate) ~ N+age+education+gender+ethnicity+party,data=SampleB)

summary(a.model4.mask)
confint(a.model4.mask, level = 0.95)
summary(o.model4.mask)
confint(o.model4.mask, level = 0.95)
summary(c.model4.mask)
confint(c.model4.mask, level = 0.95)
summary(e.model4.mask)
confint(e.model4.mask, level = 0.95)
summary(n.model4.mask)
confint(n.model4.mask, level = 0.95)

summary(a.model4.self)
confint(a.model4.self, level = 0.95)
summary(o.model4.self)
confint(o.model4.self, level = 0.95)
summary(c.model4.self)
confint(c.model4.self, level = 0.95)
summary(e.model4.self)
confint(e.model4.self, level = 0.95)
summary(n.model4.self)
confint(n.model4.self, level = 0.95)

summary(a.model4.sanitize)
confint(a.model4.sanitize, level = 0.95)
summary(o.model4.sanitize)
confint(o.model4.sanitize, level = 0.95)
summary(c.model4.sanitize)
confint(c.model4.sanitize, level = 0.95)
summary(e.model4.sanitize)
confint(e.model4.sanitize, level = 0.95)
summary(n.model4.sanitize)
confint(n.model4.sanitize, level = 0.95)

summary(a.model4.distance)
confint(a.model4.distance, level = 0.95)
summary(o.model4.distance)
confint(o.model4.distance, level = 0.95)
summary(c.model4.distance)
confint(c.model4.distance, level = 0.95)
summary(e.model4.distance)
confint(e.model4.distance, level = 0.95)
summary(n.model4.distance)
confint(n.model4.distance, level = 0.95)

summary(a.model4.isolate)
confint(a.model4.isolate, level = 0.95)
summary(o.model4.isolate)
confint(o.model4.isolate, level = 0.95)
summary(c.model4.isolate)
confint(c.model4.isolate, level = 0.95)
summary(e.model4.isolate)
confint(e.model4.isolate, level = 0.95)
summary(n.model4.isolate)
confint(n.model4.isolate, level = 0.95)

#Supplementary Digital Content Table 2
#Simple Associations between Demographic Characteristics and Health Behaviors   
summary(lm(behaviors ~ age,data=SampleB))
summary(lm(behaviors ~ education,data=SampleB))
summary(lm(behaviors ~ gender,data=SampleB))
summary(lm(behaviors ~ ethnicity,data=SampleB))
summary(lm(behaviors ~ relevel(ethnicity,ref="W"),data=SampleB))
summary(lm(behaviors ~ relevel(ethnicity,ref="EA"),data=SampleB))
summary(lm(behaviors ~ party,data=SampleB))

#Supplementary Digital Content Table 3
#Simple Associations between Demographic Characteristics and Agreeablenes   
summary(lm(A ~ age,data=SampleB))
summary(lm(A ~ education,data=SampleB))
summary(lm(A ~ gender,data=SampleB))
summary(lm(A ~ ethnicity,data=SampleB))
summary(lm(A ~ relevel(ethnicity,ref="W"),data=SampleB))
summary(lm(A ~ relevel(ethnicity,ref="EA"),data=SampleB))
summary(lm(A ~ party,data=SampleB))

#Supplementary Digital Content Table 4
#Simple Associations between Demographic Characteristics and Openness   
summary(lm(O ~ age,data=SampleB))
summary(lm(O ~ education,data=SampleB))
summary(lm(O ~ gender,data=SampleB))
summary(lm(O ~ ethnicity,data=SampleB))
summary(lm(O ~ relevel(ethnicity,ref="W"),data=SampleB))
summary(lm(O ~ relevel(ethnicity,ref="EA"),data=SampleB))
summary(lm(O ~ party,data=SampleB))

#Supplementary Digital Content Table 5
#Simple Associations between Demographic Characteristics and Conscientiousness    
summary(lm(C ~ age,data=SampleB))
summary(lm(C ~ education,data=SampleB))
summary(lm(C ~ gender,data=SampleB))
summary(lm(C ~ ethnicity,data=SampleB))
summary(lm(C ~ relevel(ethnicity,ref="W"),data=SampleB))
summary(lm(C ~ relevel(ethnicity,ref="EA"),data=SampleB))
summary(lm(C ~ party,data=SampleB))

#Supplementary Digital Content Table 6
#Simple Associations between Demographic Characteristics and Extraversion    
summary(lm(E ~ age,data=SampleB))
summary(lm(E ~ education,data=SampleB))
summary(lm(E ~ gender,data=SampleB))
summary(lm(E ~ ethnicity,data=SampleB))
summary(lm(E ~ relevel(ethnicity,ref="W"),data=SampleB))
summary(lm(E ~ relevel(ethnicity,ref="EA"),data=SampleB))
summary(lm(E ~ party,data=SampleB))

#Supplementary Digital Content Table 7
#Simple Associations between Demographic Characteristics and Neuroticism    
summary(lm(N ~ age,data=SampleB))
summary(lm(N ~ education,data=SampleB))
summary(lm(N ~ gender,data=SampleB))
summary(lm(N ~ ethnicity,data=SampleB))
summary(lm(N ~ relevel(ethnicity,ref="W"),data=SampleB))
summary(lm(N ~ relevel(ethnicity,ref="EA"),data=SampleB))
summary(lm(N ~ party,data=SampleB))

