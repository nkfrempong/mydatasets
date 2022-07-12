library(haven)
library(tidyverse)
library(ggplot2)
library(plotly)
library(magrittr)
library(zoo)
library(ggthemes)
library(ggpubr)
library(ggsci)
library(psych)
library(dplyr)
library(fBasics)
library(Hmisc)
library(MASS)
library(car)
snake=read_sav(file.choose())
View(snake)
str(snake)
attach(snake)

snake1= snake %>% 
  mutate(
    Gender=as.factor(Gender), 
    Dependence=as.factor(Dependence),
    Age=as.factor(Age), 
    Occupation=as.factor(Occupation),
    Marital=as.factor(Marital),   
    Annual_income=as.factor(Annual_income),
    Education=as.factor(Education),                  
    Time_present=as.factor(Time_present),              
    Site=as.factor(Site),                       
    Location=as.factor(Location),            
    Activity=as.factor(Activity),                 
    Time_of_bite=as.factor(Time_of_bite),           
    Symptoms=as.factor(Symptoms),             
    First_aid=as.factor(First_aid),          
    Time_lapsed=as.factor(Time_lapsed),         
    First_aid_type=as.factor(First_aid_type),      
    means_to_hosp=as.factor(means_to_hosp),              
    Clinical_symptoms=as.factor(Clinical_symptoms),       
    Treatment=as.factor(Treatment),              
    Site1=as.factor(Site1),  
    Treatment_type=as.factor(Treatment_type), 
    ATS=as.factor(ATS),
    Reaction=as.factor(Reaction),
    Antibiotics=as.factor(Antibiotics),     
    LOS=as.factor(LOS),            
  )
str(snake1)
#snake1$Gender %<>% ff_label("Gender")
#snake$Age %<>% ff_label("Age")
attach(snake1)
snake1$Age1<- fct_collapse(snake1$Age,"0-15" =c("1","2","3"),
                           "16-25" =c("4","5"),
                           "26-40"=c("6","7","8"),
                           "41-50"=c("9","10"),
                           ">=51"=c("11","12"))
snake1$means_to_hosp1= fct_collapse(snake1$means_to_hosp,"Direct"="1",
                                    "Transfer" =c("2","3"))
snake1$Treatment_type1<- fct_collapse(snake1$Treatment_type,
                                      "Polyvalent" =c("ployvelent[Afriven-10]","polychand",
                                                      "polynland","polyvalent (panaf)","polyvalent[Afriven",
                                                      "polyvalent[Afriven IV]","polyvelent","polyvelent (Afriven 10 vins)",
                                                      "polyvelent 20units","polyvelent IV","polyvelent[panat]"),
                                      "ASV" =c("ASV 20mls","ASV 3L N/S","ASV 40ml","ASV 40ml in 200mls",
                                               "ASV and hydroantisine 200l","ASV in 250 N/S + Havie 100mg Hydroci","ASV in 250mls N/S",
                                               "ASV in 500m/s of Ns","IV ASV 20m/s in 250m/s 7N/S","ST","trauexali acid","w hydrocort 100mg 7 premed"))

snake1$LOS1<- fct_collapse(snake1$LOS,"1" = "1",
                           "2" = "2",
                           "3"= "3",
                           "4"=c("4","5"))
snake1$Time_lapsed1<- fct_collapse(snake1$Time_lapsed,"1" = c("1","2","3","4","5","6"),"0"=c("7","8","11"))

mod1.ord=polr(as.ordered(LOS1)~ Gender+Dependence+Age1+Marital+Education+Site1+First_aid+
                means_to_hosp1+Treatment+Treatment_type1+Time_lapsed1+ATS,data=snake1,method='logistic')
summary(mod1)
Anova(mod1)
mod2.ord= polr(as.ordered(LOS1)~ Gender+Dependence+Age1+Marital+Education+Site1+First_aid+
                 Treatment,data=snake3,method='logistic')
mod.fit.ord.null <- update(mod1.ord, ~ . -c(means_to_hosp1,Treatment_type1,Time_lapsed1,ATS))
anova(mod.fit.ord.null, mod1.ord, test="Chisq")
Anova(mod2.ord)
summary(mod2.ord)
pi.hat.ord <- predict(mod2.ord , type = "probs")
head (pi.hat.ord )
pi.hat.ord1 <- predict(mod2.ord , type = "class")
head (pi.hat.ord1)
