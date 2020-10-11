#Load libraries
library(leaps)
library(glmnet)
library(class)
library(mlbench)
library(caret)
library(car)
library(rattle)
library(party)
library(dummies)
library(haven)
library(foreach)
library(dplyr)
library(rpart)
library(parallel)
library(doParallel)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_131')

#Remove lists from memory and load data
rm(list = ls())
library(haven)
NIDSdata <- read_dta("D:/My Documents/Reading group/Machine learning/R Resources/Data/NIDS/NIDS_data_w1.dta")
save(NIDSdata, file="D:/My Documents/Reading group/Machine learning/R Resources/Data/NIDS/NIDSdata.rda")
load("D:/My Documents/Reading group/Machine learning/R Resources/Data/NIDS/NIDSdata.rda")
attach(NIDSdata, warn.conflicts = F)

# Create hhdata.full, which drops variables that contain many missing values, drops observations with missing values, 
# creates a sensibly ordered head.educ variable, and generates dummies for all categorical variables
attr(NIDSdata$head_educ,"labels")
table(NIDSdata$head_educ)
NIDSdata$head_educ = ifelse(NIDSdata$head_educ==25,0,ifelse(NIDSdata$head_educ>=13 & NIDSdata$head_educ<=17,11,ifelse(NIDSdata$head_educ>=21 & NIDSdata$head_educ<=23,NIDSdata$head_educ-6,
                                                                                                                      ifelse(NIDSdata$head_educ>=18 & NIDSdata$head_educ<=19,13,ifelse(NIDSdata$head_educ==20,13,NIDSdata$head_educ)))))
is.na(NIDSdata$head_educ) = NIDSdata$head_educ <0 | NIDSdata$head_educ == 24
is.na(NIDSdata$head_age) = NIDSdata$head_age < 0
hhdata.full = data.frame(income= log(hhincome/hhsizer),members=hhsizer,rooms=h_dwlrms,dwelling=h_dwltyp,prov=hhprov, roof=h_dwlmatroof, wall=h_dwlmatrwll, dwelling.own=h_ownd, water=h_watsrc, toilet=h_toi, 
                         toilet.share=h_toishr, electricity=h_enrgelec, landline=h_tellnd, cellphone.use=h_telcel, refuse.removal=h_refrem, street.light=h_strlght, 
                         radio=h_ownrad, hifi=h_ownhif, tv=h_owntel, satelite=h_ownsat, video=h_ownvid, computer=h_owncom, camera=h_owncam, cellphone.own=h_owncel, tv.les=h_ownelestv, 
                         gas.stove=h_owngasstv, microwave=h_ownmic, fridge=h_ownfrg, washer=h_ownwsh,district.council=district_council,head.absent=head_absent,head = head_gender - 1, head.educ=head_educ, head.age=head_age)
for (i in 2:dim.data.frame(hhdata.full)[2]) {hhdata.full[,i]=as.integer(hhdata.full[,i])}
for (i in 4:(dim.data.frame(hhdata.full)[2]-2)) {hhdata.full[,i]=as.factor(hhdata.full[,i])}
hhdata.full$income=as.numeric(hhdata.full$income)
lm.fit.full = lm(income~.,data=hhdata.full)
summary(lm.fit.full)
sample.omit = lm.fit.full$na.action
hhdata.full = hhdata.full[-sample.omit,]

attach(hhdata.full, warn.conflicts = F)
sample.exclude = toilet.share ==-8 | toilet.share ==-5  | roof == -5 | roof == -13 | 
  wall == -13 | dwelling.own == -8 | water ==-8 | toilet == -8 | cellphone.use == -5 |
  refuse.removal == -5 | street.light == -3 | radio == -8 | hifi == -8 | satelite == -8 | tv == -8 | 
  camera == -8 | computer == -8 | video == -8 | cellphone.own == -8 | tv.les == -8 | gas.stove == -8 |
  microwave == -8 | fridge == -8 | washer == -8 | dwelling == 5
hhdata.full = hhdata.full[which(sample.exclude==F),]
hhdata.full = droplevels(hhdata.full)
levels(hhdata.full$dwelling) <- c('na', 'house', 'hut', 'flat', 'semidetached', 'backyard.house', 'backyard.shack', 'shack', 'flatlet', 'caravan')
levels(hhdata.full$prov) <- c('Western.Cape', 'Eastern.Cape', 'Northern.Cape', 'Free.State', 'KwaZulu.Natal', 'North.West', 'Gauteng', 'Mpumalanga', 'Limpopo')
levels(hhdata.full$roof) <- c('na','brick','concrete','iron','wood','plastic','cardboard','mudcement','wattle','tile','mudbricks','thatching','sheeting','rock')
levels(hhdata.full$wall) <- c('na','brick','concrete','iron','wood','plastic','cardboard','mudcement','wattle','tile','mudbricks','thatching','sheeting','rock')
levels(hhdata.full$dwelling.own) <- c('yes', 'no')
levels(hhdata.full$water) <- c('na','piped','yard','public','tanker','priv.borehole','com.borehole','rain.tank','stream','dam','well','spring','na','neighbour')
levels(hhdata.full$toilet) <- c('na','na','flush.onsite','flush.offsite','chemical','pit.ventilation','pit.noventilation','bucket','na')
levels(hhdata.full$toilet.share) <- c('na', 'yes', 'no')
levels(hhdata.full$electricity) <- c('na', 'yes', 'no')
levels(hhdata.full$landline) <- c('na', 'working', 'not.working','no')
levels(hhdata.full$cellphone.use) <- c('na', 'yes', 'no')
levels(hhdata.full$refuse.removal) <- c('na', 'yes', 'no')
levels(hhdata.full$street.light) <- c('working', 'not.working','no')
levels(hhdata.full$radio) <- c('na', 'yes', 'no')
levels(hhdata.full$hifi) <- c('na', 'yes', 'no')
levels(hhdata.full$tv) <- c('na', 'yes', 'no')
levels(hhdata.full$satelite) <- c('na', 'yes', 'no')
levels(hhdata.full$video) <- c('na', 'yes', 'no')
levels(hhdata.full$computer) <- c('na', 'yes', 'no')
levels(hhdata.full$camera) <- c('na', 'yes', 'no')
levels(hhdata.full$cellphone.own) <- c('na', 'yes', 'no')
levels(hhdata.full$tv.les) <- c('na', 'yes', 'no')
levels(hhdata.full$gas.stove) <- c('na', 'yes', 'no')
levels(hhdata.full$microwave) <- c('na', 'yes', 'no')
levels(hhdata.full$fridge) <- c('na', 'yes', 'no')
levels(hhdata.full$washer) <- c('na', 'yes', 'no')

dc.labels = gsub(" District Municipality","",names(attr(NIDSdata$district_council,"labels")))
dc.labels=gsub(" Municipality","",dc.labels)
dc.labels=gsub(" Metropolitan","",dc.labels)
dc.labels=gsub("City Of ","",dc.labels)

levels(hhdata.full$district.council) <- dc.labels
levels(hhdata.full$head.absent) <- c('no','yes')
levels(hhdata.full$head) <- c('male', 'female')
hhdata.full = data.frame(hhdata.full[,1:3],hhdata.full[,33:34],dummy.data.frame(hhdata.full[,4:32], sep=".",names = NULL, omit.constants=TRUE, dummy.classes = getOption("dummy.classes")))
save(hhdata.full, file="NIDS.hhdata.full.rda")
hhdata.small = data.frame(hhdata.full[,1:3])
save(hhdata.small, file="NIDS.hhdata.small.rda")
