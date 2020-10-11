
#Load libraries
library(leaps)
library(glmnet)
library(class)
library(mlbench)

library(car)
library(party)
library(dummies)
library(haven)
library(foreach)
library(dplyr)
library(rpart)
library(mlr)
library(mlrMBO)

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
sample.exclude = toilet.share ==-8 | toilet.share ==-5  | roof.material == -5 | roof.material == -13 | 
  wall.material == -13 | owned == -8 | water.source ==-8 | toilet.type == -8 | cellphone.use == -5 |
  refuse.removal == -5 | street.light == -3 | radio == -8 | hifi == -8 | satelite == -8 | tv == -8 | 
  camera == -8 | computer == -8 | video == -8 | cellphone.own == -8 | tv.les == -8 | gas.stove == -8 |
  microwave == -8 | fridge == -8 | washer == -8 | dwelling.type == 5
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

attach(hhdata.full, warn.conflicts = F)


compare.rmse = function(model,training,testing) {
  cv.train.rmse = model$results$RMSE
  cv.train.rmse.sd = model$results$RMSESD
  train.rmse = sqrt(mean((training$income - predict(model,training))^2))
  test.rmse = sqrt(mean((testing$income - predict(model,testing))^2))
  rmse=c(cv.train.rmse,cv.train.rmse.sd,train.rmse,test.rmse)
  names(rmse)=c("CV RMSE","CV RMSE Std. Dev.","Training RMSE","Testing RMSE")
  print(rmse)
}




set.seed(7)
inTrain = createDataPartition(y=income,p=0.75,list=F)
training= hhdata.full[inTrain,]
testing= hhdata.full[-inTrain,]
formula = income~.


parallelStartSocket(min(6,detectCores()-1))

regr.task = makeRegrTask(id = "NIDS income regression with rpart", data = training, target = "income")
regr.task
getTaskDesc(regr.task)
#str(getTaskData(regr.task))
#removeConstantFeatures(regr.task)

rdesc = makeResampleDesc("CV", folds =10)
rdesc = makeResampleDesc("CV", iter = 5)
#r = resample("regr.rpart", regr.task, rdesc)
measure = rmse
listLearners(regr.task)
listMeasures(regr.task)

mbo.ctrl = makeMBOControl()
mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 25L)


# rpart
getParamSet("regr.rpart")
regr.lrn = makeLearner("regr.rpart")
rpart.num_ps = makeParamSet(
  makeNumericParam("cp", lower = 0, upper = 0.2),
  makeIntegerParam("maxdepth", lower = 1, upper = 30),
  makeIntegerParam("minsplit", lower = 1, upper = 100)
)
#res = tuneParams("regr.rpart", task = regr.task, resampling = rdesc, measures=rmse,
#                 par.set = num_ps, control = ctrl)
design.mat = generateRandomDesign(n = 500, par.set = num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.rpart = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                       measures = rmse, par.set = num_ps, control = ctrl)

# cp=0.00117; maxdepth=14; minsplit=53 : rmse.test.rmse=0.815

#ctree
getParamSet("regr.ctree")
regr.lrn = makeLearner("regr.ctree")
ctree.num_ps = makeParamSet(
  makeNumericParam("mincriterion", lower = 0, upper = 1),
  makeIntegerParam("maxdepth", lower = 1, upper = 30),
  makeIntegerParam("minsplit", lower = 1, upper = 100)
)
res = tuneParams("regr.rpart", task = regr.task, resampling = rdesc, measures=rmse,
                 par.set = num_ps, control = ctrl)
mbo.ctrl = makeMBOControl()
mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 25L)
design.mat = generateRandomDesign(n = 100, par.set = ctree.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                             measures = rmse, par.set = ctree.num_ps, control = ctrl)
#mincriterion=0.0805; maxdepth=10; minsplit=64 : rmse.test.rmse=0.815

#regr.mob
# This did not run: 
parallelStartSocket(min(6,detectCores()-1))
getParamSet("regr.mob")
regr.lrn = makeLearner("regr.mob")
mob.tsk = makeRegrTask("nids", data = as.data.frame(lapply(training, as.numeric)), target = "income")
mob.num_ps = makeParamSet(
  makeNumericParam("alpha", lower = 0, upper = 1),
  makeNumericParam("trim", lower = 0, upper = 1),
  makeIntegerParam("minsplit", lower = 1, upper = 100)
)
design.mat = generateRandomDesign(n = 100, par.set = mob.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = mob.tsk, resampling = rdesc,
                             measures = rmse,  show.info = TRUE, par.set = mob.num_ps, control = ctrl)
parallelStop()



# other simple tree models include: 
#regr.nodeHarvest 
#regr.bcart
#regr.evtree
#regr.btlm 
#regr.bartMachine
#regr.earth 

getParamSet("regr.evtree")

#Bagging & Random Forests

#regr.ranger
parallelStartSocket(min(6,detectCores()-1))
getParamSet("regr.ranger")
regr.lrn = makeLearner("regr.ranger")
ranger.num_ps = makeParamSet(
  makeIntegerParam("num.trees", lower = 1, upper = 1000),
  makeIntegerParam("mtry", lower = 1, upper = 190),
  makeIntegerParam("min.node.size", lower = 1, upper = 100),
  makeNumericParam("sample.fraction", lower = 0, upper = 1),
  makeNumericParam("alpha", lower = 0, upper = 1)
)
design.mat = generateRandomDesign(n = 100, par.set = ranger.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                             measures = rmse,  show.info = TRUE, par.set = ranger.num_ps, control = ctrl)
#num.trees=775; mtry=78; min.node.size=8; sample.fraction=0.576; alpha=0.931 : rmse.test.rmse=0.72
parallelStop()

#regr.randomForest
parallelStartSocket(min(6,detectCores()-1))
getParamSet("regr.randomForest")
regr.lrn = makeLearner("regr.randomForest")
randomForest.num_ps = makeParamSet(
  makeIntegerParam("num.trees", lower = 1, upper = 1000),
  makeIntegerParam("mtry", lower = 1, upper = 190),
  makeIntegerParam("min.node.size", lower = 1, upper = 100),
  makeNumericParam("sample.fraction", lower = 0, upper = 1),
  makeNumericParam("alpha", lower = 0, upper = 1)
)
design.mat = generateRandomDesign(n = 100, par.set = randomForest.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                             measures = rmse,  show.info = TRUE, par.set = randomForest.num_ps, control = ctrl)
#num.trees=775; mtry=78; min.node.size=8; sample.fraction=0.576; alpha=0.931 : rmse.test.rmse=0.72
parallelStop()

regr.extraTrees
regr.h2o.randomForest
regr.randomForest 
regr.randomForestSRC
regr.RRF 

#Neural nets:
#regr.elmNN  
parallelStartSocket(min(6,detectCores()-1))
getParamSet("regr.elmNN")
regr.lrn = makeLearner("regr.elmNN")
elmNN.num_ps = makeParamSet(
  makeIntegerParam("nhid", lower = 1, upper = 1000)
)
design.mat = generateRandomDesign(n = 100, par.set = elmNN.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                             measures = rmse,  show.info = TRUE, par.set = elmNN.num_ps, control = ctrl)
parallelStop()
# nhid=36 : rmse.test.rmse=1.15


#regr.brnn
# yet to estimate
parallelStartSocket(min(6,detectCores()-1))
getParamSet("regr.brnn")
regr.lrn = makeLearner("regr.elmNN")
elmNN.num_ps = makeParamSet(
  makeIntegerParam("nhid", lower = 1, upper = 1000)
)
design.mat = generateRandomDesign(n = 100, par.set = elmNN.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                             measures = rmse,  show.info = TRUE, par.set = elmNN.num_ps, control = ctrl)
parallelStop()
# nhid=36 : rmse.test.rmse=1.15

#regr.cubist
getParamSet("regr.cubist")
regr.lrn = makeLearner("regr.cubist")
cubist.num_ps = makeParamSet(
  makeIntegerParam("committees", lower = 1, upper = 100),
  makeIntegerParam("neighbors", lower = 0, upper = 9)
)
design.mat = generateRandomDesign(n = 100, par.set = cubist.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                             measures = rmse, par.set = cubist.num_ps, control = ctrl)
#committees=98; neighbors=0; rmse.test.rmse=0.709


regr.bst
blackboost

#regr.gbm
getParamSet("regr.gbm")
regr.lrn = makeLearner("regr.gbm")
gbm.num_ps = makeParamSet(
  makeIntegerParam(id = "n.trees", lower = 100,upper = 1000),
  makeIntegerParam(id = "interaction.depth", lower = 1, upper = 30),
  makeNumericParam(id = "shrinkage", lower = .001, upper = .5),
  makeNumericParam(id = "train.fraction", lower = .33, upper = 1)
)
design.mat = generateRandomDesign(n = 100, par.set = gbm.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = regr.task, resampling = rdesc,
                             measures = rmse, show.info = TRUE, par.set = gbm.num_ps, control = ctrl)
#n.trees=676; interaction.depth=14; shrinkage=0.0243; train.fraction=0.736: rmse.test.rmse= 0.7

parallelStop()

#regr.xgboost
xbgoost.task = makeRegrTask("nids", data = as.data.frame(lapply(training, as.numeric)), target = "income")
getParamSet("regr.xgboost")
regr.lrn = makeLearner("regr.xgboost")
xgboost.num_ps = makeParamSet(
  makeIntegerParam(id = "nrounds", lower = 1,upper = 80),
  makeIntegerParam(id = "max_depth", lower = 2, upper = 15),
  makeNumericParam(id = "eta", lower = .01, upper = .4)
)
design.mat = generateRandomDesign(n = 500, par.set = xgboost.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)
tune.pars.ctree = tuneParams(learner = regr.lrn, task = xbgoost.task, resampling = rdesc,
                             measures = rmse, show.info = TRUE, par.set = xgboost.num_ps, control = ctrl)
#nrounds=57; max_depth=3; eta=0.196 : rmse.test.rmse=0.707
#much quicker than cubist
parallelStop()


detach("package:mlr", unload=TRUE)
detach("package:mlrMBO", unload=TRUE)
