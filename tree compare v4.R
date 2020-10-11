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


cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#Linear regression (lm)
set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
lm.fit.time = system.time(lm.fit <- train(formula, data=training,method='lm',trControl=fitControl))
lm.fit.size = format(object.size(lm.fit),units="Mb")
lm.fit.rmse=compare.rmse(lm.fit,training,testing)

lm.fit.tab = c(round(lm.fit.time[3],2),lm.fit.size)
names(lm.fit.tab) = c("Time to run (sec)","Size to store (MB)")
lm.plot=plot(varImp(lm.fit))
summary(lm.fit$finalModel)

lm.fit.tab
lm.fit.rmse

# OLS achieves an R-squared of 0.61 and a CV RMSE of 0.77. Testing RMSE is insignificantly higher. Can any of the trees do better?
# The most significant predictors (p < 0.001) are: all the continuous variables (rooms, members, head.educ, head.age), 
# an amenity (landline), assets (hifi1, satelite1, video1, computer1,camera1, microwave1, washer1), some DCs, and female headship.
# These are the same variables that show up in the varImp function, since variable importance is derived from the t-statistics.

#Regression tree (rpart)

#Let's start by running a simple regression tree using rpart and conservative parameter values
set.seed(123)
rpart.simple.time = system.time(rpart.simple <- rpart(income ~ ., method="anova", data=training,control=rpart.control(minsplit = 20, minbucket = round(20/3), cp = 0.03, 
                                                                                                                      maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                                                                                      surrogatestyle = 0, maxdepth = 30)))
rpart.simple.size = format(object.size(rpart.simple),units="Mb")
rpart.simple.tab = c(round(rpart.simple.time[3],2),rpart.simple.size)
names(rpart.simple.tab) = c("Time to run (sec)","Size to store (MB)")

# Options minsplit, minbucket, cp and maxdepth are used to control the complexity of the model.
# maxcompete stores the results for all competing splits that were nearly enacted. Since the model investigates all possible split
# this paramter has no effect on running time. maxsurrogate specificies how many surrogates to explore for every primary split, which
# will determine which branch to use for observations that are missing the primary split variable, this does slow down the function.
# usersurrogate determines how to apply the information from the surrogates to allocating variables with missing split variables. xval
# determines the number of folds for cross-validation, which also slows down the function.

fancyRpartPlot(rpart.simple,main="Regression Tree for NIDS Income")
# The sample is split four times, which leads to 5 terminal nodes.
# Wealthy households either have a computer, or they have 2 or fewer members and a washing machine
# Poor households have no computer or microwave and 3 or more members
# Middle income households have no computer, and either have 3 or more members and a microwave or 2 or fewer members and no washer

summary(rpart.simple)
# Without any conditioning variables, the sample has a MSE of MSE=1.399056 and a SST = MSE*N = 6550.38. 
# The MSE is also called the root node error and the TSS is also sometimes referred to as the deviance.
# The sum of the RSS in the two nodes after the first split is 4518.7630 + 588.3661 = 5107.1291. 
# Expressed as a proportion of the root SST, # this gives the rel error = 0.7797 associated with the first split. T
# he CP associated with this split is the relative reduction in the rel error of 0.2203. 
# This was the largest possible reduction in the rel error rate.
# The second split occurs in the left son of the first split, where the RSS is 4518.7630. It produces in its sons RSS's of
# 2095.1860 and 1606.0200 respectively, or 3701.206 in total. The reduction in the RSS is 4518.7630 - 3701.206 = 817.557, 
# which is 0.1248 when expressed as a proportion of the original TSS. This is the cp associated with the second split.

rpart.simple
printcp(rpart.simple)
#This table shows the rel errors and cp values associated with different splits, as well as the cv rel error values and their std deviations
plotcp(rpart.simple)
#A horizontal line is drawn 1SE above the minimum of the curve, which can assist in choosing the 1SE cp value (providing the minimum value appears on the graph)

set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 10)
seeds[[101]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
rpart.fit0 <- train(formula, data=training, method = 'rpart', trControl=fitControl, tuneLength = 10)
rpart.fit0
# The output shows that the RMSE is increasing for all candidate values of cp between 0.007181379 and 0.220330880 
# that the function chose for us, so we should also tune over smaller values.
# The fact that we are off the usual grid seems to suggest that our model calls for more larger trees than are usually optimal.

stopCluster(cluster)
registerDoSEQ()

library(mlr)
library(mlrMBO)
library(parallelMap)
regr.task = makeRegrTask(id = "NIDS income regression with rpart", data = training, target = "income")
measure = rmse
mbo.ctrl = makeMBOControl()
mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 25L)

getParamSet("regr.rpart")
regr.lrn = makeLearner("regr.rpart")
rpart.num_ps = makeParamSet(
  makeNumericParam("cp", lower = 0, upper = 0.2),
  makeIntegerParam("maxdepth", lower = 1, upper = 30),
  makeIntegerParam("minsplit", lower = 1, upper = 100)
)
design.mat = generateRandomDesign(n = 300, par.set = rpart.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

set.seed(123, "L'Ecuyer")
parallelStartSocket(min(6,detectCores()-1))
tune.pars.rpart = tuneParams(learner = regr.lrn, task = regr.task, resampling = cv10,
                             measures = rmse, par.set = rpart.num_ps, control = ctrl, show.info = TRUE)
#Result: cp=0.00101; maxdepth=25; minsplit=18 : rmse.test.rmse=0.811
parallelStop()
detach("package:mlrMBO", unload=TRUE)
detach("package:mlr", unload=TRUE)

cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
rpart.fit.time = system.time(rpart.fit <- train(formula, data=training, method = 'rpart', control= rpart.control(minsplit = 18, maxdepth = 25),trControl=fitControl,tuneGrid=expand.grid(cp=0.00101)))
rpart.fit.size = format(object.size(rpart.fit),units="Mb")
rpart.fit.rmse=compare.rmse(rpart.fit,training,testing)
rpart.fit.tab = c(round(rpart.fit.time[3],2),rpart.fit.size)
names(rpart.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(rpart.fit))
rpart.fit.plot=plot(varImp(rpart.fit))
varImp(rpart.fit)
rpart.fit$finalModel
fancyRpartPlot(rpart.fit$finalModel,main="Regression Tree for NIDS Income")
setNames(as.data.frame(table(predict(rpart.fit,training))), "")

#Regression tree (ctree)

# Regression trees (like those estimated with rpart) suffer from selection bias: predictors with a higher number of distinct values are 
# favored over more granular predictors. This issue is not addressed by fixing the tuning parameters, so makes the splitting algorithm
# vulnerbale to selecting continious or multivalued ordinal noise variables. It is worth noting that the rpart function above included all
# the continuous variables amongst the most important predictors. The conditional inference tree estimator attempts to address this by 
# using hypothesis testing of difference in the post-split means - corrected within each predictor for multiple comparisons -  before enacting 
# any splits. The p-value for this test is 1 - the mincriterion parameter, so these values would typically be between 0.75 and 0.99. 


set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 10)
seeds[[11]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
ctree.fit0 <- train(formula, data=training,method='ctree',trControl=fitControl, tuneLength = 10)
ctree.fit0
# The RMSE are fairly similiar across a wide range of the mincriterion

stopCluster(cluster)
registerDoSEQ()

library(mlr)
library(mlrMBO)
getParamSet("regr.ctree")
regr.lrn = makeLearner("regr.ctree")
ctree.num_ps = makeParamSet(
  makeNumericParam("mincriterion", lower = 0, upper = 1),
  makeIntegerParam("maxdepth", lower = 1, upper = 30),
  makeIntegerParam("minsplit", lower = 1, upper = 100)
)
design.mat = generateRandomDesign(n = 300, par.set = ctree.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

set.seed(123, "L'Ecuyer")
parallelStartSocket(min(6,detectCores()-1))
tune.pars.rpart = tuneParams(learner = regr.lrn, task = regr.task, resampling = cv10,
                             measures = rmse, par.set = ctree.num_ps, control = ctrl, show.info = TRUE)
#mincriterion=0.0379; maxdepth=26; minsplit=86 : rmse.test.rmse=0.806
parallelStop()
detach("package:mlrMBO", unload=TRUE)
detach("package:mlr", unload=TRUE)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
ctree.fit.time = system.time(ctree.fit <- train(formula, data=training, method = 'ctree', trControl=fitControl,controls= ctree_control(minsplit = 18, maxdepth = 25),tuneGrid=expand.grid(mincriterion=0.0379)))
ctree.fit.size = format(object.size(ctree.fit),units="Mb")
ctree.fit.rmse=compare.rmse(ctree.fit,training,testing)
ctree.fit.tab = c(round(ctree.fit.time[3],2),ctree.fit.size)
names(ctree.fit.tab) = c("Time to run (sec)","Size to store (MB)")
ctree.fit.plot=plot(varImp(ctree.fit))
varImp(ctree.fit)

# M5 = Regression model tree
set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 8)
seeds[[11]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
M5.fit0 <- train(formula, data=training,method='M5',trControl = fitControl,control = Weka_control(M = 10))
#M option represents minbucket

set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000,1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
M5.fit.time = system.time(M5.fit <- train(formula, data=training,method='M5',trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("No"))))
M5.fit.size = format(object.size(M5.fit),units="Mb")
M5.fit.rmse=compare.rmse(M5.fit,training,testing)
M5.fit.tab = c(round(M5.fit.time[3],2),M5.fit.size)
names(M5.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(M5.fit))
varImp(M5.fit)

# mlr does not have an M5 model, but the mob function seems comparable


stopCluster(cluster)
registerDoSEQ()

library(mlr)
library(mlrMBO)
getParamSet("regr.mob")

regr.lrn = makeLearner("regr.mob")
mob.num_ps = makeParamSet(
#  makeNumericParam("alpha", lower = 0, upper = 0.2),
  makeNumericParam("trim", lower = 0, upper = 0.2)
#  makeIntegerParam("minsplit", lower = 1, upper = 100)
)

design.mat = generateRandomDesign(n = 100, par.set = mob.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

set.seed(123, "L'Ecuyer")
parallelStartSocket(min(6,detectCores()-1))
# Could not get this to work
#tune.pars.rpart = tuneParams(learner = regr.lrn, task = regr.task, resampling = cv10,
#                             measures = rmse, par.set = mob.num_ps, control = ctrl, show.info = TRUE)
parallelStop()
detach("package:mlrMBO", unload=TRUE)
detach("package:mlr", unload=TRUE)


# evtree
set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 10)
seeds[[11]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
evtree.fit0 <- train(formula, data=training,method='evtree',trControl = fitControl, tuneLength = 10)

stopCluster(cluster)
registerDoSEQ()

library(mlr)
library(mlrMBO)
getParamSet("regr.evtree")
regr.lrn = makeLearner("regr.evtree")
evtree.num_ps = makeParamSet(
  makeNumericParam("alpha", lower = 0, upper = 0.2),
  makeIntegerParam("maxdepth", lower = 1, upper = 30),
  makeIntegerParam("minsplit", lower = 1, upper = 100),
  makeIntegerParam("niterations", lower = 100, upper = 10000),
  makeIntegerParam("ntrees", lower = 10, upper = 200)
)
design.mat = generateRandomDesign(n = 100, par.set = evtree.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

set.seed(123, "L'Ecuyer")
parallelStartSocket(min(6,detectCores()-1))
# This took about a day to run and then produced an error
#tune.pars.rpart = tuneParams(learner = regr.lrn, task = regr.task, resampling = cv10,
#                             measures = rmse, par.set = evtree.num_ps, control = ctrl, show.info = TRUE)
parallelStop()
detach("package:mlrMBO", unload=TRUE)
detach("package:mlr", unload=TRUE)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#Bagged regression tree (treebag)
# No tuning parameters
set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000,1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
treebag.fit.time <- system.time(treebag.fit <- train(formula, data=training, method = 'treebag', trControl=fitControl))
treebag.fit.size = format(object.size(treebag.fit),units="Mb")
treebag.fit.rmse=compare.rmse(treebag.fit,training,testing)
treebag.fit.tab = c(round(treebag.fit.time[3],2),treebag.fit.size)
names(treebag.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(treebag.fit))
varImp(treebag.fit)

#Random forest
set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(100, 10)
seeds[[11]] <- sample.int(100, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
rf.fit0 <- train(formula, data=training,method='rf',trControl=fitControl, tuneLength = 10)
rf.fit0
#mtry=43->RMSE=0.7211

stopCluster(cluster)
registerDoSEQ()

library(mlr)
library(mlrMBO)
getParamSet("regr.randomForest")
regr.lrn = makeLearner("regr.randomForest")
rf.num_ps = makeParamSet(
  makeIntegerParam("mtry", lower = 2, upper = 188),
  makeIntegerParam("ntree", lower = 100, upper = 3000),
  makeIntegerParam("nodesize", lower = 5, upper = 200)
)
design.mat = generateRandomDesign(n = 100, par.set = rf.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

set.seed(123, "L'Ecuyer")
parallelStartSocket(min(6,detectCores()-1))
tune.pars.rf = tuneParams(learner = regr.lrn, task = regr.task, resampling = cv10,
                             measures = rmse, par.set = rf.num_ps, control = ctrl, show.info = TRUE)
#mtry=80; ntree=1086; nodesize=5 : rmse.test.rmse=0.72
parallelStop()
detach("package:mlrMBO", unload=TRUE)
detach("package:mlr", unload=TRUE)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)


set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = F,seeds=seeds)
rf.fit.time = system.time(rf.fit <- train(formula, data=training, method = 'rf', trControl=fitControl,tuneGrid=expand.grid(mtry=43),importance = TRUE))
rf.fit.size = format(object.size(rf.fit),units="Mb")
rf.fit.rmse=compare.rmse(rf.fit,training,testing)
rf.fit.tab = c(round(rf.fit.time[3],2),rf.fit.size)
names(rf.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(rf.fit))
varImp(rf.fit)

#Boosted regression tree (gbm)
#Stochastic Gradient Boosting

set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 10)
seeds[[11]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = T,seeds=seeds)
gbm.fit0 <- train(formula, data=training,method='gbm',trControl=fitControl, tuneLength=10)
gbm.fit0 
# n.trees = 350, interaction.depth = 4, shrinkage = 0.1 and n.minobsinnode = 10 -> RMSE = 0.6974879

stopCluster(cluster)
registerDoSEQ()

library(mlr)
library(mlrMBO)
getParamSet("regr.gbm")
regr.lrn = makeLearner("regr.gbm")
gbm.num_ps = makeParamSet(
  makeIntegerParam("interaction.depth", lower = 1, upper = 20),
  makeIntegerParam("n.trees", lower = 100, upper = 500),
  makeIntegerParam("n.minobsinnode", lower = 5, upper = 200),
  makeNumericParam("shrinkage", lower = 0, upper = 0.2)
)
design.mat = generateRandomDesign(n = 100, par.set = gbm.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

set.seed(123, "L'Ecuyer")
parallelStartSocket(min(6,detectCores()-1))
tune.pars.rf = tuneParams(learner = regr.lrn, task = regr.task, resampling = cv10,
                          measures = rmse, par.set = gbm.num_ps, control = ctrl, show.info = TRUE)
#interaction.depth=19; n.trees=467; n.minobsinnode=16; shrinkage=0.0173 : rmse.test.rmse=0.692
parallelStop()
detach("package:mlrMBO", unload=TRUE)
detach("package:mlr", unload=TRUE)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = F,seeds=seeds)
Grid <- expand.grid(n.trees = c(467), interaction.depth = c(19), shrinkage = c(0.0173),n.minobsinnode = c(16))
gbm.fit.time <- system.time(gbm.fit <- train(formula, data=training, method = 'gbm', trControl=fitControl,tuneGrid=Grid,metric='RMSE'))
gbm.fit.size = format(object.size(gbm.fit),units="Mb")
gbm.fit.rmse=compare.rmse(gbm.fit,training,testing)
gbm.fit.tab = c(round(gbm.fit.time[3],2),gbm.fit.size)
names(gbm.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(gbm.fit))
varImp(gbm.fit)

#Extreme gradient boosting (xbgTree)
set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 400)
seeds[[11]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = T,seeds=seeds)
xgbTree.fit0 <- train(formula, data=training,method='xgbTree',trControl=fitControl, tuneLength=10)
xgbTree.fit0
#nrounds = 250, max_depth = 2, eta = 0.3, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1 and subsample = 1

stopCluster(cluster)
registerDoSEQ()

library(mlr)
library(mlrMBO)
getParamSet("regr.xgboost")
regr.lrn = makeLearner("regr.xgboost")
regr.task = makeRegrTask("Xgboost with NIDS data", data = as.data.frame(lapply(training, as.numeric)), target = "income")
# Important: Xgboost in mlr does not like dummy variables; have to force them to be numeric.
xgboost.num_ps = makeParamSet(
  makeIntegerParam(id = "nrounds", lower = 1,upper = 80),
  makeIntegerParam(id = "max_depth", lower = 2, upper = 15),
  makeNumericParam(id = "eta", lower = .01, upper = .4)
)
design.mat = generateRandomDesign(n = 300, par.set = xgboost.num_ps)
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

set.seed(123, "L'Ecuyer")
parallelStartSocket(min(6,detectCores()-1))
tune.pars.rpart = tuneParams(learner = regr.lrn, task = regr.task, resampling = cv10,
                             measures = rmse, par.set = xgboost.num_ps, control = ctrl, show.info = TRUE)
#nrounds=76; max_depth=3; eta=0.181 : rmse.test.rmse=0.706
parallelStop()
detach("package:mlrMBO", unload=TRUE)
detach("package:mlr", unload=TRUE)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)


set.seed(123)
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000, 1)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = F,seeds=seeds)
Grid <- expand.grid(nrounds = c(76), max_depth = c(3), eta = c(0.181),gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)
xgbTree.fit.time <- system.time(xgbTree.fit <- train(formula, data=training, method = 'xgbTree', trControl=fitControl,tuneGrid=Grid,metric='RMSE'))
xgbTree.fit.size = format(object.size(xgbTree.fit),units="Mb")
xgbTree.fit.rmse=compare.rmse(xgbTree.fit,training,testing)
xgbTree.fit.tab = c(round(xgbTree.fit.time[3],2),xgbTree.fit.size)
names(xgbTree.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(xgbTree.fit))
varImp(xgbTree.fit)
# The parameters trained in caret perform better on the CV RMSE, whereas those trained in MLR do better on the testing RMSE.



# The following functions all worked, and should be further explored:
set.seed(123)
cforest.fit0 <- train(formula, data=training,method='cforest',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
blackboost.fit0 <- train(formula, data=training,method='blackboost',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
cubist.fit0 <- train(formula, data=training,method='cubist',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
bstTree.fit0 <- train(formula, data=training,method='bstTree',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
evtree.fit0 <- train(formula, data=training,method='evtree',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
nodeHarvest.fit0 <- train(formula, data=training,method='nodeHarvest',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

#Neural nets

set.seed(123)
nnet.fit0 <- train(formula, data=training,method='nnet',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
brnn.fit0 <- train(formula, data=training,method='brnn',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
brnn.fit0 <- train(formula, data=training,method='brnn',trControl=trainControl(method = "cv",number = 5, allowParallel = F))


# These functions did not work:
set.seed(123)
bartMachine.fit0 <- train(formula, data=training,method='bartMachine',trControl=trainControl(method = "cv",number = 5, allowParallel = F))


set.seed(123)
gbm_h2o.fit0 <- train(formula, data=training,method='gbm_h2o',trControl=trainControl(method = "cv",number = 5))

# Doesnt work on continuous outcomes
#set.seed(123)
#oblique.fit0 <- train(formula, data=training,method='oblique.tree',trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE))


#Other trees

bagging:
  Boruta (doesnt seem to work with caret)
  classbagg


not sure:
  nodeHarvest


fitControl <- trainControl(method = "repeatedcv", number = 2,repeats = 2,verboseIter = TRUE)

set.seed(123)
fit.bartMachine.time <- system.time(fit.bartMachine <- train(formula, data=training, method = 'bartMachine', trControl=fitControl,metric='RMSE'))
fit.bartMachine.size = object.size(fit.bartMachine)
fit.bartMachine.rmse=compare.rmse(fit.bartMachine,training,testing)

set.seed(123)
fit.blackboost.time <- system.time(fit.blackboost <- train(formula, data=training, method = 'blackboost', trControl=fitControl,metric='RMSE'))
fit.blackboost.size = object.size(fit.blackboost)
fit.blackboost.rmse=compare.rmse(fit.blackboost,training,testing)

set.seed(123)
fit.M5.time <- system.time(fit.M5 <- train(formula, data=training, method = 'rpart', trControl=fitControl,metric='RMSE'))
fit.M5.size = object.size(fit.M5)
fit.M5.rmse=compare.rmse(fit.M5,training,testing)

set.seed(123)
1
fit.bstTree.size = object.size(fit.bstTree)
fit.bstTree.rmse=compare.rmse(fit.bstTree,training,testing)

set.seed(123)
fit.gbm_h2o.time <- system.time(fit.gbm_h2o <- train(formula, data=training, method = 'gbm_h2o', trControl=fitControl,metric='RMSE'))
fit.gbm_h2o.size = object.size(fit.gbm_h2o)
fit.gbm_h2o.rmse=compare.rmse(fit.gbm_h2o,training,testing)

set.seed(123)
fit.Boruta.time <- system.time(fit.Boruta <- train(formula, data=training, method = 'Boruta', trControl=fitControl,metric='RMSE'))
fit.Boruta.size = object.size(fit.Boruta)
fit.Boruta.rmse=compare.rmse(fit.Boruta,training,testing)

set.seed(123)
fit.evtree.time <- system.time(fit.evtree <- train(formula, data=training, method = 'evtree', trControl=fitControl,metric='RMSE'))
fit.evtree.size = object.size(fit.evtree)
fit.evtree.rmse=compare.rmse(fit.evtree,training,testing)

set.seed(123)
fit.nodeHarvest.time <- system.time(fit.nodeHarvest <- train(formula, data=training, method = 'nodeHarvest', trControl=fitControl,metric='RMSE'))
fit.nodeHarvest.size = object.size(fit.nodeHarvest)
fit.nodeHarvest.rmse=compare.rmse(fit.nodeHarvest,training,testing)

set.seed(123)
fit.gbm.time <- system.time(fit.gbm <- train(formula, data=training, method = 'gbm', trControl=fitControl,metric='RMSE'))
fit.gbm.size = object.size(fit.gbm)
fit.gbm.rmse=compare.rmse(fit.gbm,training,testing)




#BartMachine
set.seed(123)
#seeds <- vector(mode = "list", length = 11)
#for(i in 1:10) seeds[[i]] <- sample.int(100, 10000)
#seeds[[11]] <- sample.int(100, 1)
#fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE,seeds=seeds)
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = F)
bartMachine.fit0 <- train(formula, data=training,method='bartMachine',trControl=fitControl, tuneLength = 10)
bartMachine.fit0


#Boosted regression tree (gamboost)

set.seed(123)
Grid <- expand.grid(mstop=seq(100),prune=c(5))
fit.gamboost.time <- system.time(fit.gamboost <- train(formula, data=training, method = 'gamboost', trControl=fitControl,tuneGrid=Grid,metric='RMSE'))
fit.gamboost.size = object.size(fit.gamboost)
fit.gamboost.rmse=compare.rmse(fit.gamboost,training,testing)
plot(varImp(fit.gamboost))

fit.lm.rmse
fit.ctree.rmse
fit.ctree2.rmse
fit.rpart.rmse
fit.treebag.rmse
fit.bagEarth.rmse
fit.rf.rmse

fitControl <- trainControl(method = "repeatedcv", number = 2,repeats = 2,verboseIter = TRUE)
set.seed(123)
cubist.fit0 <- train(formula, data=training,method='cubist',trControl=fitControl)

