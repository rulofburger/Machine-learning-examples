#Load libraries
library(leaps)
library(glmnet)
library(class)
library(mlbench)
library(caret)
library(car)
library(party)
library(dummies)
library(haven)
library(foreach)
library(dplyr)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_131')

#Remove lists from memory and load data
library(haven)
rm(list=ls())
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
hhdata.full = data.frame(income= log(hhincome/hhsizer),members=hhsizer,rooms=h_dwlrms,dwelling.type=h_dwltyp,province=hhprov, roof.material=h_dwlmatroof, wall.material=h_dwlmatrwll, owned=h_ownd, water.source=h_watsrc, toilet.type=h_toi, 
                         toilet.share=h_toishr, electricity=h_enrgelec, landline=h_tellnd, cellphone.use=h_telcel, refuse.removal=h_refrem, street.light=h_strlght, 
                         radio=h_ownrad, hifi=h_ownhif, tv=h_owntel, satelite=h_ownsat, video=h_ownvid, computer=h_owncom, camera=h_owncam, cellphone.own=h_owncel, tv.les=h_ownelestv, 
                         gas.stove=h_owngasstv, microwave=h_ownmic, fridge=h_ownfrg, washer=h_ownwsh,district.council=district_council,head.absent=head_absent,head.female = head_gender - 1, head.educ=head_educ, head.age=head_age)
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
hhdata.full = data.frame(hhdata.full[,1:3],hhdata.full[,33:34],dummy.data.frame(hhdata.full[,4:32], names = NULL, omit.constants=TRUE, dummy.classes = getOption("dummy.classes")))

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

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#stopCluster(cluster)
#registerDoSEQ()

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = TRUE, allowParallel = TRUE)

#Linear regression (lm)

set.seed(123)
lm.fit.time = system.time(lm.fit <- train(formula, data=training,method='lm',trControl=fitControl))
lm.fit.size = format(object.size(lm.fit),units="Mb")
lm.fit.rmse=compare.rmse(lm.fit,training,testing)
lm.fit.rmse
lm.fit.tab = c(round(lm.fit.time[3],2),lm.fit.size)
names(lm.fit.tab) = c("Time to run (sec)","Size to store (MB)")
lm.plot=plot(varImp(lm.fit))
summary(lm.fit$finalModel)

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

plot(rpart.simple, uniform=TRUE, 
     main="Regression Tree for NIDS Income")
text(rpart.simple, use.n=TRUE, all=TRUE, cex=.8)
post(rpart.simple, file = "D:\\My Documents\\Reading group\\Machine learning\\Data examples\\Poverty targeting example\\rpart.simple.ps", 
     title = "Regression Tree for NIDS Income")
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
rpart.fit0 <- train(formula, data=training, method = 'rpart', trControl=fitControl, tuneLength = 10)
# The output shows that the RMSE is increasing for all candidate values of cp between 0.007181379 and 0.220330880 
# that the function chose for us, so we should also tune over smaller values.
# The fact that we are off the usual grid seems to suggest that our model calls for more larger trees than are usually optimal.

set.seed(123)
Grid <- expand.grid(cp=seq(0, 0.002, 0.0001))
rpart.fit1 <- train(formula, data=training, method = 'rpart', trControl=fitControl,tuneGrid=Grid)
rpart.fit1
trellis.par.set(caretTheme())
rpart.fit1.plot=plot(rpart.fit1)
rpart.fit1$bestTune$cp
# Out of sample fit is maximised for cp = 0.0008, which is much smaller than the default value.

set.seed(123)
rpart.fit.time = system.time(rpart.fit <- train(formula, data=training, method = 'rpart', trControl=fitControl,tuneGrid=expand.grid(cp=rpart.fit1$bestTune$cp)))
rpart.fit.size = format(object.size(rpart.fit),units="Mb")
rpart.fit.rmse=compare.rmse(rpart.fit,training,testing)
rpart.fit.tab = c(round(rpart.fit.time[3],2),rpart.fit.size)
names(rpart.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(rpart.fit))
rpart.fit.plot=plot(varImp(rpart.fit))
varImp(rpart.fit)
rpart.fit$finalModel
plot(rpart.fit$finalModel, uniform=TRUE, 
     main="Regression Tree for NIDS Income")
text(rpart.fit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
setNames(as.data.frame(table(predict(rpart.fit,training))), "")

# Note that the same model can be obtained using either of the following commands:
rpart(income ~ .,data=training,control=rpart.control(cp = 0.0008))
rpart(income ~ ., method="anova", data=training,control=rpart.control(minsplit = 20, minbucket = round(20/3), cp = 0.0008, 
                                                                      maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                                      surrogatestyle = 0, maxdepth = 30))
rpart(income ~ ., method="anova", data=training,control=rpart.control(cp = 0.0008))
train(income ~ ., training, method = "rpart", control = rpart.control(minsplit = 20, minbucket = round(20/3),
                                                                      maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                                      surrogatestyle = 0, maxdepth = 30),tuneGrid=expand.grid(cp=0.0008))
train(income ~ ., training, method = "rpart", control = rpart.control(minsplit = 20, minbucket = round(20/3),
                                                                      maxdepth = 30),tuneGrid=expand.grid(cp=0.0008))


# This is a very large model. As far as I can tell there are 93 unique terminal nodes.
# Calling the final model shows model node numbers into the 2000s, but these numbers enumerate all of the potential splits at nodes below 
# the max depth that were not enacted due to too small bins or too small an improvement in fit.
# The model has a depth of 12, which is too deep to sensibly plot the final decision tree or to interepret sensibly.
# The one standard error rule dictates that we should choose the simplest model which performs no worse than the best fit.
# In Monte-Carlo trials, this method of pruning has proven very reliable for screening out ‘pure noise’ variables in the data set.

set.seed(123)
Grid <- expand.grid(cp=c(seq(0, 0.002, 0.0001),seq(0.002, 0.003, 0.0002)))
rpart.fit11 <- train(formula, data=training, method = 'rpart', trControl=fitControl,tuneGrid=Grid)

rpart.fit11$results %>%
  mutate(RMSESD_low = RMSE - (RMSESD/sqrt(rpart.fit11$control$number * rpart.fit11$control$repeats)),
         RMSESD_high = RMSE + (RMSESD/sqrt(rpart.fit11$control$number * rpart.fit11$control$repeats))) %>%
  ggplot(aes(x = cp)) +
  geom_line(aes(y = RMSE)) +
  geom_point(aes(y = RMSE)) +
  xlim(-0.0001, 0.0031) +
  #scale_x_continuous() + #correct spacing of the cost parameter
  ylim(0.8, 0.86) + #set correct y-axis
  geom_errorbar(aes(ymin=RMSESD_low, ymax=RMSESD_high), 
                colour="gray50",
                width=.0001) +
  labs(title="Estimates of prediction accuracy\nwith 1 SD errror bars")

#The graph suggests that a cp value of around 0.0024 would produce a RMSE that is only 1 SD worse than the optimal RMSE

set.seed(123)
rpart.fit.1SD.time = system.time(rpart.fit.1SD <- train(formula, data=training, method = 'rpart', trControl=fitControl,tuneGrid=expand.grid(cp=0.0024)))
rpart.fit.1SD.size = format(object.size(rpart.fit.1SD),units="Mb")
rpart.fit.1SD.rmse=compare.rmse(rpart.fit.1SD,training,testing)
rpart.fit.1SD.tab = c(round(rpart.fit.1SD.time[3],2),rpart.fit.1SD.size)
names(rpart.fit.1SD.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(rpart.fit.1SD))
rpart.fit.1SD.plot=plot(varImp(rpart.fit.1SD))
varImp(rpart.fit.1SD)
rpart.fit.1SD$finalModel
plot(rpart.fit.1SD$finalModel, uniform=TRUE, 
     main="Regression Tree for NIDS Income")
text(rpart.fit.1SD$finalModel, use.n=TRUE, all=TRUE, cex=.8)
setNames(as.data.frame(table(predict(rpart.fit.1SD,training))), "")

# This new model seems much simpler, although perhaps still too complicated to interpret sensibly. 
# It had a depth of 8 and only 25 terminal nodes.

set.seed(123)
rpart1SE.fit0 <- train(formula, data=training, method = 'rpart1SE', trControl=fitControl, tuneLength = 10)

#This model produces an RMSE of 0.85, which is considerably worse. 
# An alternative way to achieve the same thing (but which seem to work better):
rpart1SE.fit1=rpart(income ~ ., method="anova", data=training,control=rpart.control(cp = 0.0008))
plotcp(rpart1SE.fit1)
rpart1SE.fit1=rpart(income ~ ., method="anova", data=training,control=rpart.control(cp = 0.004))

# Cp is the only value that caret allow the rpart function to tune. However, rpart2 can be used to tune the maxdepth

#Regression tree (rpart2)
set.seed(123)
rpart2.fit0 <- train(formula, data=training, method = 'rpart2', trControl=fitControl, tuneLength = 10)

# The RMSE are considerably worse than those achieved for rpart. The reason is that those allowed cp to vary for
# maxdepth values of 30 (which was never a binding constraint). In contrast, rpart2 fixes cp = 0.01, which is much
# higher than is optimal for this model, and then varies the values of maxdepth. It doesnt help to allow for higher
# maxdepth values than 10, since at cp = 0.01 the optimal tree depth is 9. 

set.seed(123)
Grid <- expand.grid(maxdepth=seq(15, 30, 2))
rpart2.fit1 <- train(formula, data=training, method = 'rpart2', trControl=fitControl,tuneGrid=Grid, control=rpart.control(cp = 0.0001,minsplit = 20))
rpart2.fit1
trellis.par.set(caretTheme())
rpart2.fit1.plot=plot(rpart2.fit1)
# At a sufficiently low cp value, maxdepth becomes a U-shaped function of the tuning parameter. 
# However for this application , it does not achieve values superior to those obtained with rpart, so we will not pursue this issue further.



#Regression tree (ctree)

# Regression trees (like those estimated with rpart) suffer from selection bias: predictors with a higher number of distinct values are 
# favored over more granular predictors. This issue is not addressed by fixing the tuning parameters, so makes the splitting algorithm
# vulnerbale to selecting continious or multivalued ordinal noise variables. It is worth noting that the rpart function above included all
# the continuous variables amongst the most important predictors. The conditional inference tree estimator attempts to address this by 
# using hypothesis testing of difference in the post-split means - corrected within each predictor for multiple comparisons -  before enacting 
# any splits. The p-value for this test is 1 - the mincriterion parameter, so these values would typically be between 0.75 and 0.99. 

set.seed(123)
ctree.fit0 <- train(formula, data=training,method='ctree',trControl=fitControl, tuneLength = 10)
ctree.fit0
# The RMSE are fairly similiar across a wide range of the mincriterion

set.seed(123)
Grid <- expand.grid(mincriterion=c(0.5,0.55,0.6,0.65,0.7))
ctree.fit1 <- train(formula, data=training,method='ctree',trControl=fitControl,tuneGrid=Grid)
trellis.par.set(caretTheme())
ctree.fit1.plot=plot(ctree.fit1)

set.seed(123)
ctree.fit.time = system.time(ctree.fit <- train(formula, data=training,method='ctree',trControl=fitControl,tuneGrid=expand.grid(mincriterion=0.6)))
ctree.fit.size = format(object.size(ctree.fit),units="Mb")
ctree.fit.rmse=compare.rmse(ctree.fit,training,testing)
ctree.fit.tab = c(round(ctree.fit.time[3],2),ctree.fit.size)
names(ctree.fit.tab) = c("Time to run (sec)","Size to store (MB)")
ctree.fit.plot=plot(varImp(ctree.fit))
varImp(ctree.fit)

#Regression tree (ctree2)
set.seed(123)
ctree2.fit0 <- train(formula, data=training,method='ctree2',trControl=fitControl)
ctree2.fit0

set.seed(123)
Grid <- expand.grid(maxdepth = c(3,5,8,10,20),mincriterion=c(0.35, 0.55, 0.75, 0.95))
ctree2.fit1 <- train(formula, data=training, method = 'ctree2', trControl=fitControl,tuneGrid=Grid)
ctree2.fit1
trellis.par.set(caretTheme())
ctree2.fit1.plot=plot(ctree2.fit1)

set.seed(123)
ctree2.fit.time = system.time(ctree2.fit <- train(formula, data=training,method='ctree2',trControl=fitControl,tuneGrid=expand.grid(maxdepth = 8,mincriterion=0.35)))
ctree2.fit.size = format(object.size(ctree2.fit),units="Mb")
ctree2.fit.rmse=compare.rmse(ctree2.fit,training,testing)
ctree2.fit.tab = c(round(ctree2.fit.time[3],2),ctree2.fit.size)
names(ctree2.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(ctree2.fit))
varImp(ctree2.fit)

#library(devtools)
#install_url('https://cran.r-project.org/src/contrib/Archive/oblique.tree/oblique.tree_1.1.1.tar.gz')
#install_url('https://cran.r-project.org/src/contrib/Archive/RGtk2/RGtk2_2.20.31.tar.gz')

# Doesnt work on continuous outcomes
#set.seed(123)
#oblique.fit0 <- train(formula, data=training,method='oblique.tree',trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE))

# M option is minbucket
set.seed(123)
M5.fit0 <- train(formula, data=training,method='M5',trControl = trainControl(method = "cv",number = 5, allowParallel = TRUE),control = Weka_control(M = 10))

set.seed(123)
M5.fit.time = system.time(M5.fit <- train(formula, data=training,method='M5',trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=expand.grid(pruned = "Yes", smoothed = c("Yes"), rules = c("No"))))
M5.fit.size = format(object.size(M5.fit),units="Mb")
M5.fit.rmse=compare.rmse(M5.fit,training,testing)
M5.fit.tab = c(round(M5.fit.time[3],2),M5.fit.size)
names(M5.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(M5.fit))
varImp(ctree2.fit)

#stopCluster(cluster)
#registerDoSEQ()


#Bagged regression tree (treebag)
# No tuning parameters
set.seed(123)
treebag.fit.time <- system.time(treebag.fit <- train(formula, data=training, method = 'treebag', trControl=fitControl))
treebag.fit.size = format(object.size(treebag.fit),units="Mb")
treebag.fit.rmse=compare.rmse(treebag.fit,training,testing)
treebag.fit.tab = c(round(treebag.fit.time[3],2),treebag.fit.size)
names(treebag.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(treebag.fit))
varImp(treebag.fit)


#Bagged regression tree (bagEarth)

set.seed(123)
bagEarth.fit0 <- train(formula, data=training,method='bagEarth',trControl=trainControl(method = "cv",number = 5, allowParallel = F))
bagEarth.fit0


set.seed(123)
Grid <- expand.grid(degree=c(1), nprune = c(40,50,60))
bagEarth.fit1 <- train(formula, data=training, method = 'bagEarth', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=Grid)
bagEarth.fit1
trellis.par.set(caretTheme())
bagEarth.fit1.plot=plot(bagEarth.fit1)

set.seed(123)
bagEarth.fit.time <- system.time(bagEarth.fit <- train(formula, data=training, method = 'bagEarth', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=expand.grid(nprune=50, degree=1)))
bagEarth.fit.size = format(object.size(bagEarth.fit),units="Mb")
bagEarth.fit.rmse=compare.rmse(bagEarth.fit,training,testing)
bagEarth.fit.tab = c(round(bagEarth.fit.time[3],2),treebag.fit.size)
names(bagEarth.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(bagEarth.fit))
varImp(bagEarth.fit)

#Random forest
# When trying to run this with allowParallel option, it produces the error message: "Error in serialize(data, node$con): error writing to connection"
set.seed(123)
rf.fit0 <- train(formula, data=training,method='rf',trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneLength=10)
rf.fit0

set.seed(123)
Grid <- expand.grid(mtry = seq(40,80,5))
rf.fit1 <- train(formula, data=training , method='rf', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=Grid,metric='RMSE')
rf.fit1
trellis.par.set(caretTheme())
rf.fit1.plot=plot(rf.fit1)

set.seed(123)
rf.fit.time = system.time(rf.fit <- train(formula, data=training, method = 'rf', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=expand.grid(mtry=55),importance = TRUE))
rf.fit.size = format(object.size(rf.fit),units="Mb")
rf.fit.rmse=compare.rmse(rf.fit,training,testing)
rf.fit.tab = c(round(rf.fit.time[3],2),rf.fit.size)
names(rf.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(rf.fit))
varImp(rf.fit)

#Boosted regression tree (gbm)
#Stochastic Gradient Boosting

set.seed(123)
gbm.fit0 <- train(formula, data=training,method='gbm',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

# It seems that tuning across different n.trees comes at little additional cost in computing time, since the all trees lower than the maximum have to evaluated anyway.
set.seed(123)
# model still increasing at n.trees at 800
Grid <- expand.grid(n.trees = c(400,500,600,700,800), interaction.depth = c(10,20), shrinkage = c(0.01),n.minobsinnode = c(30,50))
gbm.fit1 <- train(formula, data=training, method = 'gbm', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=Grid,metric='RMSE')
gbm.fit1

set.seed(123)
Grid <- expand.grid(n.trees = c(800), interaction.depth = c(20), shrinkage = c(0.01),n.minobsinnode = c(30))
gbm.fit.time <- system.time(gbm.fit <- train(formula, data=training, method = 'gbm', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=Grid,metric='RMSE'))
gbm.fit.size = format(object.size(gbm.fit),units="Mb")
gbm.fit.rmse=compare.rmse(gbm.fit,training,testing)
gbm.fit.tab = c(round(gbm.fit.time[3],2),gbm.fit.size)
names(gbm.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(gbm.fit))
varImp(gbm.fit)

#This models seems a little overfitted. Is it an issue with interaction.depth being too large?

#Boosted regression tree (xbgTree)


set.seed(123)
xgbTree.fit0 <- train(formula, data=training,method='xgbTree',trControl=trainControl(method = "cv",number = 5, allowParallel = F))

set.seed(123)
xgbTree.fit.time <- system.time(xgbTree.fit <- train(formula, data=training, method = 'xgbTree', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=expand.grid(eta=0.4,max_depth=2,colsample_bytree=0.8,subsample=1,nrounds=100,gamma=0,min_child_weight=1)))
xgbTree.fit.size = format(object.size(xgbTree.fit),units="Mb")
xgbTree.fit.rmse=compare.rmse(xgbTree.fit,training,testing)
xgbTree.fit.tab = c(round(xgbTree.fit.time[3],2),xgbTree.fit.size)
names(xgbTree.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(xgbTree.fit))
varImp(xgbTree.fit)
# This method seems bizarrely quick. Am i missing something? It could also do with more tuning

#Here is also a nice article about how to tune xgboost automatically with help of the mlr (and mlrMBO) R-package:
#  http://mlr-org.github.io/How-to-win-a-drone-in-20-lines-of-R-code/


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




# These functions did not work:
set.seed(123)
bartMachine.fit0 <- train(formula, data=training,method='bartMachine',trControl=trainControl(method = "cv",number = 5, allowParallel = F))


set.seed(123)
gbm_h2o.fit0 <- train(formula, data=training,method='gbm_h2o',trControl=trainControl(method = "cv",number = 5))

#Other trees

# bagging:
#   Boruta (doesnt seem to work with caret)
#   classbagg
# 
# 
# not sure:
#   nodeHarvest
# 
# 
# fitControl <- trainControl(method = "repeatedcv", number = 2,repeats = 2,verboseIter = TRUE)
# 
# set.seed(123)
# fit.bartMachine.time <- system.time(fit.bartMachine <- train(formula, data=training, method = 'bartMachine', trControl=fitControl,metric='RMSE'))
# fit.bartMachine.size = object.size(fit.bartMachine)
# fit.bartMachine.rmse=compare.rmse(fit.bartMachine,training,testing)
# 
# set.seed(123)
# fit.blackboost.time <- system.time(fit.blackboost <- train(formula, data=training, method = 'blackboost', trControl=fitControl,metric='RMSE'))
# fit.blackboost.size = object.size(fit.blackboost)
# fit.blackboost.rmse=compare.rmse(fit.blackboost,training,testing)
# 
# set.seed(123)
# fit.M5.time <- system.time(fit.M5 <- train(formula, data=training, method = 'rpart', trControl=fitControl,metric='RMSE'))
# fit.M5.size = object.size(fit.M5)
# fit.M5.rmse=compare.rmse(fit.M5,training,testing)
# 
# set.seed(123)
# 1
# fit.bstTree.size = object.size(fit.bstTree)
# fit.bstTree.rmse=compare.rmse(fit.bstTree,training,testing)
# 
# set.seed(123)
# fit.gbm_h2o.time <- system.time(fit.gbm_h2o <- train(formula, data=training, method = 'gbm_h2o', trControl=fitControl,metric='RMSE'))
# fit.gbm_h2o.size = object.size(fit.gbm_h2o)
# fit.gbm_h2o.rmse=compare.rmse(fit.gbm_h2o,training,testing)
# 
# set.seed(123)
# fit.Boruta.time <- system.time(fit.Boruta <- train(formula, data=training, method = 'Boruta', trControl=fitControl,metric='RMSE'))
# fit.Boruta.size = object.size(fit.Boruta)
# fit.Boruta.rmse=compare.rmse(fit.Boruta,training,testing)
# 
# set.seed(123)
# fit.evtree.time <- system.time(fit.evtree <- train(formula, data=training, method = 'evtree', trControl=fitControl,metric='RMSE'))
# fit.evtree.size = object.size(fit.evtree)
# fit.evtree.rmse=compare.rmse(fit.evtree,training,testing)
# 
# set.seed(123)
# fit.nodeHarvest.time <- system.time(fit.nodeHarvest <- train(formula, data=training, method = 'nodeHarvest', trControl=fitControl,metric='RMSE'))
# fit.nodeHarvest.size = object.size(fit.nodeHarvest)
# fit.nodeHarvest.rmse=compare.rmse(fit.nodeHarvest,training,testing)
# 
# set.seed(123)
# fit.gbm.time <- system.time(fit.gbm <- train(formula, data=training, method = 'gbm', trControl=fitControl,metric='RMSE'))
# fit.gbm.size = object.size(fit.gbm)
# fit.gbm.rmse=compare.rmse(fit.gbm,training,testing)
# 
# 
# 
# 
# 
# 
# 
# #Boosted regression tree (gamboost)
# 
# set.seed(123)
# Grid <- expand.grid(mstop=seq(100),prune=c(5))
# fit.gamboost.time <- system.time(fit.gamboost <- train(formula, data=training, method = 'gamboost', trControl=fitControl,tuneGrid=Grid,metric='RMSE'))
# fit.gamboost.size = object.size(fit.gamboost)
# fit.gamboost.rmse=compare.rmse(fit.gamboost,training,testing)
# plot(varImp(fit.gamboost))
# 
# fit.lm.rmse
# fit.ctree.rmse
# fit.ctree2.rmse
# fit.rpart.rmse
# fit.treebag.rmse
# fit.bagEarth.rmse
# fit.rf.rmse
# 
# fitControl <- trainControl(method = "repeatedcv", number = 2,repeats = 2,verboseIter = TRUE)
# set.seed(123)
# cubist.fit0 <- train(formula, data=training,method='cubist',trControl=fitControl)
# 
