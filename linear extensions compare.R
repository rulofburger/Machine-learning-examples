#Load libraries
#library(haven)
#library(mlbench)
#library(caret)

#detach("package:mlrMBO", unload=TRUE)
#detach("package:mlr", unload=TRUE)

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
library(dummies)
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

library(caret)
fitControl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, allowParallel = TRUE)

set.seed(7)
inTrain = createDataPartition(y=income,p=0.75,list=F)
training= hhdata.full[inTrain,]
testing= hhdata.full[-inTrain,]
formula = income~.

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

#Boosted regression tree (gbm)
#Stochastic Gradient Boosting
library(foreach)
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

set.seed(123)
lasso.fit0 <- train(formula, data=training,method='lasso',trControl=trainControl(method = "cv",number = 5, allowParallel = T))

# It seems that tuning across different n.trees comes at little additional cost in computing time, since the all trees lower than the maximum have to evaluated anyway.
set.seed(123)
Grid <- expand.grid(fraction = seq(0.0001,0.01,0.0001))
lasso.fit1 <- train(formula, data=training, method = 'lasso', trControl=trainControl(method = "cv",number = 5, allowParallel = T),tuneGrid=Grid,metric='RMSE')
lasso.fit1

set.seed(123)
Grid <- expand.grid(n.trees = c(800), interaction.depth = c(20), shrinkage = c(0.01),n.minobsinnode = c(30))
gbm.fit.time <- system.time(gbm.fit <- train(formula, data=training, method = 'gbm', trControl=trainControl(method = "cv",number = 5, allowParallel = F),tuneGrid=Grid,metric='RMSE'))
gbm.fit.size = format(object.size(gbm.fit),units="Mb")
gbm.fit.rmse=compare.rmse(gbm.fit,training,testing)
gbm.fit.tab = c(round(gbm.fit.time[3],2),gbm.fit.size)
names(gbm.fit.tab) = c("Time to run (sec)","Size to store (MB)")
plot(varImp(gbm.fit))
varImp(gbm.fit)


#Pre-analysis inspection

nzv <- nearZeroVar(training, saveMetrics= TRUE)
nzv[nzv$nzv,]


filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)

# calculate correlation matrix
correlationMatrix <- cor(hhdata.full[,2:189])
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# ensure results are repeatable
set.seed(7)
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
#results <- rfe(training[,2:189], training[,1], sizes=c(1:20), rfeControl=control)
#print(results)
#predictors(results)
#plot(results, type=c("g", "o"))

regfit.fwd=regsubsets(income~., data=training,nvmax =99,method="backward")
summary(regfit.fwd)
plot(regfit.fwd, scale="bic", main="BIC")
plot(regfit.fwd, scale="adjr2", main="Adjusted R^2")

regfit.bwd=regsubsets(income~.,data=training,nvmax =99,method="backward")
summary(regfit.bwd)$outmat
plot(regfit.bwd, scale="bic", main="BIC")
plot(regfit.bwd, scale="adjr2", main="Adjusted R^2")

regfit.seq=regsubsets(income~.,data=training,nvmax =99,method="seqrep")
summary(regfit.seq)$outmat
plot(regfit.seq, scale="bic", main="BIC")
plot(regfit.seq, scale="adjr2", main="Adjusted R^2")

regfit.fwd$xnames[rank(regfit.fwd$vorder, ties.method= "first")][1:20]
regfit.fwd$xnames[rank(regfit.bwd$vorder, ties.method= "first")][1:20]
regfit.fwd$xnames[rank(regfit.seq$vorder, ties.method= "first")][1:20]

summary(regfit.fwd)$bic
summary(regfit.bwd)$bic
summary(regfit.seq)$bic
plot(1:100,summary(regfit.fwd)$bic,type="l",col="red")
lines(1:100,summary(regfit.bwd)$bic,col="green")
lines(1:100,summary(regfit.seq)$bic,col="blue")


which.min(summary(regfit.fwd)$bic)
which.min(summary(regfit.bwd)$bic)
which.min(summary(regfit.seq)$bic)

names(which(summary(regfit.fwd)$which[which.min(summary(regfit.fwd)$bic),]==T))
names(which(summary(regfit.bwd)$which[which.min(summary(regfit.bwd)$bic),]==T))
names(which(summary(regfit.seq)$which[which.min(summary(regfit.seq)$bic),]==T))

coefi.fwd = coef(regfit.fwd, id = which.min(summary(regfit.fwd)$bic))
coefi.bwd = coef(regfit.bwd, id = which.min(summary(regfit.bwd)$bic))
coefi.seq = coef(regfit.seq, id = which.min(summary(regfit.seq)$bic))
x.test = model.matrix(income ~ ., data = hhdata.full[-train, ])  # notice the -index!
pred.fwd = x.test[, names(coefi.fwd)] %*% coefi.fwd
pred.bwd = x.test[, names(coefi.bwd)] %*% coefi.bwd
pred.seq = x.test[, names(coefi.seq)] %*% coefi.seq

rmse.fwd = sqrt(mean(((income[-train]-pred.fwd))^2))
rmse.fwd
rmse.bwd = sqrt(mean(((income[-train]-pred.bwd))^2))
rmse.bwd
rmse.seq = sqrt(mean(((income[-train]-pred.seq))^2))
rmse.seq

x=model.matrix(income~.,hhdata.full)[,-1]
y=hhdata.full$income
grid =10^seq(10,-2,length=100)
ridge.mod =glmnet(x,y,alpha=0,lambda =grid)
dim(coef(ridge.mod))

ridge.mod$lambda [50]
coef(ridge.mod)[,50]

cv.out =cv.glmnet(x[train,],y[train],alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod ,s=bestlam ,newx=x[test ,])
rmse.ridge=sqrt(mean((ridge.pred-y[test])^2))
rmse.ridge

model <- train(income~., data=hhdata.full, method="lm", preProcess="scale")
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

lasso.pred=predict(lasso.mod ,s=bestlam ,newx=x[test ,])
rmse.lasso=sqrt(mean((lasso.pred-y[test])^2))
rmse.lasso

rmse.lm.full
rmse.fwd
rmse.bwd
rmse.seq
rmse.lasso
rmse.ridge
