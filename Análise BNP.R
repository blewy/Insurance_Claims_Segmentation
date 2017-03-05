# Packages
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(caret)
library(Matrix)
library(SOAR)
library(Amelia)
library(corrplot)
library(ROCR)
library(gridExtra)


#Dealing with missing Data
getwd()
Sys.setenv(R_LOCAL_CACHE="cache")
ls()

### ----- Import Data --------------

train.i <- fread("./data/train.csv")
test.i <- fread("./data/test.csv")
#summary(train)
#str(train)
cat("\nNumber of Rows in Train: ",nrow(train.i),"Number of columns: ", ncol(train.i))
cat("\nNumber of Rows in Test: ",nrow(test.i),"Number of columns: ", ncol(test.i))

names(test.i)
names(train.i)



#Response Variable
response_name<-setdiff(names(train.i),names(test.i))
train_target <- train.i[,target]
response_name



convClaims <- (sum(train.i$target)/nrow(train.i))*100
cat("\nProportion of CLaim that were suitable for an accelerated approval: ", convClaims, "%")


# Remove id and Target variable from train data set
train.i[,target:=NULL]

# joing both table to be create factors
all.data<-rbind(as.data.frame(train.i),as.data.frame(test.i))
names(all.data)
character_vars <- lapply(all.data, class) == "character" 
all.data[, character_vars] <- lapply(all.data[, character_vars], as.factor)
#integer_vars <- lapply(all.data, class) == "integer" 
#all.data[, integer_vars] <- lapply(all.data[, integer_vars], as.factor)
all.data$ID<-as.integer(all.data$ID)
sapply(all.data, class)
head(all.data)

#Again data.table
train<-data.table(all.data[1:nrow(train.i),],keep.rownames=TRUE)
names(train)
test<-data.table(all.data[(nrow(train.i)+1):nrow(all.data),],keep.rownames=TRUE)
names(test)
head(test)
#remove id from train
train[,ID:=NULL]
train[,rn:=NULL]
test[,rn:=NULL]

setdiff(names(test),names(test.i))


cat("\nNumber of Rows in Train: ",nrow(train),"Number of columns: ", ncol(train))
cat("\nNumber of Rows in Test: ",nrow(test),"Number of columns: ", ncol(test))

names(train)


col_ct <- sapply(train, function(x) length(unique(x)))
colCnt.df <- data.frame(colName = names(train), colCount = col_ct)
cat("\nNumber of columns with constant values: ",sum(col_ct==1))
cat("\nName of constant columns: ", names(train)[which(col_ct==1)])

cat("\n\nRemoving the constant fields from train & test set....")
#train <- train[, names(train)[which(col_ct == 1)] := NULL, with = FALSE]
cat("\nTrain dimensions: ", dim(train))
#test <- test[, names(train)[which(col_ct == 1)] := NULL, with = FALSE]
cat("\nTest dimensions: ", dim(test))


###  -------- Numerical variables -----------------

cat("\n\nNumeric variables on the train set....")
train_num <- train[,names(train)[which(sapply(train, is.numeric))], with = FALSE]
cat("\nColumns with Numeric values: ",names(train_num))
#Show some values
str(lapply(train_num, unique), vec.len = 4)
summary(train_num)
sapply(train_num, class)


# Looking for near zero variable varibles and remove those high correlated ones to
#nzv <- nearZeroVar(train_num, saveMetrics= FALSE)
#to.remove<-names(train_num)[nzv]
#cat("\nNumeric Variables with Near Zero Variance: ", to.remove)
#train.nzv<- train_num[,c(nzv):=NULL]




cat("\nFindind highly correlated variables: ", to.remove)
descrCor<- cor(train_num, y = NULL, use = "na.or.complete")
summary(descrCor[upper.tri(descrCor)])
corrplot(descrCor, order = "hclust")

highlyCorDescr <- findCorrelation(descrCor, cutoff = .85)
cat("\nHighly correlated variables above 0.85 : ", names(train_num)[highlyCorDescr])
names(train_num)[highlyCorDescr]

#Remove these variales
train_num_clean<- train_num[, c(highlyCorDescr):=NULL ]
  
descrCor2<- cor(train_num_clean, y = NULL, use = "na.or.complete")
summary(descrCor2[upper.tri(descrCor2)])
corrplot(descrCor2, order = "hclust")

#Preprocess and input missing values
pp.numeric <- preProcess(train_num_clean,method=c("center", "scale","medianImpute"))
#                 method=c("bagImpute"))
#                 method=c("bagImpute"))
train_num.inputed <- predict(pp.numeric,train_num_clean)


###  -------- Character variables -----------------

#Char vars
#cat("\n\nChar variables train & test set....")
train_factor <- train[,names(train)[which(sapply(train, is.factor))], with = FALSE]
#cat("\nColumns with Character values: ",names(train_char))
#Show some values
str(lapply(train_factor, unique), vec.len = 4)
#summary(train_char)

#Converting the characters to factors
#train_char <- train_char[,(names(train_char)) := lapply(.SD, as.factor), .SDcols =names(train_char)]
#train_char<-as_data_frame(train_char)
#str(train_char)


print("Some factor levels are blanks or whitespaces",quote=F)
print("We can change them to respectively Bl and Ew",quote=F)
fact_var_names<-sapply(train_factor,is.factor)
fact_var_names<-attr(fact_var_names[fact_var_names==T],"names")
replace_lev<-function(x){
  levels(x)[levels(x)==""]<-"Bl"
  levels(x)[levels(x)==" "]<-"Ws"
  return(x)       
}
train_char<-as.data.frame(train_factor)
train_char[,fact_var_names]<-as.data.frame(sapply(train_char[,fact_var_names],replace_lev))
print("Done!",quote=F)

head(train_char)
str(train_char)


#teste<-sapply(train_charv2, as.integer)
#str(teste)

#dummies <- dummyVars( ~ ., data = train_charv2)
#dummied<-  predict(dummies, newdata=train_charv2)
#head(dummied)

cat("Factor column count : ", dim(train_char)[2])

#We can see that 2 facots have to many values, i dint know thw underling structure or ways to agregate this data.class(
#   will remove these two
train_char$v22 <- NULL
train_char$v56 <- NULL
train_char$v125 <- NULL

str(lapply(train_char, unique), vec.len = 4)

#str(lapply(train_charv2, unique), vec.len = 4)
#was.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
#int_train_char <- as.data.frame(sapply(train_charv2, as.integer))

train.all <- as.data.frame(cbind(train_num.inputed,train_char))
train_target_coded<- ifelse(train_target==1, "yes","no")


#head(train_target)
#head(train_target_coded)
train_target_coded<- as.factor(train_target_coded)

table(train_target)
table(train_target_coded)

cat("Train data has", nrow(train.all), "rows and", ncol(train.all), "columns! \n")
str(lapply(train.all, unique), vec.len = 4)


#---------- Looking for missings: All Missings ---------
(vars  <- names(train.all))
mvc <- sapply(train.all[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(train.all)))

cat("\nName of columns with all missings: ", mvn)

#---------- Looking for missings: Many Missings ---------

# Features with any missings
cat("\nName of columns with any missings: ", names(which(mvc>0)))


# Features with % of missings above 70%
names(which(mvc>=0.3*nrow(train.all)))

#----- Proportion of missing values ----- 
cat("\n % of missins of columns with any missings: ")

sapply(train.all[names(which(mvc>0))], function(x) mean(is.na(x)))

#----  All Values constant -----------  

sapply(train.all, function(x) length(unique(x))==1)

names(which(mvc==nrow(train.all)))

sapply(train.all, function(x) class(x))

###  ---------  Cleaning --------



rm(colCnt.df,train, train_char,train_charv2,train_num,train.nzv,test.i,train_factor, train_num_clean,train.i,train_num.inputed,all.data,train_num.inputed,all.data,test,test.i)





###  -------------  Setup Modeling & Cross-Validation ------------

train_matrix <- as.data.frame(model.matrix( ~ .-1, data = train.all, sparse=FALSE))

#table(train_target)
#table(train_target_coded)
train_target_coded <- relevel(train_target_coded, ref = 1)
contrasts(train_target_coded)
#trainIndex <- createDataPartition(train_target_coded, p = .8,
#                                  list = FALSE,
#                                  times = 1)

set.seed(998)
#CvFoldsForTrain <- createFolds(train_target_coded, k = 6,list = TRUE, returnTrain=TRUE)
#str(CvFoldsForTrain)
trainIndex <- createDataPartition(train_target_coded, p = .8,
                                  list = FALSE,
                                  times = 1)
str(trainIndex)


fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 3,
                           ## repeated ten times
                           repeats = 1,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = mnLogLoss)


## -----------------  Random Forest (RF) ---------------

train_sample <- train_matrix[trainIndex,]
target_sample_coded <- (train_target_coded[trainIndex])
#table(target_sample)
dim(train_sample)
length(target_sample_coded)
prop.table(table(target_sample_coded))


mtryValues <- c(5,as.integer(sqrt(ncol(train.all))),15,35,50)
set.seed(998)
rfFit <- train(x = train_sample,
               y = target_sample_coded ,
               method = "rf",
               ntree = 185,
               tuneGrid = data.frame(mtry = mtryValues),
               importance = TRUE,
               trControl = fitControl,
               maximize=FALSE,
               metric = "logLoss"
               )
rfFit
plot(rfFit)


importance <- varImp(rfFit, scale=FALSE)

print(importance)
# plot importance
plot(importance,top = 20)


save(rfFit, file='rfFit.rda') 
rm(rfFit)

### --------  Artificial Neural Networks (ANN)  -------------

nnetGrid <- expand.grid(.size = c(2,4,6,12), .decay = c(0,0.1,0.5,1))
#nnetGrid <- expand.grid(.size = 5 , .decay = c(0.1))
maxSize <- max(nnetGrid$.size)
numWts <- 1*(maxSize * (length(target_sample_coded) + 1) + maxSize + 1)
#number of parameters of the ANN
numWts

set.seed(998)
nnetFit <- train(y = target_sample_coded,
                 x= train_sample,
                 method = "nnet",
                 #preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = numWts,
                 ## ctrl was defined
                 trControl = fitControl,
                 maximize=FALSE,
                 metric = "logLoss")
nnetFit
plot(nnetFit)

save(nnetFit, file='nnetFit.rda') 
rm(nnetFit)



gc()


### --------  Gradient Bosting Trees (GBM) -------------


gbmGrid <-  expand.grid(interaction.depth = c(10,15,25),
                        n.trees = (1:5)*75,
                        shrinkage = c(0.05,0.1),
                        n.minobsinnode = c(20))
#gbmGrid <-  expand.grid(interaction.depth = c(9),
#                        n.trees = (2)*50,
#                        shrinkage = 0.1,
#                        n.minobsinnode = c(20))

set.seed(998)
gbmFit <- train(x = train_sample,
                y = target_sample_coded,
                method = "gbm",
                trControl = fitControl,
                verbose = FALSE,
                tuneGrid = gbmGrid,
                ## Specify which metric to optimize
                maximize=FALSE,
                metric = "logLoss")
gbmFit
plot(gbmFit)

save(gbmFit, file='gbmFit.rda') 
rm(gbmFit)

#data_pred<- predict(gbmFit, newdata = train.all)
#confusionMatrix(data = data_pred, reference = train_target_coded,positive="yes")




### --------  Support Vector Machines (SVM)  -------------
library( kernlab)
data.sigeste <-cbind(target_sample_coded,train_sample)
srange<- sigest(target_sample_coded~.,data = data.sigeste)

svmRGridReduced <- expand.grid(.sigma = srange[1],
                               .C = c(0.01,0.1,1,10))
svmRGridReduced
#2^seq(-4,4)

set.seed(998)
svmRModel <- train(y= target_sample_coded,
                   x= train_sample ,
                   method = "svmRadial", 
                   tuneGrid = svmRGridReduced, 
                   trControl = fitControl,
                   maximize=FALSE,
                   metric = "logLoss")
svmRModel
plot(svmRModel)

save(svmRModel, file='svmRModel.rda') 
rm(svmRModel)



### --------  K Nearest Neighbours (KNN)  -------------

knnGrid <- data.frame(.k = c(as.integer(sqrt(nrow(train_sample)))))

set.seed(998)
knnFit <- train(y = target_sample_coded,
                x= train_sample,
                method = "knn",
                tuneGrid = knnGrid,
                trControl = fitControl,
                maximize=FALSE,
                metric = "logLoss")
knnFit
plot(knnFit)

save(knnFit, file='knnFit.rda') 
rm(knnFit)



###  ------ Loading the models --------

load('rfFit.rda') 
load('gbmFit.rda') 
load('nnetFit.rda') 
load('svmRModel.rda') 
load('knnFit.rda') 


### --------  Comparing models  -------------

# collect resamples
set.seed(998)
results <- resamples(list(RF=rfFit, GBM=gbmFit, SVM=svmRModel, KNN=knnFit, ANN=nnetFit))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)


### ----  Model Staking  -------

#stack_sample_matrix <- train_matrix[unlist(CvFoldsForTrain[1:5]),]
#stack_sample <- train.all[unlist(CvFoldsForTrain[1:5]),]
#stack_target_sample <- (train_target[unlist(CvFoldsForTrain[1:5])])
#stack_target_sample_coded <- (train_target_coded[unlist(CvFoldsForTrain[1:5])])

# RForest
load('rfFit.rda')
trainClasses.rf  <- predict(rfFit, newdata = train_sample,type = "prob")
head(trainClasses.rf[2])
rm(rfFit)
# GBM
load('gbmFit.rda')
trainClasses.gbm <- predict(gbmFit, newdata = train_sample ,type = "prob")
head(trainClasses.gbm[2])
rm(gbmFit)

# ANN
load('nnetFit.rda')
trainClasses.ann <- predict(nnetFit, newdata = train_sample ,type = "prob")
rm(nnetFit)

# SVM
load('svmRModel.rda')
trainClasses.svm <- predict(svmRModel, newdata = train_sample ,type = "prob")
rm(svmRModel)
# KNN
load('knnFit.rda') 
trainClasses.knn <- predict(knnFit, newdata = train_sample ,type = "prob")
rm(knnFit)



model_probs_stack <- data.frame(RForest=trainClasses.rf[2] , GBM=trainClasses.gbm[2] , ANN=trainClasses.ann[2], SVM=trainClasses.svm[2] , KNN= trainClasses.knn[2])

head(model_probs_stack)
names(model_probs_stack)<- c("RForest","GBM","ANN","SVM","KNN")

save(model_probs_stack, file='model_probs_stack.rda') 
write.csv(model_probs, './data/ensemble.csv', row.names = FALSE, quote = FALSE)


#------------------ Logistic regression --------------

# Penalized model grid 

# L1 absolute value ("lasso")   - tends to result in many regression coeffients shrunk exactly to zero and a few other regression coefcients with comparatively little shrinkage
#  L2 quadratic ("ridge") penalty - Smmal but no zero

# .lambda  - 
# .alpha= elasticnet mixing parameter, alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
#grid = expand.grid(.alpha=c(0,0.25,0.5,1),.lambda=seq(0,0.05,by=0.01))

load('model_probs_stack.rda')


set.seed(998)
fitStaking <- trainControl("cv", 5, 
                           savePredictions = FALSE, 
                           classProbs = TRUE,
                            summaryFunction = twoClassSummary)

lrFull <- train(y = target_sample_coded,
                x= model_probs_stack,
              method = "glm", 
              trControl = fitStaking,
              family = binomial, 
              metric = 'ROC', 
              trace = FALSE)

lrFull
save(lrFull, file='lrFull.rda') 


lrFull$finalModel
load('lrFull.rda') 
lrFull$finalModel$coefficients

head(model_probs_stack)
data_pred<- predict(lrFull, newdata = model_probs_stack)

dim(model_probs_stack)
length(target_sample_coded)

head(data_pred)

confusionMatrix(data = data_pred, reference = target_sample_coded ,positive="yes")


### ---------  Test Models ------------

## clean a bit

test_sample <- train_matrix[-trainIndex,]
target_test_coded <- (train_target_coded[-trainIndex])

# RForest
load('rfFit.rda') 
p.rf  <- predict(rfFit, newdata = test_sample ,type = "prob")[2]
rm(rfFit)

#GBM
load('gbmFit.rda') 
p.gbm  <- predict(gbmFit, newdata = test_sample,type = "prob")[2]
rm(gbmFit)

#ANN
load('nnetFit.rda') 
p.ann  <- predict(nnetFit, newdata = test_sample,type = "prob")[2]
rm(nnetFit)   

#SVM
load('svmRModel.rda') 
p.svm  <- predict(svmRModel, newdata = test_sample,type = "prob")[2]
rm(svmRModel)

#KNN
load('knnFit.rda') 
p.knn  <- predict(knnFit, newdata = test_sample,type = "prob")[2]
rm(knnFit)



test_model_probs <- data.frame(RForest=p.rf,GBM=p.gbm,ANN= p.ann,SVM=p.svm,KNN= p.knn)
names(test_model_probs)<- c("RForest","GBM","ANN","SVM","KNN")
head(test_model_probs)
save(test_model_probs, file='test_model_probs.rda') 


load('lrFull.rda') 
p.ensemble<-predict(lrFull, newdata = test_model_probs,type = "prob")[2]

head(test_model_probs)
head(p.ensemble)

data_pred<- predict(lrFull, newdata = test_model_probs)
rm(lrFull)

confusionMatrix(data = data_pred, reference = target_test_coded,positive="yes")

load('test_model_probs.rda')
p.rf<-test_model_probs$RForest
p.gbm<-test_model_probs$GBM
p.ann<-test_model_probs$ANN
p.svm<-test_model_probs$SVM
p.knn<-test_model_probs$KNN


proc <- function(pr.m)
{
pr <- prediction(pr.m, target_test_coded)
pe <- performance(pr, "tpr", "fpr")
au <- performance(pr, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle(deparse(substitute(pr.m)))
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
return(p)
}
grid.arrange(proc(p.rf),proc(p.gbm), proc(p.ann),proc(p.svm),proc(p.knn), proc(p.ensemble),ncol=3)


proc_pre_recall <- function(pr.m)
{
  pr <- prediction(pr.m, target_test_coded)
  pe <- performance(pr, "prec", "rec")
  au <- performance(pr, "auc")@y.values[[1]]
  pd <- data.frame(rec=unlist(pe@x.values), prec=unlist(pe@y.values))
  p <- ggplot(pd, aes(x=rec, y=prec))
  p <- p + geom_line(colour="red")
  p <- p + xlab("Recall") + ylab("Precision")
  p <- p + ggtitle(deparse(substitute(pr.m)))
  #p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
  #                  label=paste("AUC =", round(au, 2)))
  return(p)
}

grid.arrange(proc_pre_recall(p.rf),proc_pre_recall(p.gbm), proc_pre_recall(p.ann),proc_pre_recall(p.svm),proc_pre_recall(p.knn), proc_pre_recall(p.ensemble),ncol=3)
