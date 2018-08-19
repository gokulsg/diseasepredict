library(ggplot2)
library(pROC)
disease <- read.csv("disease.csv",header=FALSE,sep=",",na.strings = '?')
names(disease) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg","thalach","exang", "oldpeak","slope", "ca", "thal", "num")
attach(disease)
head(disease)
dim(disease)
disease$num<-ifelse(disease$num > 0,"Disease","noDisease")
table(disease$num)
ggplot(disease,aes(x = num)) +geom_bar(fill="black")
disease$sex<-ifelse(disease$sex==0,"female","male")
table(disease$sex)
table(sex=disease$sex,disease=disease$num)
ggplot(disease,aes(x=sex)) +geom_bar(fill="purple") +facet_wrap(~num)
by(disease$age,disease$num,summary)
ggplot(disease,aes(x = num,y = age)) +geom_boxplot()
cor.test(age,chol) 
table(cp,num)
table(exang,num)
cor.test(age,thalach)
ggplot(disease,aes(x = age,y = thalach )) + geom_point() +  geom_smooth()
library(caret)
set.seed(20)
partition <- createDataPartition(disease$num,p=0.7,list=FALSE)
train <- disease[partition,]
test<- disease[-partition,]
feature.names=names(disease)
for (f in feature.names) {
  if (class(disease[[f]])=="factor") {
    levels <- unique(c(disease[[f]]))
    disease[[f]] <- factor(disease[[f]],
                           labels=make.names(levels))
  }
}
disease$num<-as.factor(disease$num)
levels(disease$num) <- c("Notdisease","Disease")
table(disease$num)
divide <- createDataPartition(disease$num,p=0.7,list=FALSE)
train2 <- disease[divide,]
test2 <-  disease[-divide,]
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,classProbs = TRUE,summaryFunction = twoClassSummary)
svmModel <- train(num ~ ., data = na.omit(train2),method = "svmRadial",trControl = fitControl,preProcess = c("center", "scale"),tuneLength = 8,metric = "ROC")
svmModel
svmPredict <- predict(svmModel, test2)
predictprob <- predict(svmModel, test2, type='prob')[2]
ConfMatrixPredict <- confusionMatrix(svmPredict, na.omit(test2)$num)
ConfMatrixPredict$table
AUC<- roc(na.omit(test2)$num,as.numeric(as.matrix((predictprob))))$auc
Accuracy<- ConfMatrixPredict$overall['Accuracy'] 
svmPerformance<-cbind(AUC,Accuracy)
svmPerformance
