set.seed(1)
id=read.csv("revdata.csv",header=T)[,2:35]
test_data=read.csv("test_data.csv",header=T)[,2:35]
str(id)
str(test_data)
id<-id[,-1]
test_data<-test_data[,-1]
str(id)
str(test_data)
id$Claim_Amount=as.factor(ifelse(id$Claim_Amount>0,"Yes","No"))
test_data$Claim_Amount=as.factor(ifelse(test_data$Claim_Amount>0,"Yes","No"))

levels(test_data$Blind_Make) <- levels(id$Blind_Make)
levels(test_data$Blind_Model) <- levels(id$Blind_Model)
levels(test_data$Blind_Submodel) <- levels(id$Blind_Submodel)
levels(test_data$Cat1) <- levels(id$Cat1)
levels(test_data$Cat2) <- levels(id$Cat2)
levels(test_data$Cat3) <- levels(id$Cat3)
levels(test_data$Cat4) <- levels(id$Cat4)
levels(test_data$Cat5) <- levels(id$Cat5)
levels(test_data$Cat6) <- levels(id$Cat6)
levels(test_data$Cat7) <- levels(id$Cat7)
levels(test_data$Cat8) <- levels(id$Cat8)
levels(test_data$Cat9) <- levels(id$Cat9)
levels(test_data$Cat10) <- levels(id$Cat10)
levels(test_data$Cat11) <- levels(id$Cat11)
levels(test_data$Cat12) <- levels(id$Cat12)
levels(test_data$NVCat) <- levels(id$NVCat)


str(id)
str(test_data)

b<-as.data.frame(table(id$Claim_Amount))

# subample the super majority class to reduce the ratio to 20:1 ~ undersampling
maj <- id[id$Claim_Amount=="No",]
subid <- sample(1:dim(maj)[1], size=b[2,2]*20, replace=FALSE)
ids <- rbind(id[id$Claim_Amount=="Yes",],maj[subid,])
table(ids$Claim_Amount)
summary(ids)

a<-as.data.frame(table(ids$Claim_Amount))

library(randomForest)
rf.df=randomForest(Claim_Amount~.,data=ids,importance=T, prox=TRUE, strata=ids$Claim_Amount, sampsize=c(a[2,2],a[2,2]))

# make prediction on the test set
test.pred1 <- predict(rf.df,newdata=test_data)
# compare it to the actual outcome
confusionMatrix <- table(predicted=test.pred1,observed=test_data$Claim_Amount)
confusionMatrix
confusionMatrix[2,2]/sum(confusionMatrix[,2]) #sensitivity
(confusionMatrix[1,1]+confusionMatrix[2,2])/(sum(confusionMatrix[,1])+sum(confusionMatrix[,2])) #accuracy


#ROC plot and AUC
library(ROCR)
library(pROC)
rocplot =function (pred , truth , ...){ predob = prediction (pred , truth) 
perf = performance (predob , "tpr", "fpr") 
plot(perf ,...)}
testpred2=predict(rf.df,newdata = test_data,type = "prob")
rocplot(testpred2[,2],test_data$Claim_Amount)
abline(0,1)
auc(test_data$Claim_Amount,testpred2[,2])

