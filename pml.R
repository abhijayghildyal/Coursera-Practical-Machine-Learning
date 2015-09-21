### set seed example
set.seed(1)
rnorm(4)
set.seed(2)
rnorm(4)
#### Thus set.seed is for reproducible random result

library(caret)
library(randomForest)

setwd("/media/maverick/New Volume/DS/PML")

training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing<-read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
set.seed(10)
#set.seed(12345)
#head(training)
#names(training)

inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
#inTrain <- createDataPartition(y=ptrain$classe, p=0.7, list=F)
myTraining <- training[inTrain, ]; 
myTesting <- training[-inTrain, ]
#dim(myTraining); 
#dim(myTesting)

##myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)
##saveMetrics
##a logical. If false, the positions of the zero- or near-zero predictors is returned. If true, a data frame with predictor information is returned.
nzv<- nearZeroVar(myTraining,saveMetrics = T)
##myDataNZV[myDataNZV$nzv,]
##myTraining <- myTraining[c(-1)]
##myTraining<- myTraining[-myDataNZV,]
#names(myTraining)
#head(myTraining)
myTraining <- myTraining[,nzv$nzv==FALSE]
myTraining <- myTraining[c(-1)]
myTraining <- myTraining[,-(1:4)]

## grep(names(myTraining[2]))
## length(myTraining)  ##no. of columns


trainingV3 <- myTraining
for(i in 1:length(myTraining)) {
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .7) {
    for(j in 1:length(trainingV3)) {
      if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) == 1)  {
        trainingV3 <- trainingV3[ , -j]
      }   
    } 
  }
}
dim(myTraining)

#clean1 <- colnames(myTraining)
#clean2 <- colnames(myTraining[, -127])

#myTesting <- myTesting[clean1]         # allow only variables in myTesting that are also in myTraining
#testing <- testing[clean2]  ### IMP load testing dataset

#len<-length(names(myTraining))
#names(trainingV3)
myTraining <- trainingV3
rm(trainingV3)

clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -54])
#names(myTraining)
myTesting <- myTesting[clean1]
testing <- testing[clean2]

#names(testing)
modFitB1 <- randomForest(classe ~. , data=myTraining)
predictionsB1 <- predict(modFitB1, myTesting, type = "class")
confusionMatrix(predictionsB1, myTesting$classe)

predictionsB1 <- predict(modFitB1, testing, type = "class")


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}