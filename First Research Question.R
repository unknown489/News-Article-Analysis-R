#ensure data is reproducable
set.seed(15040704)

mainData <- read.csv("DataReadyForAnalysis(WC above 300).csv")
mainData <- mainData[-1]

#str(mainData)
#Love and Haha

#each reaction has it own data frame
angryData <- mainData
angryData$Category <- ifelse((angryData$Angry_Count > angryData$Haha_Count &
                                angryData$Angry_Count > angryData$Wow_Count &
                                angryData$Angry_Count > angryData$Sad_Count &
                                angryData$Angry_Count > angryData$Love_Count), 1, 0)
NROW(angryData[angryData$Category == 1,])
###################################################
wowData <- mainData
wowData$Category <- ifelse((wowData$Wow_Count > wowData$Haha_Count &
                              wowData$Wow_Count > wowData$Angry_Count &
                              wowData$Wow_Count > wowData$Sad_Count &
                              wowData$Wow_Count > wowData$Love_Count), 1, 0)
NROW(wowData[wowData$Category == 1,])
###################################################
sadData <- mainData
sadData$Category <- ifelse((sadData$Sad_Count > sadData$Haha_Count &
                              sadData$Sad_Count > sadData$Angry_Count &
                              sadData$Sad_Count > sadData$Wow_Count &
                              sadData$Sad_Count > sadData$Love_Count), 1, 0)
NROW(sadData[sadData$Category == 1,])
###################################################
loveData <- mainData
loveData$Category <- ifelse((loveData$Love_Count > loveData$Haha_Count &
                               loveData$Love_Count > loveData$Angry_Count &
                               loveData$Love_Count > loveData$Sad_Count &
                               loveData$Love_Count > loveData$Wow_Count), 1, 0)
NROW(loveData[loveData$Category == 1,])
###################################################
hahaData <- mainData
hahaData$Category <- ifelse((hahaData$Haha_Count > hahaData$Wow_Count &
                               hahaData$Haha_Count > hahaData$Angry_Count &
                               hahaData$Haha_Count > hahaData$Sad_Count &
                               hahaData$Haha_Count > hahaData$Love_Count), 1, 0)
NROW(hahaData[hahaData$Category == 1,])
###################################################


#columns are remove that are not relevent to our analysis
angryData <- subset(angryData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                           , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                           , Organization))
hahaData <- subset(hahaData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                           , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                           , Organization))
loveData <- subset(loveData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                           , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                           , Organization))
sadData <- subset(sadData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                           , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                           , Organization))
wowData <- subset(wowData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                           , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                           , Organization))

############################################

angryData$Category <- factor(angryData$Category)
hahaData$Category <- factor(hahaData$Category)
loveData$Category <- factor(loveData$Category)
sadData$Category <- factor(sadData$Category)
wowData$Category <- factor(wowData$Category)

reactions <- c("wowData", "hahaData", "loveData", "sadData", "angryData")

#install.packages("splitstackshape")
#mainData2 <- wowData
library(splitstackshape)
library(caret)
library(e1071)
library(pROC)
completeDataCaptured <- 200
for(i in 1:NROW(reactions)){
if(i == 1){mainData2 <- wowData} else
  if(i == 2){mainData2 <- hahaData} else
    if(i == 3){mainData2 <- loveData} else
      if(i == 4){mainData2 <- sadData} else
        if(i == 5){mainData2 <- angryData}
  
    for(j in 1:10){
      
    
      
    #stratifying the data
    sizee <- NROW(mainData2[mainData2$Category == 1,])
    temppp <- stratified(mainData2, "Category", size = sizee)
    ##################Createing training and test##################################
    sample <- createDataPartition(y = temppp$Category, p = .75, list = FALSE) 
    train <- temppp[sample, ]
    test <- temppp[-sample, ]
    
    
    
    ######################SVM##########################
    
    svm.model <- svm(Category ~., data = train)
    scm.test <- predict(svm.model, subset(test,select=-c(Category)), type = "raw")
    
    #capture1 <- capture.output(confusionMatrix(scm.test, test$Category))
    confusionMatrix(scm.test, test$Category)
    caonfustionMatrixData <- confusionMatrix(scm.test, test$Category)
    caonfustionMatrixData$positive
    dataCaptured <- as.data.frame(NA)
    dataCaptured$Type <- reactions[i]
    dataCaptured$positive <- caonfustionMatrixData$positive
    dataCaptured$Accuracy <- caonfustionMatrixData$overall[1]
    dataCaptured$Kappa <- caonfustionMatrixData$overall[2]
    dataCaptured$AccuracyLower <- caonfustionMatrixData$overall[3]
    dataCaptured$AccuracyUpper <- caonfustionMatrixData$overall[4]
    dataCaptured$AccuracyNull <- caonfustionMatrixData$overall[5]
    dataCaptured$AccuracyPValue <- caonfustionMatrixData$overall[6]
    dataCaptured$McnemarPValue <- caonfustionMatrixData$overall[7]
    
    dataCaptured$Sensitivity <- caonfustionMatrixData$byClass[1]
    dataCaptured$Specificity <- caonfustionMatrixData$byClass[2]
    dataCaptured$Pos_Pred_Value  <- caonfustionMatrixData$byClass[3]
    dataCaptured$Neg_Pred_Value <- caonfustionMatrixData$byClass[4]
    dataCaptured$Precision <- caonfustionMatrixData$byClass[5]
    dataCaptured$Recall <- caonfustionMatrixData$byClass[6]
    dataCaptured$F1 <- caonfustionMatrixData$byClass[7]
    dataCaptured$Prevalence <- caonfustionMatrixData$byClass[8]
    dataCaptured$Detection_Rate <- caonfustionMatrixData$byClass[9]
    dataCaptured$Detection_Prevalence <- caonfustionMatrixData$byClass[10]
    dataCaptured$Balanced_Accuracy <- caonfustionMatrixData$byClass[11]
    dataCaptured$Size <- sizee
    
    test1 <- test
    test1$Prediction1 <- scm.test
    ROC1 <- roc(as.numeric(test1$Category), as.numeric(test1$Prediction1))
    
    dataCaptured$AUC <- ROC1$auc
    
    #remove(dataCaptured)
    dataCaptured <- dataCaptured[-1]
    if(completeDataCaptured == 200){
      completeDataCaptured <- dataCaptured
    } else {
      completeDataCaptured <- rbind(completeDataCaptured, dataCaptured)
    }
    
    
    #test1 <- test
    #test1$Prediction1 <- scm.test
    #ROC1 <- multiclass.roc(as.numeric(test1$Category), as.numeric(test1$Prediction1))
    #https://stackoverflow.com/questions/34169774/plot-roc-for-multiclass-roc-in-proc-package
    #capture2 <- capture.output(ROC1$auc)
    #rs <- ROC1[['rocs']]
    
    
    #file_name_img <- paste(reactions[i] , "png", sep=".")
    #png(filename= file_name_img)
    
    
    #plot.roc(rs[[1]])
    
    #dev.off()
    
    
    #file_name <- paste(reactions[i] , "txt", sep=".")
    
    #cat(capture1,file=file_name,sep="\n",append=TRUE)
    #cat(capture2,file=file_name,sep="\n",append=TRUE)
    
    #remove(mainData2, sample, train, test)
    }
  
  #preparing CSV file for analysis on Tableau
  file_name <- paste("SVMMultipleTestsUpdated" , "csv", sep=".")
  write.csv(completeDataCaptured , file = file_name)
  
}




#dataBackup <- completeDataCaptured

#wowDataAnalysis <- completeDataCaptured[completeDataCaptured$Type == 1,]
#wowDataAnalysis <- subset(wowDataAnalysis, select = -c(positive))


#wowDataAnalysis1 <- as.data.frame(colMeans(wowDataAnalysis))


#install.packages("fBasics")
#library(fBasics)

#wowDataAnalysis2 <- as.data.frame(colStdevs(wowDataAnalysis))

#str(wowDataAnalysis)
##################################################################################################
#####################The code below are the other classifier tests done###########################
##########################to help decide which classifier to use.#################################
##################################################################################################


####################Ramdom Tree########################

#install.packages("randomForest")
library(randomForest)
nameS <- names(subset(mainData2, select = -c(Category)))
formulA <- as.formula(paste("Category~", paste(nameS, collapse = " + ")))
ranDomF <- randomForest(formulA , data = train, ntree=1000 )



scm.test <- predict(ranDomF, test)

confusionMatrix(scm.test, test$Category, positive = "spam")


test1 <- test
test1$Prediction1 <- scm.test
ROC1 <- multiclass.roc(as.numeric(test1$Category), as.numeric(test1$Prediction1))
#https://stackoverflow.com/questions/34169774/plot-roc-for-multiclass-roc-in-proc-package
ROC1$auc
rs <- ROC1[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))













######################GLM (Generalized Linear Models)##########################
test$Category <- as.numeric(test$Category)
train$Category <- as.numeric(train$Category)

genLiMo <- glm(Category ~., data = train, family=gaussian)

abline(genLiMo)
scm.test <- predict(genLiMo, test)


install.packages("hydroGOF")
library("hydroGOF")
RMSE = rmse(scm.test, test$Category)

#####################ANN################################


library(neuralnet)
train$Category <- as.numeric(train$Category)
test$Category <- as.numeric(test$Category)
nameS <- names(subset(temppp, select = -c(Category)))
formulA <- as.formula(paste("Category~", paste(nameS, collapse = " + ")))
network <- neuralnet(formulA, data = train,linear.output=F)
network$result.matrix
plot(network)


nn.results <- compute(network, subset(test, select = -c(Category)))
results <- data.frame(actual = test$Category, prediction = nn.results$net.result)
results
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)






scm.test <- predict(network, test)


confusionMatrix(scm.test, test$Category, positive = "spam")

