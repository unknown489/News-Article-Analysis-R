#ensure data is reproducable

set.seed(15040704)

mainData <- read.csv("DataReadyForAnalysis(WC above 300).csv")
mainData <- mainData[-1]

#str(mainData)
#Love and Haha

reactions <- c("angrySad", "angryHaha", "angryWow", "angryLove", "hahaLove",
               "hahaWow", "hahaSad", "wowSad", "wowHaha", "sadLove")
#each group  of reaction has it own data frame
angrySadData <- mainData
angrySadData$Category <- ifelse(((angrySadData$Angry_Count > angrySadData$Haha_Count &
                                    angrySadData$Angry_Count > angrySadData$Wow_Count &
                                    angrySadData$Angry_Count > angrySadData$Love_Count)&
                                   (angrySadData$Sad_Count > angrySadData$Haha_Count &
                                      angrySadData$Sad_Count > angrySadData$Wow_Count &
                                      angrySadData$Sad_Count > angrySadData$Love_Count)
), 1, 0)


NROW(angrySadData[angrySadData$Category == 1,])
###################################################
angryHahaData <- mainData
angryHahaData$Category <- ifelse(((angryHahaData$Angry_Count > angryHahaData$Sad_Count &
                                     angryHahaData$Angry_Count > angryHahaData$Wow_Count &
                                     angryHahaData$Angry_Count > angryHahaData$Love_Count)&
                                    (angryHahaData$Haha_Count > angryHahaData$Sad_Count &
                                       angryHahaData$Haha_Count > angryHahaData$Wow_Count &
                                       angryHahaData$Haha_Count > angryHahaData$Love_Count)), 1, 0)


NROW(angryHahaData[angryHahaData$Category == 1,])
###################################################
angryWowData <- mainData
angryWowData$Category <- ifelse(((angryWowData$Angry_Count > angryWowData$Sad_Count &
                                    angryWowData$Angry_Count > angryWowData$Haha_Count &
                                    angryWowData$Angry_Count > angryWowData$Love_Count)&
                                   (angryWowData$Wow_Count > angryWowData$Sad_Count &
                                      angryWowData$Wow_Count > angryWowData$Haha_Count &
                                      angryWowData$Wow_Count > angryWowData$Love_Count)), 1, 0)


NROW(angryWowData[angryWowData$Category == 1,])
###################################################
angryLoveData <- mainData
angryLoveData$Category <- ifelse(((angryLoveData$Angry_Count > angryLoveData$Sad_Count &
                                     angryLoveData$Angry_Count > angryLoveData$Haha_Count &
                                     angryLoveData$Angry_Count > angryLoveData$Wow_Count)&
                                    (angryLoveData$Love_Count > angryLoveData$Sad_Count &
                                       angryLoveData$Love_Count > angryLoveData$Haha_Count &
                                       angryLoveData$Love_Count > angryLoveData$Wow_Count)), 1, 0)


NROW(angryLoveData[angryLoveData$Category == 1,])
###################################################
hahaLoveData <- mainData
hahaLoveData$Category <- ifelse(((hahaLoveData$Haha_Count > hahaLoveData$Sad_Count &
                                    hahaLoveData$Haha_Count > hahaLoveData$Angry_Count &
                                    hahaLoveData$Haha_Count > hahaLoveData$Wow_Count)&
                                   (hahaLoveData$Love_Count > hahaLoveData$Sad_Count &
                                      hahaLoveData$Love_Count > hahaLoveData$Angry_Count &
                                      hahaLoveData$Love_Count > hahaLoveData$Wow_Count)), 1, 0)


NROW(hahaLoveData[hahaLoveData$Category == 1,])
###################################################
hahaWowData <- mainData
hahaWowData$Category <- ifelse(((hahaWowData$Haha_Count > hahaWowData$Sad_Count &
                                   hahaWowData$Haha_Count > hahaWowData$Angry_Count &
                                   hahaWowData$Haha_Count > hahaWowData$Love_Count)&
                                  (hahaWowData$Wow_Count > hahaWowData$Sad_Count &
                                     hahaWowData$Wow_Count > hahaWowData$Angry_Count &
                                     hahaWowData$Wow_Count > hahaWowData$Love_Count)), 1, 0)


NROW(hahaWowData[hahaWowData$Category == 1,])
########################################################
hahaSadData <- mainData
hahaSadData$Category <- ifelse(((hahaSadData$Haha_Count > hahaSadData$Wow_Count &
                                   hahaSadData$Haha_Count > hahaSadData$Angry_Count &
                                   hahaSadData$Haha_Count > hahaSadData$Love_Count)&
                                  (hahaSadData$Sad_Count > hahaSadData$Wow_Count &
                                     hahaSadData$Sad_Count > hahaSadData$Angry_Count &
                                     hahaSadData$Sad_Count > hahaSadData$Love_Count)), 1, 0)


NROW(hahaSadData[hahaSadData$Category == 1,])
########################################################
wowSadData <- mainData
wowSadData$Category <- ifelse(((wowSadData$Wow_Count > wowSadData$Haha_Count &
                                  wowSadData$Wow_Count > wowSadData$Angry_Count &
                                  wowSadData$Wow_Count > wowSadData$Love_Count)&
                                 (wowSadData$Sad_Count > wowSadData$Haha_Count &
                                    wowSadData$Sad_Count > wowSadData$Angry_Count &
                                    wowSadData$Sad_Count > wowSadData$Love_Count)), 1, 0)


NROW(wowSadData[wowSadData$Category == 1,])
########################################################
wowHahaData <- mainData
wowHahaData$Category <- ifelse(((wowHahaData$Wow_Count > wowHahaData$Sad_Count &
                                   wowHahaData$Wow_Count > wowHahaData$Angry_Count &
                                   wowHahaData$Wow_Count > wowHahaData$Love_Count)&
                                  (wowHahaData$Haha_Count > wowHahaData$Sad_Count &
                                     wowHahaData$Haha_Count > wowHahaData$Angry_Count &
                                     wowHahaData$Haha_Count > wowHahaData$Love_Count)), 1, 0)


NROW(wowHahaData[wowHahaData$Category == 1,])
########################################################
sadLoveData <- mainData
sadLoveData$Category <- ifelse(((sadLoveData$Sad_Count > sadLoveData$Wow_Count &
                                   sadLoveData$Sad_Count > sadLoveData$Angry_Count &
                                   sadLoveData$Sad_Count > sadLoveData$Haha_Count)&
                                  (sadLoveData$Love_Count > sadLoveData$Wow_Count &
                                     sadLoveData$Love_Count > sadLoveData$Angry_Count &
                                     sadLoveData$Love_Count > sadLoveData$Haha_Count)), 1, 0)


NROW(sadLoveData[sadLoveData$Category == 1,])
########################################################


#columns are remove that are not relevent to our analysis

angrySadData <- subset(angrySadData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                                 , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                                 , Organization))
angryHahaData <- subset(angryHahaData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                                   , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                                   , Organization))
angryWowData <- subset(angryWowData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                                 , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                                 , Organization))
angryLoveData <- subset(angryLoveData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                                   , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                                   , Organization))
hahaLoveData <- subset(hahaLoveData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                                 , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                                 , Organization))
hahaWowData <- subset(hahaWowData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                               , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                               , Organization))
hahaSadData <- subset(hahaSadData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                               , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                               , Organization))
wowSadData <- subset(wowSadData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                             , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                             , Organization))
wowHahaData <- subset(wowHahaData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                               , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                               , Organization))
sadLoveData <- subset(sadLoveData, select = -c(ID, Created_Time, Share_Count, Comment_Count, Likes_Count
                                               , Haha_Count, Wow_Count, Sad_Count, Angry_Count, Love_Count
                                               , Organization))


############################################

angryLoveData$Category <- factor(angryLoveData$Category)
angryWowData$Category <- factor(angryWowData$Category)
angryHahaData$Category <- factor(angryHahaData$Category)
angrySadData$Category <- factor(angrySadData$Category)
hahaLoveData$Category <- factor(hahaLoveData$Category)
hahaWowData$Category <- factor(hahaWowData$Category)
hahaSadData$Category <- factor(hahaSadData$Category)
wowSadData$Category <- factor(wowSadData$Category)
wowHahaData$Category <- factor(wowHahaData$Category)
sadLoveData$Category <- factor(sadLoveData$Category)

#reactions
mainData2 <- hahaSadData
#install.packages("splitstackshape")

library(splitstackshape)
library(caret)
library(e1071)
library(pROC)

completeDataCaptured <- 200
for(i in 1:NROW(reactions)){
  if(i == 1){mainData2 <- angrySadData} else
    if(i == 2){mainData2 <- angryHahaData} else
      if(i == 3){mainData2 <- angryWowData} else
        if(i == 4){mainData2 <- angryLoveData} else
          if(i == 5){mainData2 <- hahaLoveData} else
            if(i == 6){mainData2 <- hahaWowData} else
              if(i == 7){mainData2 <- hahaSadData} else
                if(i == 8){mainData2 <- wowSadData} else
                  if(i == 9){mainData2 <- wowHahaData} else
                    if(i == 10){mainData2 <- sadLoveData}
  
  for(j in 1:10){
    
    sizee <- NROW(mainData2[mainData2$Category == 1,])
    
    
    temppp <- stratified(mainData2, "Category", size = sizee)
    
    outputSize <- capture.output(NROW(temppp))
    ####################################################
    sample <- createDataPartition(y = temppp$Category, p = .75, list = FALSE) 
    train <- temppp[sample, ]
    test <- temppp[-sample, ]
    
    
    
    ######################SVM##########################
    
    svm.model <- svm(Category ~., data = train)
    scm.test <- predict(svm.model, subset(test,select=-c(Category)), type = "raw")
    
    #capture1 <- capture.output(confusionMatrix(scm.test, test$Category))
    
    
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
    #plot.roc(ROC1)
    
    dataCaptured$AUC <- ROC1$auc
    
    #remove(dataCaptured)
    dataCaptured <- dataCaptured[-1]
    if(completeDataCaptured == 200){
      completeDataCaptured <- dataCaptured
    } else {
      completeDataCaptured <- rbind(completeDataCaptured, dataCaptured)
    }
    
    
    
  }
  
  #preparing CSV file for analysis on Tableau
  file_name <- paste("SVMMultipleTests GROUPED" , "csv", sep=".")
  write.csv(completeDataCaptured , file = file_name)
  
  
}
