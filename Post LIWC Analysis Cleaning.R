#cleaning the LIWC file
mainData <- read.csv("LIWC2015 Results (CompleteDataFinal.csv).csv")

#remove column(contains irrelevent data)
mainData <- mainData[-1]

#column name were removed, so code below should place the column names back.
colnames(mainData)[1]<-"ExpandedURL"
colnames(mainData)[2]<-"ID"
colnames(mainData)[3]<-"From_ID"
colnames(mainData)[4]<-"From_Name"
colnames(mainData)[5]<-"Message"
colnames(mainData)[6]<-"Created_Time"
colnames(mainData)[7]<-"Type"
colnames(mainData)[8]<-"Link"
colnames(mainData)[9]<-"Story"
colnames(mainData)[10]<-"Organization"
colnames(mainData)[11]<-"Likes_Count"
colnames(mainData)[12]<-"Comment_Count"
colnames(mainData)[13]<-"Share_Count"
colnames(mainData)[14]<-"Love_Count"
colnames(mainData)[15]<-"Haha_Count"
colnames(mainData)[16]<-"Wow_Count"
colnames(mainData)[17]<-"Sad_Count"
colnames(mainData)[18]<-"Angry_Count"
colnames(mainData)[19]<-"MainText"

#remove first row, as it contains the column name that was analysed by the LIWC
mainData <- mainData[-1,]

#we will only use the article that have word count greater than 300.
mainData <- mainData[mainData$WC >= 300,]

file_name <- paste("DataReadyForAnalysis(WC above 300)" , "csv", sep=".")
write.csv(mainData , file = file_name)
