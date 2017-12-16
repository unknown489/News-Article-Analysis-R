library(devtools)
install_github("mannau/boilerpipeR", args = "--no-multiarch")
install.packages("R.utils")
install.packages("longurl")
library(boilerpipeR)

library(R.utils)
library(httr)
library(plyr)
library(dplyr)
library(longurl)
library(compare)
packageVersion("longurl")

#Get the cleaned Facebook posts with links
mainData <- read.csv("ABCLinkOrgUniquePost.csv")


#when we first created a csv file, a column was created by the R. It needs to be removed
mainData <- mainData[-1]

#First we convert the short URL to long URLs

#convert the original type to charector, so that the url can be read
mainData$link <- as.character(mainData$link)

mainData$expandedURL <- NA

#getting original url
for(i in 1:(NROW(mainData))){ #NROW(dataSubset)
  url <- NA
  errorCount <- 0
  
  while(is.na(url)){
    
    url <- NA
    url <- tryCatch({
      withCallingHandlers({
        withRestarts({ 
          #
          withTimeout(expand_urls(mainData$link[i]), timeout = 4, onTimeout = "error")
          
        }, restartLoop = function(e) {
          print(paste("MY_INNER_restart:  ",e))
          return(NA)
        }) 
        
      }, error = function(e) {
        print(paste("MY_INNER_Error:  ",e))
        invokeRestart("restartLoop")
      }) 
      
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      return(NA)
      
    }, error = function(err) {
      print(paste("MY_ERROR:  ",err))
      return(NA)
    },  finally = {
      
    })
    
    
    errorCount <- errorCount + 1
    if(errorCount >= 3){
      break
    }
    if(is.na(url)){
      Sys.sleep(1)
    }
  }
  if(!is.na(url)){
  mainData$expandedURL[i] <- url$expanded_url
  print(paste("", i))
  }
  
  
}
####################Precaution Step taken to save expanded URL###########
file_name <- paste("ABCUnCleanURLExtendedData" , "csv", sep=".")
write.csv(mainData , file = file_name)

mainData <- read.csv("ABCUnCleanURLExtendedData.csv")

mainData <- mainData[-1]
#########################################################################

###########################################################
#Removing posts with expanded URL that were not retrived.
#remove na expandedURL rows
mainData <- mainData[!is.na(mainData$expandedURL),]

#remove empty expandedURL rows
mainData <- mainData[ !mainData$expandedURL == "",]
###########################################################

#Remove links that have do not point towards an article. Using charector count, i can check if the link
#is directing towards the main page or the article page. While proper news atricle URL links are long, 
#the home page not. For this perticular example, I used 45 charector count.

mainData$expandedURL <- as.character(mainData$expandedURL)

#visually check the URLs. If they redirect to only main page, remove them by running the 
#code on line 109. Otherwise, do not run the code.
shortURLData <- mainData[nchar(mainData$expandedURL) <= 45,]
shortURLData

mainData <- mainData[nchar(mainData$expandedURL) > 45,]

#checking for duplicate and non duplicated urls
#non_dup <- subset(mainData3, !duplicated(mainData3$expandedURL))
#dup <- subset(mainData3, duplicated(mainData3$expandedURL))

library(plyr)

#Here the data is prepared for combining the posts with same original URLs
mainData$likes_count <- as.numeric(mainData$likes_count)
mainData$comments_count <- as.numeric(mainData$comments_count)
mainData$sad_count <- as.numeric(mainData$sad_count)
mainData$shares_count <- as.numeric(mainData$shares_count)
mainData$love_count <- as.numeric(mainData$love_count)
mainData$haha_count <- as.numeric(mainData$haha_count)
mainData$wow_count <- as.numeric(mainData$wow_count)
mainData$angry_count <- as.numeric(mainData$angry_count)

#The result of the ddply only returns the columns with numeric values, so they need 
#to be joined with the original facebook post
mainData4 <- ddply(mainData, .(expandedURL),  numcolwise(sum))

#from_id is numeric, so it needs to be remove from joining as it needs to be unique,
#and a sum of 2 different facebook posts.
mainData4 <- subset(mainData4, select = -c(from_id))

#preparing data frame for joining

#Duplicated values are converted to single instances. You can confirm this by looking at the number
# of observations of mainData4 and mainData5 in the top right side of R Studio.
mainData5 <- subset(mainData, !duplicated(mainData$expandedURL))

#the original counts are removed
mainData6 <- subset(mainData5, select = -c(likes_count,comments_count,sad_count,
                                           shares_count,love_count,haha_count,
                                           wow_count,angry_count))

#joining the 2 data frames.
mainData7 <- merge(mainData6, mainData4, by = "expandedURL")

##############################################################################################
#Preparing to retrive the news articles.


mainData7$expandedURL <- as.character(mainData7$expandedURL)
mainData7$mainText <- NA

file_name <- paste("ABCUnCleanURLExtendedDataV2" , "csv", sep=".")
write.csv(mainData7 , file = file_name)

for(i in 1:(NROW(mainData7))){
  
  maintext <- NA
  errorCount <- 0
  
  #too many error were generated because of the API, so in order to handle it, and automate the whole process, 
  #trycatch was initially used, but it was unable to handle is, so using combination of withCallHandlers and
  #withRestarts was used to handle these situations. The reason I can find for trycatch not working is that it was unable to 
  #handle error generated by java code of the boilerpipeR
  while(is.na(maintext)){
    
    
    maintext <- tryCatch({
      withCallingHandlers({
        withRestarts({ 
          #
          withTimeout(ArticleExtractor(mainData7$expandedURL[i], asText = FALSE), timeout = 4, onTimeout = "error")
          
        }, restartLoop = function(e) {
          print(paste("MY_INNER_restart:  ",e))
          return(NA)
        }) 
        
      }, error = function(e) {
        print(paste("MY_INNER_Error:  ",e))
        invokeRestart("restartLoop")
      }) 
      
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      return(NA)
      
    }, error = function(err) {
      print(paste("MY_ERROR:  ",err))
      return(NA)
    },  finally = {
      
    })
    
    
    errorCount <- errorCount + 1
    if(errorCount >= 3){
      break
    }
    if(is.na(maintext)){
      Sys.sleep(1)
    }
  }
  mainData7$mainText[i] <- maintext
  print(paste("", i))
  
  
}

######################################################################################
#Saving data retrived.
file_name <- paste("ABCCompleteDataUNCLEAN" , "csv", sep=".")
write.csv(mainData7 , file = file_name)

mainData <- read.csv("ABCCompleteDataUNCLEAN.csv")

#Removing post that did not get the news articles text.
#remove na mainText rows
mainData1 <- mainData[!is.na(mainData$mainText),]

#remove empty mainText rows
mainData2 <- mainData1[ !mainData1$mainText == "",]

###################################################################################################################
#################################The following code is for cleaning ABC news articles only#########################
###################################################################################################################

#remove mainText with "External links are provided for reference purposes. ABC News is not responsible for the content of external Internet sites.
#Copyright © 2017 ABC News Internet Ventures. Yahoo! - ABC News Network", as it only returns to main site.


mainData3 <- as.data.frame(sapply(mainData2,gsub,
                                  pattern="External links.*ABC News Network", replacement=NA))
mainData4 <- mainData3[!is.na(mainData3$mainText),]


#removing string that are irrelevent to our analysis

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="Yahoo!.*reserved.\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="0 Shares\n", replacement=""))

mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="Email\n", replacement=""))

mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Add Interest\n", replacement=""))

mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="<iframe.*</iframe>\n", replacement=""))


mainData11 <- as.data.frame(sapply(mainData10,gsub,
                                   pattern="EMBED.*Videos\n", replacement=""))


mainData12 <- as.data.frame(sapply(mainData11,gsub,
                                   pattern="Related Topics:\n", replacement=""))

dataCleaning <- mainData12

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning BBC news articles only#########################
###################################################################################################################



#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData2,gsub,
                                  pattern="These are external links and will open in a new window\n", replacement=""))
mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Close share panel\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="Media playback is unsupported on your device\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Related Topics\n", replacement=""))

#look into the one below to see if you can fix i later. ask the advisor
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="Image copyright .*\n]", replacement=""))

dataCleaning <- mainData7
###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning CNN news articles only#########################
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData2,gsub,
                                  pattern="must watch\n", replacement=""))


dataCleaning <- mainData3

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning Fox news articles only#########################
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData2,gsub,
                                  pattern="Read Full Article\n", replacement=""))

mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Published .* Print\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="Facebook .* Print\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Advertisement\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="Like us on Facebook\n", replacement=""))



mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Follow us on Twitter & Instagram\n", replacement=""))



mainData9 <- as.data.frame(sapply(mainData8,gsub,
                                  pattern="More on this...\n", replacement=""))
mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="More On This...\n", replacement=""))

dataCleaning <- mainData10

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning Guardian news articles only####################
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData2 <- as.data.frame(sapply(mainData,gsub,
                                  pattern="Read more\n", replacement=""))

mainData3 <- as.data.frame(sapply(mainData2,gsub,
                                  pattern="Monday.*GMT", replacement=""))

mainData3 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Monday.*BST", replacement=""))
mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Tuesday.*GMT\n", replacement=""))
mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="Wednesday.*GMT\n", replacement=""))
mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Thursday.*GMT\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="Friday.*GMT\n", replacement=""))
mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Saturday.*GMT\n", replacement=""))
mainData9 <- as.data.frame(sapply(mainData8,gsub,
                                  pattern="Sunday.*GMT\n", replacement=""))
mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="Last.*GMT\n", replacement=""))
mainData11 <- as.data.frame(sapply(mainData10,gsub,
                                   pattern="Last.*BST\n", replacement=""))
mainData12 <- as.data.frame(sapply(mainData11,gsub,
                                   pattern="First.*GMT\n", replacement=""))
mainData13 <- as.data.frame(sapply(mainData12,gsub,
                                   pattern="Last modified on", replacement=""))


dataCleaning <- mainData13

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning LA Times news articles only####################
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData2,gsub,
                                  pattern="Read Full Article\n", replacement=""))

mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Published .* Print\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="Facebook .* Print\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Advertisement\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="Like us on Facebook\n", replacement=""))



mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Follow us on Twitter & Instagram\n", replacement=""))

dataCleaning <- mainData8

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning NBC news articles only####################
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData,gsub,
                                  pattern="advertisement\n", replacement=""))

mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Subscribe\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="SIGN UP\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Follow.*Instagram\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="autoplay.*</iframe>\n", replacement=""))

mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Play\n", replacement=""))

mainData9 <- as.data.frame(sapply(mainData8,gsub,
                                  pattern="Follow.*Tumblr .", replacement=""))

mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="Follow.*Instagram .", replacement=""))

mainData11 <- as.data.frame(sapply(mainData10,gsub,
                                   pattern="Download the App\n", replacement=""))
mainData12 <- as.data.frame(sapply(mainData11,gsub,
                                   pattern="Available for IOS and Android\n", replacement=""))
mainData13 <- as.data.frame(sapply(mainData12,gsub,
                                   pattern="Follow necn\n", replacement=""))
mainData14 <- as.data.frame(sapply(mainData13,gsub,
                                   pattern="Email\n", replacement=""))
mainData15 <- as.data.frame(sapply(mainData14,gsub,
                                   pattern="Contributors\n", replacement=""))

mainData16 <- as.data.frame(sapply(mainData15,gsub,
                                   pattern="Follow NBC New York\n", replacement=""))
mainData17 <- as.data.frame(sapply(mainData16,gsub,
                                   pattern="Follow NBC4\n", replacement=""))
mainData18 <- as.data.frame(sapply(mainData17,gsub,
                                   pattern="Follow NBC Chicago\n", replacement=""))
mainData19 <- as.data.frame(sapply(mainData18,gsub,
                                   pattern="Follow NBC 6\n", replacement=""))

mainData20 <- as.data.frame(sapply(mainData19,gsub,
                                   pattern="Close.*«»\n", replacement=""))
mainData21 <- as.data.frame(sapply(mainData20,gsub,
                                   pattern="Get.*anytime\n", replacement=""))
mainData22 <- as.data.frame(sapply(mainData21,gsub,
                                   pattern="Follow NBC Bay Area\n", replacement=""))
mainData23 <- as.data.frame(sapply(mainData22,gsub,
                                   pattern="Follow NBC 7 San Diego\n", replacement=""))
mainData24 <- as.data.frame(sapply(mainData23,gsub,
                                   pattern="Follow NBC DFW\n", replacement=""))
mainData25 <- as.data.frame(sapply(mainData24,gsub,
                                   pattern="(Published.*)\n", replacement=""))
mainData26 <- as.data.frame(sapply(mainData25,gsub,
                                   pattern="Published.*2016\n", replacement=""))
mainData27 <- as.data.frame(sapply(mainData26,gsub,
                                   pattern="Published.*2017\n", replacement=""))
dataCleaning <- mainData27

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning New York Times news articles only##############
###################################################################################################################


mainData3 <- as.data.frame(sapply(mainData,gsub,
                                  pattern="Continue reading the main story\n", replacement=""))

mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Advertisement\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="Newsletter Sign Up\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Photo\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="Please verify.*email.\n", replacement=""))



mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Site Mobile Navigation\n", replacement=""))



mainData9 <- as.data.frame(sapply(mainData8,gsub,
                                  pattern="Sign Up for the Sports Newsletter\n", replacement=""))
mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="A version.*Subscribe\n", replacement=""))

mainData11 <- as.data.frame(sapply(mainData10,gsub,
                                   pattern="Images\n", replacement=""))
dataCleaning <- mainData11

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning USA Today news articles only###################
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData,gsub,
                                  pattern="Post to Facebook\n", replacement=""))

mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="CancelSend\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="A link has been sent to your friend's email address.\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Posted!\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="A link has been posted to your Facebook feed.\n", replacement=""))



mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Join the Nation's Conversation\n", replacement=""))



mainData9 <- as.data.frame(sapply(mainData8,gsub,
                                  pattern="Newsletter\n", replacement=""))
mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="Share\n", replacement=""))
mainData11 <- as.data.frame(sapply(mainData10,gsub,
                                   pattern="CLOSE\n", replacement=""))
mainData12 <- as.data.frame(sapply(mainData11,gsub,
                                   pattern="Replay\n", replacement=""))
mainData13 <- as.data.frame(sapply(mainData12,gsub,
                                   pattern="FacebookEmail Twitter Google+ LinkedIn Pinterest\n", replacement=""))
mainData14 <- as.data.frame(sapply(mainData13,gsub,
                                   pattern="To find out more about Facebook commenting please read the Conversation Guidelines and FAQs\n", replacement=""))

mainData15 <- as.data.frame(sapply(mainData14,gsub,
                                   pattern="Share your feedback to help improve our site experience!\n", replacement=""))
mainData16 <- as.data.frame(sapply(mainData15,gsub,
                                   pattern="Most Popular\n", replacement=""))

dataCleaning <- mainData16

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning Washington Post news articles only#############
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData,gsub,
                                  pattern="Be the.*published.\n", replacement=""))

mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="You're all set!\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="View Photos\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Caption\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="Got it\n", replacement=""))



mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Liked that? Try these:\n", replacement=""))



mainData9 <- as.data.frame(sapply(mainData8,gsub,
                                  pattern="Buy Photo\n", replacement=""))
mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="Wait 1 second to continue.\n", replacement=""))
mainData11 <- as.data.frame(sapply(mainData10,gsub,
                                   pattern="Most Read\n", replacement=""))
mainData12 <- as.data.frame(sapply(mainData11,gsub,
                                   pattern="Close video player\n", replacement=""))
mainData13 <- as.data.frame(sapply(mainData12,gsub,
                                   pattern="Most Read:\n", replacement=""))



dataCleaning <- mainData13

###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#################################The following code is for cleaning Yahoo news articles only#######################
###################################################################################################################


#removing string that are irrelevent to our analysis
mainData3 <- as.data.frame(sapply(mainData,gsub,
                                  pattern="Share\n", replacement=""))

mainData4 <- as.data.frame(sapply(mainData3,gsub,
                                  pattern="Reblog\n", replacement=""))

mainData5 <- as.data.frame(sapply(mainData4,gsub,
                                  pattern="More\n", replacement=""))

mainData6 <- as.data.frame(sapply(mainData5,gsub,
                                  pattern="Read More\n", replacement=""))
mainData7 <- as.data.frame(sapply(mainData6,gsub,
                                  pattern="More\n", replacement=""))



mainData8 <- as.data.frame(sapply(mainData7,gsub,
                                  pattern="Tweet\n", replacement=""))



mainData9 <- as.data.frame(sapply(mainData8,gsub,
                                  pattern="View photos\n", replacement=""))
mainData10 <- as.data.frame(sapply(mainData9,gsub,
                                   pattern="More From CNBC\n", replacement=""))

mainData11 <- as.data.frame(sapply(mainData10,gsub,
                                   pattern="Read more from Yahoo News:\n", replacement=""))


dataCleaning <- mainData11


###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#########################################The following code is for cleaning code ##################################
##################################################for all news articles############################################
###################################################################################################################


changeDateAndTime <- function(x){
  x <- as.data.frame(sapply(x,gsub,pattern="\\bJan\\.", replacement="January"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bJan\\b", replacement="January"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bFeb\\.", replacement="February"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bFeb\\b", replacement="February"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bMar\\.", replacement="March"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bMar\\b", replacement="March"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bApr\\.", replacement="April"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bApr\\b", replacement="April"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bMay\\.", replacement="May"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bMay\\b", replacement="May"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bJune\\.", replacement="June"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bJune\\b", replacement="June"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bJuly\\.", replacement="July"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bJuly\\b", replacement="July"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bAug\\.", replacement="August"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bAug\\b", replacement="August"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bSept\\.", replacement="September"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bSept\\b", replacement="September"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bOct\\.", replacement="October"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bOct\\b", replacement="October"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bNov\\.", replacement="November"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bNov\\b", replacement="November"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bDec\\.", replacement="December"))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bDec\\b", replacement="December"))
  x <- as.data.frame(sapply(x,gsub,pattern=" p\\.m\\.", replacement="PM", ignore.case=TRUE))
  x <- as.data.frame(sapply(x,gsub,pattern=" a\\.m\\.", replacement="AM", ignore.case=TRUE))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bp\\.m\\.\\b", replacement="PM", ignore.case=TRUE))
  x <- as.data.frame(sapply(x,gsub,pattern="\\ba\\.m\\.\\b", replacement="AM", ignore.case=TRUE))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bET\\.\\b", replacement="ET ", ignore.case=TRUE))
  x <- as.data.frame(sapply(x,gsub,pattern="\\bE\\.T\\.\\b", replacement="ET ", ignore.case=TRUE))
  
  return(x)
}
dataCleaning <- changeDateAndTime(dataCleaning)


handleCommonAbbriviations <- function(x){
  x <- as.data.frame(sapply(x,gsub,pattern="Dr\\.", replacement="Dr"))
  x <- as.data.frame(sapply(x,gsub,pattern="Mr\\.", replacement="Mr"))
  x <- as.data.frame(sapply(x,gsub,pattern="Miss\\.", replacement="Miss"))
  x <- as.data.frame(sapply(x,gsub,pattern="Mrs\\.", replacement="Mrs"))
  x <- as.data.frame(sapply(x,gsub,pattern="Ms\\.", replacement="Ms"))
  x <- as.data.frame(sapply(x,gsub,pattern="Prof\\.", replacement="Professor"))
  x <- as.data.frame(sapply(x,gsub,pattern="U\\.S\\.", replacement="USA"))
  x <- as.data.frame(sapply(x,gsub,pattern="U\\.S\\.A\\.", replacement="USA"))
  
  return(x)
}

dataCleaning <- handleCommonAbbriviations(dataCleaning)


handleCommonInternetNotation <- function(x){
  #Email
  x$mainText <- sapply(x$mainText,gsub,pattern="([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", replacement=" subEmailaddress")
  
  #url
  x$mainText <- sapply(x$mainText,gsub,pattern="\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", replacement=" subURLaddress")
  
  # Hashtags
  x$mainText <- sapply(x$mainText,gsub,pattern="#(\\d|\\w)+", replacement=" subHashtag")
  
  #Twitter Handler
  x$mainText <- sapply(x$mainText,gsub,pattern="@\\w+", replacement=" subTwittername")
  
  return(x$mainText)

}

dataCleaning$mainText <- handleCommonInternetNotation(dataCleaning)
#abcnewstravelstories@gmail.com
handleOtherCommonProblem <- function(x){
  x$mainText <- sapply(x$mainText,gsub,pattern=" w/ ", replacement=" with ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" b/ ", replacement=" between ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" & ", replacement=" and ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" 'cause ", replacement=" because ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" and/or ", replacement=" and-or ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" 'an ", replacement=" and ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" 'n ", replacement=" and ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" mos ", replacement=" months ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" sec ", replacement=" second ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" @ ", replacement=" at ")
  x$mainText <- sapply(x$mainText,gsub,pattern=" w/a ", replacement=" with a ")

  return(x$mainText)
  
}
dataCleaning$mainText <- handleOtherCommonProblem(dataCleaning)


#Change file name based on news organizations
file_name <- paste("ABCCleanDataFinalUpdatedLatest" , "csv", sep=".")
write.csv(testData , file = file_name)

