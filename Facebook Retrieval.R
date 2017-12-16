install.packages("devtools")
require(stats)
library(devtools)

install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")

require("Rfacebook")

#Here you will add your app id and secret id. Currently, the ids used are registered under my facebook developer account.
appId <- ""
appSecret <- ""

#Facebook Authentication process
fb_oauth <- fbOAuth(app_id = appId, app_secret = appSecret,extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

#Here, you will add 1 news organization facebook account ID
organizations <- c("washingtonpost")

#datelist <- seq(as.Date("2016/3/1"), as.Date("2017/5/28"), by = "week")
#datelist <- seq(as.Date("2017/4/19"), as.Date("2017/6/8"), by = "day")

#for saving retrived data periodically, as a precaution
llll <- 1
kkkk <- 1


#list of each day between the dates below
datelist <- seq(as.Date("2016/3/1"), as.Date("2017/6/1"), by = "day")
#please note that in cases where the system is interrupted, and tryCatch fails to catch it, you should look at the last 
#date save in the csv files generated. Add the last date to the first parameter of the above function, so that you can 
#resume retrieving facebook posts from that date.

fb_data <- NA


count <- 0
for(i in 1:(length(datelist) - 1)){
  count <- count +1
  #you can change the limit in the if condition between 60 to 75, as the getPage function will call the facebook
  #retrival 2 to 4 times for each day.
  if(count >= 75){
    temp <- paste(organizations[1], llll, sep = "")
    llll <- llll + 1
    file_name <- paste(temp , "csv", sep=".")
    write.csv(fb_data , file = file_name)
    fb_data <- 100
    
    count <- 0
    #pauses the process for an hour
    Sys.sleep(3600)
    
  }
  fb_page <- NA
  #too many error were generated because of the API, so in order to handle it, 
  #and automate the whole process, trycatch was used to handle these situations.
  while(length(fb_page) == 1){
    fb_page <- tryCatch({
      getPage(page=organizations[1], n = 2000, since=datelist[i] , until=datelist[(i+1)], token=fb_oauth, reactions = TRUE)
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      return(NA)
      
    }, error = function(err) {
      print(paste("MY_ERROR:  ",err))
      return(NA)
      
    }, finally = {
      
    })
    if(length(fb_page) == 1){
      Sys.sleep(10)
    }
  }
  
  if(length(fb_data) == 1){
    fb_data <- fb_page
  } else {
    fb_data <- rbind(fb_data, fb_page)
  }
  
}



#There are 2 reasons for this, the first is to save data retrieved 
#before an error stops the whole process. This rarely 
#happens. The second reason is to save the last strands 
#of data retrived from Facebook from the last few days in datelist.
asasasa <- paste(organizations[1], "00", kkkk, sep = "")
kkkk <- kkkk + 1
file_name <- paste(asasasa , "csv", sep=".")
write.csv(fb_data , file = file_name)

