library(dplyr, warn.conflicts=FALSE)
library(chron)

targFileName <- "data/repdata\ data\ StormData.csv.bz2"

downLoadData <- function(){
  if(length(list.dirs("data"))==0) {
    dir.create("data")
  }
  dataURL <- 
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  if(!file.exists(targFileName)) {
    download.file(dataURL, destfile = targFileName, method = "curl")
  }
} 

loadStormDat <- function(){
  downLoadData()                            ## Call to download data.
  if(!"stormDat" %in% ls()){                ## If not already loaded, read-in
    stormDat <- read.csv(targFileName)      ## csv data file directly from bzip
    ## archive.
    return(stormDat)
  }  
}

cleanStormDat <- function(stormDat){        ## Function makes data "tidier."
  require(dplyr)
  chainGsub <-function(dat, pat, repl) {    ## Function to chain string replace-
    return(gsub(pat, repl, dat))            ## ments for colnames
  }
  
  insertMark <- function(hourMinStr){       ## Function to change time stamp 
    return(paste0(strtrim(                  ## data to text, and insert ":".
      hourMinStr,2), 
      ":", 
      substr(
        hourMinStr, 3, 4)))
  }
  stormDat <- stormDat[,1:36]               ## Discard record number column
  colnames(stormDat) <-                     ## Rename the column headings to  
    colnames(stormDat) %>%                  ## lower case using chaining to
  tolower() %>%                             ## modify the strings to "tidier"
  chainGsub("_locati", "location") %>%      ## form.
  chainGsub("latitude_e", 
            "latitudeatend") %>%  
  chainGsub("longitude_", "longitudeatend") %>%
  chainGsub("_azi", "heading") %>%
  chainGsub("bgn", "begin") %>%
  chainGsub("evtype", "eventtype") %>%
  chainGsub("endn", "endname") %>%
  chainGsub("state__", "statenumcode") %>%
  chainGsub("stateoffic", "stateweatheroffice") %>%
  chainGsub("mag", "magnitude")  %>%
  chainGsub("dmg", "damage") %>%
  chainGsub("propd", "propertyd") %>%
  chainGsub("wfo", "weatherforecastoffice") %>%
  chainGsub("_","")

  colnames(stormDat)[21] <- "fujitascale"   ## Name this column without sub-
                                            ## stitution.
  stormDat$begin_date <-                    ## Change begin and end date types 
    gsub(" 0:00:00", "",                    ## to plain character and remove 
         as.character(stormDat$begin_date)) ## superfluous time stamps from
  stormDat$end_date <-                      ## date fields. 
    gsub(" 0:00:00", "",
         as.character(stormDat$end_date))
  stormDat$begin_time <-                    ## Add colon and change the event
    insertMark(as.character(                ## beginning and ending time stamps
      stormDat$begin_time))                 ## to text data type.
  stormDat$end_time <- 
    insertMark(as.character(
      stormDat$end_time))

  stormDat[stormDat$state == "XX",          ## Correction of State codes
           7] <- "NY"                       ## New York coastal events
  stormDat[stormDat$state == "SL",          ## New York St. Lawrence Seaway
           7] <- "NY"                       ## events   
  stormDat[stormDat$state == "PZ",          ## Indicates pacific coast. All refs
           7] <- "CA"                       ## are to CA
  stormDat[stormDat$state == "ST",          
           7] <- "OH"
  stormDat[stormDat$state=="PK",            ## Pacific + Alaskan coastal areas  
           7] <- "AK" 
  stormDat$time_zone <-                     ## Change time zone abbreviation to
    toupper(                                ## upper case plain text format.
      as.character(stormDat$time_zone))
  stormDat[stormDat$state=="AK" &           ## Alaska Daylight savings time code
             stormDat$time_zone=="ADT",     ## to modern standard code.
           4]<-"AKDT"                       ## Correction of Alaskan Standard  
  stormDat[stormDat$state=="AK" &           ## time codes to modern standard
             stormDat$time_zone %in%        ## code.
             c("AST","AKS"), 4] <- "AKST"    
  
  return(stormDat)
}

##stormDat <- cleanStormDat(loadStormDat())

#1.  _\*Record collection began before Alaska and Hawaii statehood, and time 
#zone standard codes have changed multiple times since the beginning of record 
#keeping.  Several typographical errors are noted in the original data set time 
#zone and state abbreviations._  

#2.  _\*\*State code "AN" represents "Atlantic North," and should be treated 
#specially in state counts: data with this label represent multiple states, 
#including at least "DE", "NJ", "NY", "VA", "MD", "NC". 
#State code "LE" is used for Lake Erie multitstate events, 
#including "OH", "NY", "MI", "PA". State code "GM" is used for mulitstate 
#events around the Gulf of Mexico_  
