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
    
    colnames(stormDat) <-                     ## Rename the column headings to
      colnames(stormDat) %>%                    ## lower case using chaining to
      tolower() %>%                             ## modify the strings
      chainGsub("_locati", "_location") %>%
      chainGsub("latitude_e", "latitude_end") %>%
      chainGsub("longitude_", "longitude_end") %>%
      chainGsub("_azi", "_heading") %>%
      chainGsub("bgn", "begin") %>%
      chainGsub("evtype", "event_type") %>%
      chainGsub("endn", "endname") %>%
      chainGsub("state__", "state_code") %>%
      chainGsub("stateoffic", "stateoffice") %>%
      chainGsub("dmg", "damage") %>%
      chainGsub("propd", "propertyd") %>%
      chainGsub("wfo", "weatherforecastoffice")
    colnames(stormDat)[21] <- "fujita_scale"
    
    stormDat$begin_date <-                      ## Change to character and remove 
      gsub(" 0:00:00", "",                    ## superfluous time stamps
           as.character(stormDat$begin_date))
    stormDat$end_date <-                      ## Change to character and remove 
      gsub(" 0:00:00", "",                    ## superfluous time stamps
           as.character(stormDat$end_date))
    stormDat$begin_time <-                      ## Add colon and change the time 
      insertMark(as.character(                ## stamps to text data type.
        stormDat$begin_time))
    stormDat$end_time <- 
      insertMark(as.character(
        stormDat$end_time))
    stormDat$time_zone <-                     ## Change time zone abbreviation to
      toupper(                                ## text format.
        as.character(stormDat$time_zone))
    ## Data cleaning data -- Correction of State codes
    stormDat[stormDat$state == "XX",          ## New York coastal events
             7] <- "NY"
    stormDat[stormDat$state == "SL",          ## New York St. Lawrence Seaway
             7] <- "NY"                       ## events   
    stormDat[stormDat$state == "PZ",          ## Indicates pacific coast. All refs
             7] <- "CA"                       ## are to CA
    stormDat[stormDat$state == "ST",          
             7] <- "OH"
    ## State code "AN" may represent "Atlantic Narrows," and should be treated 
    ## specially in state counts: data with  
    ## this label represent multiple states, including "DE", "NJ", "NY", "VA", 
    ## "MD", "NC".  State code "LE" is used for Lake Erie multitstate events, 
    ## including "OH", "NY", "MI", "PA". State code "GM" used for mulitstate 
    ## events around the Gulf of Mexico
    
    stormDat[stormDat$state=="PK",            ## Pacific + Alaskan coastal areas  
             7] <- "AK"                       ## 
    stormDat[stormDat$state=="AK" &           ## Alaska Daylight savings time code
               stormDat$time_zone=="ADT",       ## to modern standard code.
             4]<-"AKDT"                       ## Correction of Alaskan Standard  
    stormDat[stormDat$state=="AK" &           ## time codes to modern standard
               stormDat$time_zone %in%          ## code.
               c("AST","AKS"), 4] <- "AKST"  
    
    return(stormDat)
  }  
}

loadStormDat()