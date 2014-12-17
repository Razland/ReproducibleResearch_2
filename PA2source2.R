library(dplyr, warn.conflicts=FALSE)
library(knitr)
library(stringr)
library(ggplot2)
library(grid)
source("multiplot.R")
targFileName <-                             ## Target-download compressed file
  "data/repdata\ data\ StormData.csv.bz2"
if(length(list.dirs("./figures"))==0){      ## Create a target directory for any 
  dir.create("./figures")                   ## plot files that may be produced.
  }
downLoadData <- function(){                 ## Function downloads data if needed
  if(length(list.dirs("./data"))==0){       ## Create data directory if it does
    dir.create("./data")                    ## not exist.
    }
  dataURL <- paste0(                        ## Name of data file
    "https://d396qusza40orc.cloudfront.",
    "net/repdata%2Fdata%2FStormData.csv.bz2")
  if(!file.exists(targFileName)){           ## If not present, download file
    download.file(dataURL,                  ## from web and decompress 
                  destfile = targFileName, 
                  method = "curl")
    }
  }

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
#multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#  require(grid, quietly=TRUE)
#  
#  # Make a list from the ... arguments and plotlist
#  plots <- c(list(...), plotlist)
#  
#  numPlots = length(plots)
#  
#  # If layout is NULL, then use 'cols' to determine layout
#  if (is.null(layout)) {
#    # Make the panel
#    # ncol: Number of columns of plots
#    # nrow: Number of rows needed, calculated from # of cols
#    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                     ncol = cols, nrow = ceiling(numPlots/cols))
#  }
#  
#  if (numPlots==1) {
#    print(plots[[1]])
#    
#  } else {
#    # Set up the page
#    grid.newpage()
#    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#    
#    # Make each plot, in the correct location
#    for (i in 1:numPlots) {
#      # Get the i,j matrix positions of the regions that contain this subplot
#      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#      
#      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                      layout.pos.col = matchidx$col))
#    }
#  }
#}
## Credit: this helper function was obtained from the R Cookbook example at 
## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
## Since the assignment calls for showing all work, this function is included
## in-line, with attribution/credit shown for the source, rather than "sourced"  
## from a separate R file.

loadStormDat <- function(){                 ## Function reads csv file.
  downLoadData()                            ## Call to download data function.
  if(!"stormDat" %in% ls()){                ## If not already loaded, read-in
    stormDat <- read.csv(targFileName)      ## csv data file directly from bzip
  return(stormDat)                          ## archive.
  }
}
stormDat <- loadStormDat()
dim(stormDat)                               ## Show dimensions of the data

colnames(stormDat)                          ## Show the variable names of data
head(stormDat, 3)                           ## First 3 rows of data 
range(stormDat[,37])                        ## Max, min of range variable
chainGsub <-function(dat, pat, repl) {      ## Subfunction to chain string
  return(gsub(pat, repl, dat))              ## replacements for colnames
}

insertMark <- function(hourMinStr){         ## Subfunction to change time stamp 
  return(paste0(strtrim(                    ## data to text, and insert ":". 
    hourMinStr,2), ":", 
    substr(hourMinStr, 3, 4)))
}

cleanStormDat <- function(stormDat){        ## Function makes data "tidier."
  stormDat <- stormDat[,1:36]               ## Discard record number column
  colnames(stormDat) <-                     ## Rename the column headings to  
    colnames(stormDat) %>%                  ## lower case using chaining to
  tolower() %>%                             ## modify the strings to "tidier"
  chainGsub("_locati", "location") %>%      ## form.
  chainGsub("tude_e", "tudeatend") %>%  
  chainGsub("tude_", "tudeatend") %>%
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
  colnames(stormDat)[21] <- "fujitascale"   ## Name this column name directly.
  stormDat$begindate <-                     ## Change begin and end date types 
    gsub(" 0:00:00", "",                    ## to plain character and remove 
       as.character(stormDat$begindate))    ## superfluous time stamps from
  stormDat$enddate <-                       ## date fields. 
    gsub(" 0:00:00", "", as.character(stormDat$enddate))
  stormDat$begindate <-                     ## Change date fields to date type
    as.Date(stormDat$begindate, format="%m/%d/%Y")
  stormDat$enddate <- as.Date(stormDat$enddate, format="%m/%d/%Y")
  stormDat$begintime <- as.character(stormDat$begintime)
  stormDat$endtime <- as.character(stormDat$endtime)
  stormDat[str_length(                      ## Add colon to event time stamps
             stormDat$begintime)==4,3] <-   ## with entry as a 4-digit integer. 
    insertMark(stormDat[str_length(stormDat$begintime)==4,3])
  stormDat[str_length(stormDat$endtime)==4, 13] <- 
    insertMark(stormDat[str_length(stormDat$endtime)==4, 13])
                                            ## note1: *Correcting State codes
  stormDat[stormDat$state=="LO",7] <- "NY"  ## New York Lake Ontario events
  stormDat[stormDat$state=="PM",7] <- "GU"  ## Guam coastal area events 
  stormDat[stormDat$state=="PK",7] <- "AK"  ## Pacific + Alaskan coastal areas
  stormDat[stormDat$state=="PZ",7] <- "CA"  ## Indicates pacific coast. All 
                                            ## references are to coastal CA
  stormDat[stormDat$state=="SL",7] <- "NY"  ## New York St. Lawrence Seaway
                                            ## events   
  stormDat[stormDat$state=="ST",7] <- "OH"  ## Correction for Ohio typo
  stormDat[stormDat$state=="XX",7] <- "NY"  ## New York coastal events
  stormDat$state <-                         ## Recast state abbreviation back to 
    as.factor(stormDat$state)               ## factor.
  stormDat$timezone <-                      ## note2: **Change time zone 
    toupper(as.character(stormDat$timezone))## abbreviation to upper case, and 
                                            ## temporarily to plain text format.
  stormDat[stormDat$state=="AK" &           ## Correction of Alaska Daylight
           stormDat$timezone=="ADT", 4] <-  ## Savings time code to modern
             "AKDT"                         ## standard code.   
  stormDat[stormDat$state=="AK" &           ## Correction of Alaskan Standard 
           stormDat$timezone %in%           ## time codes to modern standard
           c("AST","AKS"), 4] <- "AKST"     ## code.
  stormDat$timezone <-                      ## Recast time zone as a factor. 
    as.factor(stormDat$timezone)
  for(colNum in 32:35){                     ## Fix lat/long in four columns from
    stormDat[,colNum] <-                    ## integer to real number with two 
      stormDat[,colNum] * .01               ## decimal places
    }
  stormDat[stormDat$state== "AS" &          ## Fix American Samoa Latitude 
           is.na(stormDat$latitude),32] <-  ## values that are NA to appropriate
             as.numeric(-14.3)              ## lat for approx middle of island
  stormDat[stormDat$state == "AS" &
           is.na(stormDat$latitudeatend), 34] <- -14.3
  stormDat[stormDat$state == "AS", 33] <-   ## Fix American Samoa Longitude to
    (stormDat[stormDat$state == "AS",       ## negative numbers. 
              33] * -1)
  stormDat[stormDat$state == "AS", 35] <- 
    (stormDat[stormDat$state == "AS", 35] * -1)
  stormDat[stormDat$state == "GU" &         ## Fix Guam longitude reported with
           stormDat$longitude > 0, 33] <-   ## wrong sign.  
    (stormDat[stormDat$state == "GU" & stormDat$longitude > 0, 33] * -1)
  stormDat[stormDat$state == "GU" & stormDat$longitudeatend > 0, 35] <-     
    (stormDat[stormDat$state == "GU" & stormDat$longitudeatend > 0, 35] * -1)
  stormDat[524416, 7] <- "MH"               ## Fix single Marshall Islands event 
  stormDat$remarks <- as.character(         ## reported by Guam regional weather
    stormDat$remarks)                       ## station
  return(stormDat)                          ## Change freeform text from factor 
}                                           ## to plain text.

stormDat <- cleanStormDat(stormDat)         ## Call to clean data function
format(stormDat[                            ## Random sample of 10 observations
                sample(1:length(stormDat$eventtype),10), 6:8], 
       justify="right")

convertEvent <- function(dataVect){        # Function parses input string (in-    
  as.character(dataVect) %>%               # tended for stormDat$eventtype) and 
  toupper() %>%                            # makes specific changes to partly
  chainGsub("&", " AND ") %>%              # normalize the values to the NWS
  chainGsub("/", " ") %>%                  # standard strings.
  chainGsub("-", " ") %>%
  chainGsub("\\?", "NONE") %>%
  chainGsub("\\; ", " ") %>%
  chainGsub("\\;$", "") %>%
  chainGsub("\\\\", " ") %>%
  chainGsub(" $", "") %>%
  chainGsub("^  ", "") %>%
  chainGsub("^ ", "") %>%
  chainGsub("ABNORMAL WARMTH", "EXTREME HEAT") %>%
  chainGsub("ABNORMALLY", "UNSEASONABLY") %>%
  chainGsub(" AND$", "") %>%
  chainGsub("APACHE COUNTY", "THUNDERSTORM WIND") %>%
  chainGsub("ASHFALL", "VOLCANIC ASH") %>%
  chainGsub("AVALANCE", "AVALANCHE") %>%
  chainGsub("BITTER WIND CHILL TEMPERATURES", "EXTREME COLD WIND CHILL") %>%
  chainGsub("BLIZZARD HEAVY", "BLIZZARD AND HEAVY") %>%
  chainGsub("BLIZZARD SUMMARY", "BLIZZARD") %>%
  chainGsub("BLIZZARD WEATHER", "BLIZZARD") %>%
  chainGsub("CHILL TEMPERATURE$", "CHILL") %>%
  chainGsub("CHILLS", "CHILL") %>%
  chainGsub("CHIL$", "CHILL") %>%
  chainGsub("CHI$", "CHILL") %>%
  chainGsub("CH$", "CHILL") %>%
  chainGsub("^COL$", "COLD") %>%
  chainGsub("COASTAL FLOODING", "COASTAL FLOOD") %>%
  chainGsub("COASTALF", "COASTAL F") %>%
  chainGsub("COASTALS", "COASTAL S") %>%
  chainGsub(" CLOUDS$", " CLOUD" ) %>%
  chainGsub(" CLOU$", " CLOUD" ) %>%
  chainGsub("CSTL ", "COASTAL ") %>%
  chainGsub("DAMAGE TO", "DAMAGE") %>%
  chainGsub("DEVEL", "DEVIL") %>%
  chainGsub("DUST DEVIL", "TORNADO" ) %>%
  chainGsub("DUSTSTORM", "DUST STORM") %>%
  chainGsub("DRIZZLE AND FREEZING", "DRIZZLE") %>%
  chainGsub("DRYNESS", "DROUGHT") %>%   ###
  chainGsub("DRY CONDITIONS", "DROUGHT") %>%
  chainGsub("DRY PATTERN", "DROUGHT") %>%
  chainGsub("DRY SPELL", "DROUGHT") %>%
  chainGsub("DRY WEATHER", "DROUGHT") %>%
  chainGsub("^DRY$", "DROUGHT") %>% ##
  chainGsub(" DR$", " DROUGHT") %>%
  chainGsub("DUS$", "DUST") %>%
  chainGsub("EROSIN", "EROSION") %>%
  chainGsub(" EROSIO$", " EROSION") %>%
  chainGsub("EROSION COASTAL FLOOD", "COASTAL FLOOD EROSION") %>%
  chainGsub("EXCESSIVE HEAT DROUGHT", "DROUGHT EXCESSIVE HEAT") %>%
  chainGsub("EXCESSIVELY DRY", "DRY SPELL") %>%
  chainGsub("EXCESSIVE", "EXTREME") %>%
  chainGsub("EXTREME RECORD", "EXTREME") %>%
  chainGsub("EXTREME WETNESS", "EXTREME PRECIPITATION") %>%
  chainGsub("SEVERE COLD", "EXTREME COLD") %>%  
  chainGsub("FIR$", "FIRE") %>%
  chainGsub("FIRES", "FIRE") %>% 
  chainGsub("FREEZ$", "FREEZE") %>%
  chainGsub("FREEZING RA$", "FREEZING RAIN") %>%
  chainGsub("FROS$", "FROST") %>%
  chainGsub("FROSTFREEZE", "FROST FREEZE") %>%
  chainGsub("FLASHFLOOD", "FLASH FLOOD") %>%
  chainGsub("FLD$", "FLOOD") %>%
  chainGsub("FLOODING FLOOD$", "FLOOD") %>%
  chainGsub(" FLOOODING$", " FLOOD") %>%
  chainGsub("FLOOD FLOODING", "FLOOD") %>%
  chainGsub("FLOOD FLASH", "FLASH FLOOD") %>%
  chainGsub("FLOOD RIVER", "FLOOD") %>%
  chainGsub("FLOODIN", "FLOOD") %>%
  chainGsub("FLOODINGG", "FLOOD") %>%
  chainGsub("FLDG", "FLOOD") %>%
  chainGsub("FLOODG", "FLOOD") %>%
  chainGsub("FLOODS", "FLOOD") %>%
  chainGsub("FLOODING EROSION", "FLOOD EROSION") %>%
  chainGsub("FLOOD FLOOD", "FLOOD") %>%
  chainGsub("FUNNE$", "FUNNEL") %>%
  chainGsub("FUNNELS$", "FUNNEL") %>%
  chainGsub("SNOW ICESTORM", "SNOW AND ICE STORM") %>%
  chainGsub("SNOW ICE$", "SNOW AND ICE STORM") %>%
  chainGsub("SNOW AND ICE$", "SNOW AND ICE STORM") %>%
  chainGsub("ICE ON ROAD", "ICY ROADS") %>%
  chainGsub("ICE ROADS", "ICY ROADS") %>%
  chainGsub("GUSTNADO", "THUNDERSTORM WIND") %>%
  chainGsub("HAIL[0-9]", "HAIL") %>%
  chainGsub("HAIL [0-9]{1,5}$", "HAIL") %>%
  chainGsub("HAIL STORM", "HAIL") %>%
  chainGsub("SMALL HAIL", "HAIL") %>%
  chainGsub("HEAT WAVES{0,1}", "EXCESSIVE HEAT") %>%
  chainGsub("HEATBURST", "EXCESSIVE HEAT") %>%
  chainGsub("HURRICANE$", "HURRICANE TYPHOON") %>%
  chainGsub("HURRICANE [EFGO][A-Z]{1,25}", "HURRICANE TYPHOON") %>%
  chainGsub("HURRICANE [A-Z ]{1,25}WIND", "HURRICANE TYPHOON") %>%
  chainGsub("HURRICANE [A-Z ]{1,25}SWELLS", "STORM SURGE") %>%
  chainGsub("HVY", "HEAVY") %>%
  chainGsub("LIGHTING", "LIGHTNING") %>%
  chainGsub("LIGNTING", "LIGHTNING") %>%
  chainGsub("LIGNTNING", "LIGHTNING") %>%
  chainGsub("LIGHTNINGNONE", "LIGHTNING") %>%
  chainGsub("LOW TEMPERATURE RECORD", "EXTREME COLD") %>%
  chainGsub("RECORD LOW$", "EXTREME COLD") %>%
  chainGsub("RECORD COLD$", "EXTREME COLD") %>%
  chainGsub("LOW WIND CHILL", "WIND CHILL") %>%
  chainGsub(", MAY 26$", "") %>%
  chainGsub("MICO", "MICRO") %>%
  chainGsub("MIRCO", "MICRO") %>%
  chainGsub("MUDSLIDES", "MUD SLIDE") %>%
  chainGsub("MUDSLIDE", "MUD SLIDE") %>%
  chainGsub("MUD SLIDE LANDSLIDE", "MUD SLIDE") %>%
  chainGsub("NO SEVERE WEATHER", "NONE") %>%
  chainGsub("NON SEVERE ", "") %>%
  chainGsub("NON THUNDERSTORM WIND", "STRONG WIND") %>%
  chainGsub("^OTHER$", "NONE") %>%
  chainGsub(" PLUME$", "") %>%
  chainGsub(" PRECIPATATION$", " PRECIPITATION") %>%
  chainGsub(" PRECIPITATIO$", " PRECIPITATION") %>%
  chainGsub(" PRECIP$", " PRECIPITATION") %>%
  chainGsub("^PROLONGED ", "EXTENDED ") %>%
  chainGsub("^PROLONG ", "EXTENDED ") %>%
  chainGsub("RSPOUT", "R SPOUT") %>% 
  chainGsub("RAIN AND SNOW", "RAIN SNOW") %>%
  chainGsub("RAIN SLEET AND LIGHT$", "RAIN AND SLEET") %>%
  chainGsub("RAIN SLEET$", "RAIN AND SLEET") %>%
  chainGsub(" RAINFALL$", " RAIN") %>%
  chainGsub("RAINSTORM$", "RAIN STORM") %>%
  chainGsub("RAIN HEAVY", "HEAVY RAIN") %>%
  chainGsub("RECORD EXTREME", "EXTREME") %>%
  chainGsub("RECORD HIGH TEMPERATURE", "EXCESSIVE HEAT") %>%
  chainGsub("RECORD HIGH$", "EXCESSIVE HEAT") %>%
  chainGsub("RECORD HEAT$", "EXCESSIVE HEAT") %>%
  chainGsub("RECORD HEAT WAVE", "EXCESSIVE HEAT") %>%
  chainGsub("RECORD WARM$", "EXCESSIVE HEAT") %>%
  chainGsub("RECORD WARM TEMPS$", "EXCESSIVE HEAT") %>%  
  chainGsub("RECORD PRECIPITATION", "EXTREME PRECIPITATION") %>%
  chainGsub("RIP CURRENTS", "RIP CURRENT") %>%
  chainGsub("RIVER FLOOD$", "FLOOD") %>%
  chainGsub("SLEET FREEZING RAIN", "WINTER STORM") %>%
  chainGsub("SLEET RAIN SNOW", "WINTER STORM") %>%
  chainGsub("SLEET SNOW", "WINTER STORM") %>%
  chainGsub("SLEET STORM", "SLEET") %>%
  chainGsub("SLEET AND FREEZING RAIN", "WINTER STORM") %>%
  chainGsub("SLIDES", "SLIDE") %>%
  chainGsub("SML", "SMALL") %>%
  chainGsub("SMALL STREAM AND URBAN", "") %>% 
  chainGsub("SMALL STREAM URBAN", "") %>% 
  chainGsub(" SMALL$", " FLOOD") %>%
  chainGsub(" SMALL STREAM FLOOD$", "FLOOD") %>%
  chainGsub(" SNOWFALL$", " SNOW") %>%
  chainGsub("SNOW AND EXTREME", "SNOW EXTREME") %>%
  chainGsub("SNOW ANDBLOWING", "SNOW AND BLOWING") %>%
  chainGsub("SNOW BLOWING", "SNOW AND BLOWING") %>%
  chainGsub("SNOW HEAVY SNOW", "HEAVY SNOW") %>%
  chainGsub("SNOW SLEET RAIN", "WINTER STORM") %>%
  chainGsub("SQUALL$", "SQUALLS") %>%
  chainGsub("NEAR RECORD SNOW$", "HEAVY SNOW") %>%
  chainGsub(" SNO$", " SNOW") %>%
  chainGsub(" SNOWS", " SNOW") %>%
  chainGsub("^SNOW ICE$", "HEAVY SNOW ICE STORM") %>%
  chainGsub("SPOUTS", "SPOUT") %>% 
  chainGsub("SPOUT ", "SPOUT") %>% 
  chainGsub("SPOUTT", "SPOUT T") %>% 
  chainGsub("SPOUTF", "SPOUT F") %>% 
  chainGsub("STRM", "STREAM") %>%
  chainGsub("STREA$", "STREAM") %>%
  chainGsub("STREAM FLOOD$", "FLOOD") %>%
  chainGsub("STREET FLOOD$", "FLOOD") %>%
  chainGsub("STREA$", "STREAM") %>%
  chainGsub("STROM", "STORM" ) %>%
  chainGsub("STORMS$", "STORM") %>%  
  chainGsub("STORMS WIND", "STORM WIND") %>%
  chainGsub("STORMIND", "STORM WIND") %>%
  chainGsub("STORMSS$", "STORMS") %>%
  chainGsub("STORMSS", "STORMS") %>%
  chainGsub("STORMWIND", "STORM WIND") %>%
  chainGsub("STORMW$", "STORM") %>%
  chainGsub("STORMW ", "STORM ") %>%
  chainGsub("STORMS W", "STORM W") %>%
  chainGsub("STORMSW", "STORM") %>%
  chainGsub("SURG$", "SURGE") %>%
  chainGsub("TIDES$", "TIDE") %>%  
  chainGsub("TEMPERATURES$", "TEMPERATURE") %>%
  chainGsub("LOW TEMPERATURE", "COLD") %>%
  chainGsub("HIGH TEMPERATUE", "HEAT") %>%
  chainGsub("TREE$", "TREES") %>%
  chainGsub("TROPICAL STORMS ", "TROPICAL STORM ") %>%
  chainGsub("TORNADOS", "TORNADOES") %>%
  chainGsub("TORNDAO", "TORNADO") %>%
  chainGsub("TSTM", "THUNDERSTORM") %>%
  chainGsub("TSORM", "STORM" ) %>%
  chainGsub("TORRENTIAL RAINFALL", "HEAVY RAIN") %>%
  chainGsub("THUNDERSTORMS DAMAGE TO", "THUNDERSTORM WIND") %>%
  chainGsub("THUNDERSTORMW", "THUNDERSTORM") %>%
  chainGsub("TUNDERSTORM", "THUNDERSTORM") %>%
  chainGsub("THUNDERTORM", "THUNDERSTORM") %>%
  chainGsub("THUNDEERSTORM", "THUNDERSTORM") %>%
  chainGsub("THUDERSTORM", "THUNDERSTORM") %>%
  chainGsub("THUNDERESTORM", "THUNDERSTORM") %>%
  chainGsub("THUNERSTORM", "THUNDERSTORM") %>%
  chainGsub("THUNDESTORM", "THUNDERSTORM") %>%
  chainGsub("UNSEASONABLE", "UNSEASONABLY") %>%
  chainGsub("UNSEASONAL LOW TEMP", "COLD") %>%
  chainGsub("UNUSUAL RECORD WARMTH", "UNSEASONABLY WARM") %>%
  chainGsub("UNUSUAL WARMTH", "HEAT") %>%
  chainGsub("UNUSUALLY WARM", "HEAT") %>%
  chainGsub("UNUSUALLY COLD", "COLD") %>%
  chainGsub("URBAN FLOODING", "FLOOD") %>% 
  chainGsub("URBAN AND SMALL", "URBAN SMALL") %>% 
  chainGsub("URBAN SMALL FLOOD$", "FLOOD") %>%
  chainGsub("^URBAN AND$", "") %>% 
  chainGsub("URBAN SMALL STREAM$", "FLOOD") %>%
  chainGsub("URBAN FLOOD", "FLOOD") %>% 
  chainGsub("VERY DRY", "DROUGHT" ) %>%
  chainGsub("VERY WARM", "HEAT" ) %>%
  chainGsub("VOG", "VOLCANIC ASH" ) %>%
  chainGsub("VOLCANIC ERUPTION", "VOLCANIC ASH" ) %>%
  chainGsub("VOLCANIC VOLCANIC", "VOLCANIC" ) %>%
  chainGsub("WARMT$", "WARM") %>%
  chainGsub("WARM WET", "WARM AND WET") %>%
  chainGsub("WARM YEAR", "WARM") %>%
  chainGsub("WARM TEMPS$", "WARM") %>%
  chainGsub("WARMTH", "WARM") %>%
  chainGsub("WATCHILL", "WATCH") %>%
  chainGsub("WAV$", "WAVE") %>%
  chainGsub("WAYTER", "WATER") %>% 
  chainGsub("^ATER", "WATER") %>% 
  chainGsub(" WAUSEON$", "") %>% 
  chainGsub("WEATHE$", "WEATHER") %>%
  chainGsub("WHIRLWIND", "TORNADO" ) %>%
  chainGsub("WILD FIRE", "WILDFIRE" ) %>%
  chainGsub("WILD FOREST FIRE", "WILDFIRE" ) %>%  
  chainGsub("WINDCHILL", "WIND CHILL") %>%
  chainGsub("WINDTER", "WINTER") %>%
  chainGsub("WI$", "WIND") %>%
  chainGsub("WINDS", "WIND") %>%
  chainGsub("WIN$", "WIND") %>%
  chainGsub("WINDHAIL", "WIND HAIL") %>%
  chainGsub("WINS$", "WIND") %>%
  chainGsub("WND", "WIND") %>% 
  chainGsub("W INDS", "WIND") %>%
  chainGsub("?MPH", "") %>%
  chainGsub("[0-9]{1,5}","") %>%  
  chainGsub("WINTRY", "WINTERY") %>%
  chainGsub("WIND CHILL TEMPERATURE", "WIND CHILL") %>%
  chainGsub(" WIND WIND ", " WIND AND WIND ") %>%
  chainGsub("WINTER WEATHER MIX", "WINTER STORM") %>%
  chainGsub("WINTER MIX", "WINTER STORM") %>%
  chainGsub("WINTERY MIX", "WINTER STORM") %>%  
  chainGsub("WX", "WEATHER") %>%
  chainGsub("WINTER WEATHER$", "WINTER STORM") %>% 
  chainGsub("\\.", "") %>%
  chainGsub("   ", " ") %>% 
  chainGsub("  ", " ") %>%
  chainGsub("\\)", "") %>%
  chainGsub("\\(", "") %>%
  chainGsub(" $", "") %>%
  chainGsub("^$", "NONE") %>%
  chainGsub("FLOOD FLOOD$", "FLOOD") %>%
  chainGsub("FLOOD STREET", "FLOOD") %>%
  chainGsub("URBAN","") %>%
  chainGsub("COLD WAVE", "COLD") %>%
  chainGsub("COLD WEATHER", "COLD") %>%
  chainGsub("UNSEASONABLY COLD", "COLD") %>%
  chainGsub("HIGH WAVES", "HEAVY SURF") %>%
  chainGsub("HIGH SURF", "HEAVY SURF") %>%
  chainGsub("HYPERTHERMIA EXPOSURE", "HYPOTHERMIA") %>%
  chainGsub(" [A-Z]$", "") %>%
  chainGsub("EXTREME RAIN", "HEAVY RAIN") %>%
  chainGsub(":","") %>%
  chainGsub("\\,","") %>%
  chainGsub("SUMMARY [ A-Z]{1,20}$", "SEE REMARKS") %>%
  chainGsub("^ {0,3}", "") %>%
  chainGsub("AND FLOOD", "FLOOD") %>%
  chainGsub("ANDFLOOD", "FLOOD") %>%
  return()
}
eventVect <- convertEvent(stormDat$eventtype)
print(                                      ## Reduction in event types through
  paste0("Index reduced by ",               ## consolidation.
         length(sort(unique(stormDat$eventtype))) -
         length(sort(unique(eventVect))), 
         " event type/names" ))

stormDat <- (mutate(                        ## Inserts new event classification
               stormDat,                    ## vector as variable in position 9
               eventclass = eventVect))[,c(1:8,37,9:36)]
print(stormDat[                             ## Print some of the new data
               sample(1:length(eventVect),  ## showing substitution/inter-
               20), c(6:9)])                ## pretation
getQ1 <- function () {
##require(dplyr)

  sumInj <- 
     data.frame(eventclass=unique(stormDat[stormDat$injuries>=1,]$eventclass), 
               injuries = 0)
  for (evCl in sumInj$eventclass) {
    sumInj[sumInj$eventclass==evCl,]$injuries <- 
      sum(stormDat[stormDat$eventclass==evCl,]$injuries) 
  }
  sumInj <- sumInj[order(sumInj$injuries, decreasing=TRUE),][1:6,]
  P1 <- ggplot(sumInj, aes(x=eventclass, y=injuries,
                          main = "Injuries by Weather Event")) +
        geom_boxplot( aes(xlab = "Weather Event Type",
                          ylab = "Injuries")) +
        scale_x_discrete(limits=sumInj$eventclass) +
        theme_bw(base_size = 9) +
        theme(axis.text.x = element_text(angle = 20,hjust = 1)) 

#sumInj <- data.frame(summarize(             ## Select for injury data grouped 
#                       group_by(stormDat,   ## and sum
#                                eventclass),
#                       injuries = sum(injuries)))
#colnames(sumInj) <- c("eventclass", "injuries")        ## Fix column names                      
#sumInj <- (sumInj[order(sumInj$injuries,    ## Reorder by injuries high-to-low
#                       decreasing=TRUE),])[1:6,]
#P1 <- qplot(eventclass, injuries,  data=sumInj, geom = "boxplot") + 
#      aes(main = "Injuries by Weather Event",
#          xlab = "Weather Event Type", ylab = "Injuries",
#          ylimit = max(sumInj$injuries)) 

  sumFatal <- 
    data.frame(eventclass=unique(stormDat[stormDat$fatalities>=1,]$eventclass), 
               fatalities = 0)
  for (evCl in sumFatal$eventclass) {
    sumFatal[sumFatal$eventclass==evCl,]$fatalities <- 
      sum(stormDat[stormDat$eventclass==evCl,]$fatalities) 
  }

  sumFatal <- sumFatal[order(sumFatal$fatalities, decreasing=TRUE),][1:6,]

  P2 <- ggplot(sumFatal, aes(x=eventclass, y=fatalities,
                          main = "Fatalities by Weather Event")) +
        geom_boxplot( aes(xlab = "Weather Event Type",
                          ylab = "Injuries")) +
        scale_x_discrete(limits=sumFatal$eventclass) +
        theme_bw(base_size = 9) +
        theme(axis.text.x = element_text(angle = 20,hjust = 1))

#sumFatal <- data.frame(summarize(           ## Select total number of fatalities
#                         group_by(stormDat, ## by group
#                                  eventclass), 
#                       sum(fatalities)))
#colnames(sumFatal) <- c("eventclass",       ## Rename the columns
#                        "fatalities")
#sumFatal <-                                 ## Reorder the data with the highest
#  sumFatal[order(sumFatal$fatalities,       ## number first
#                  decreasing = TRUE),]
#sumFatal <- sumFatal[1:6,]
#P2 <- qplot(eventclass, fatalities,  data=sumFatal, geom = "boxplot") +
#      theme(axis.text.x = element_text(angle = 20, hjust = 1)) 

  sumInj92 <- 
    data.frame(eventclass=unique(stormDat[stormDat$injuries >= 1 &
                                          stormDat$begindate >= 
                                            as.Date("1992-01-01"),]$eventclass), 
               injuries = 0)
  for (evCl in sumInj92$eventclass) {
    sumInj92[sumInj92$eventclass==evCl,]$injuries <- 
      sum(stormDat[stormDat$eventclass==evCl,]$injuries) 
  }
  sumInj92 <- sumInj92[order(sumInj92$injuries, decreasing=TRUE),][1:6,]
  P3 <- ggplot(sumInj92, aes(x=eventclass, y=injuries,
                          main = "Injuries by Weather Event")) +
        geom_boxplot( aes(xlab = "Weather Event Type",
                          ylab = "Injuries")) +
        scale_x_discrete(limits=sumInj92$eventclass) +
        theme_bw(base_size = 9) +
        theme(axis.text.x = element_text(angle = 20,hjust = 1)) 

#sumInj92 <- 
#  data.frame(summarize(
#               group_by(stormDat[stormDat$begindate >=
#                                 as.Date("1992-01-01"),],
#                        eventclass),
#               sum(injuries)))
#colnames(sumInj92) <- c("eventclass",
#                        "injuries")
#sumInj92 <- (sumInj92[order(sumInj92$injuries, 
#                           decreasing=TRUE),])[1:6,]
#P3 <- qplot(eventclass, injuries,  data=sumInj92, geom = "boxplot") +
#      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

  sumFatal92 <- 
    data.frame(eventclass = unique(stormDat[stormDat$fatalities >= 1 &
                                            stormDat$begindate >= 
                                              as.Date("1992-01-01"),]$eventclass), 
               fatalities = 0)
  for (evCl in sumFatal92$eventclass) {
    sumFatal92[sumFatal92$eventclass==evCl,]$fatalities <- 
      sum(stormDat[stormDat$eventclass==evCl,]$fatalities) 
  }

  sumFatal92 <- sumFatal92[order(sumFatal92$fatalities, decreasing=TRUE),][1:6,]

  P4 <- ggplot(sumFatal92, aes(x=eventclass, y=fatalities,
                          main = "Fatalities by Weather Event")) +
        geom_boxplot( aes(xlab = "Weather Event Type",
                          ylab = "Injuries")) +
        scale_x_discrete(limits=sumFatal92$eventclass) +
        theme_bw(base_size = 9) +
        theme(axis.text.x = element_text(angle = 20,hjust = 1))
#sumFatal92 <- 
#  data.frame(summarize(
#               group_by(stormDat[stormDat$begindate >=
#                                 as.Date("1992-01-01"),], 
#                        eventclass), 
#               sum(fatalities)))
#colnames(sumFatal92) <- c("eventclass", 
#                          "fatalities")
#sumFatal92 <- (sumFatal92[order(sumFatal92$fatalities, 
#                               decreasing = TRUE),])[1:6, ]
#P4 <- qplot(eventclass, fatalities,  data=sumFatal92, geom = "boxplot") +
#      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  head(sumInj, 3)                             ## Report top 3
  head(sumFatal, 3)
  head(sumInj92, 3)
  head(sumFatal92, 3) 

## conclusions for question 1
  multiplot(P1, P2, P3, P4, cols=2)           ## multipane plots to screen      
}
## Echo off -- memory management not pertinent to report processing.
#rm(sumFatal,  sumInj, sumFatal92,  sumInj92,## Explicitly cleanup Q1 temp data 
#   convertEvent, targFileName, loadStormDat, 
#   downLoadData, insertMark, cleanStormDat,
#   P1, P2, P3, P4)

showExpnt <- function () {
  unique(stormDat$propertydamageexp)          ## Show property exponent variable
  unique(stormDat$cropdamageexp)              ## Show the crop exponent variable
}

cleanExpnt <- function(expVect) {           ## Function replaces exponent values
  as.character(expVect) %>%                 ## with useful numeric equivalents
  toupper() %>%                             ## and strips out unused symbols and
  chainGsub("H", "2") %>%                   ## values
  chainGsub("K", "3") %>%
  chainGsub("M", "6") %>%
  chainGsub("B", "9") %>%
  chainGsub("\\?", "") %>%
  chainGsub("\\+", "") %>%
  chainGsub("\\-", "") %>%
  chainGsub("0", "") %>%
  chainGsub("1", "") %>%
  return()
 }
 
ques2 <- function() {
  stormDat$cropdamageexp <- 
    as.factor(cleanExpnt(stormDat$cropdamageexp))
  stormDat$propertydamageexp <- 
    as.factor(cleanExpnt(stormDat$propertydamageexp))
  unique(stormDat$propertydamageexp)          ## Show property exponent variable
  unique(stormDat$cropdamageexp)              ## Show the crop exponent variable

  require(dplyr)
  require(plyr)
  costDat <- data.frame(eventclass = stormDat$eventclass,
                        propDamage = (as.numeric(stormDat$propertydamage) *
                                     (10^as.numeric(stormDat$propertydamageexp))))

  costDat <- summarize(group_by(costDat, eventclass), mean(propDamage))
  colnames(costDat) <- c("eventclass", "propDamage")

  costDat <-                                  ## Reorder by damage high-to-low
    costDat[order(costDat$propDamage, decreasing=TRUE),]

  costDat$propDamage <- formatC(costDat$propDamage/(10^9), 
                                width=8, 
                                digits=2, 
                                format="f")
  plot(costDat[1:6, 2],
       type = "l" , 
       ylab = "Property Damage Cost ($Billions)",
       xlab = "Weather Event Types", 
       main = "Property Damage by Weather Event")
  head(costDat, 3)

  costDat <- data.frame(eventclass = stormDat$eventclass,
                        cropDamage = (as.numeric(stormDat$cropdamage) *
                                     (10^as.numeric(stormDat$cropdamageexp))))
  costDat <- summarize(group_by(costDat, eventclass), mean(cropDamage))
  colnames(costDat) <- c("eventclass", "cropDamage")
  costDat <-                                  ## Reorder by damage high-to-low
    costDat[order(costDat$cropDamage, decreasing=TRUE),]
  costDat$cropDamage <- formatC(costDat$cropDamage/(10^3), 
                                width=8, 
                                digits=2, 
                                format="f")
  plot(costDat[1:6, 2],
       type = "l" ,
       ylab = "Crop Damage Cost ($Thousands)",
       xlab = "Weather Event Types", 
       main = "Crop Damage by Weather Event")
  head(costDat, 3)
}

## Echo off -- memory managment step not pertinent to analyses
#rm(costDat)                                 ## Leave stormDat in environment
                                            ## with functions.

