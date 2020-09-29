###########################################################################
################### Svalbard kittiwake trips: numbered ####################
###########################################################################

# setwd("/Users/stephh/Dropbox/PhD/Data/")
# setwd("/Users/Steph/Dropbox/PhD/Data")

###########################################################################
########## Packages #######################################################

necessary.packages <- c("rptR","mapproj","lubridate","PBSmapping","RColorBrewer",
                        "raster","rasterVis","maps","mapdata","globe","showtext","plotrix",
                        "stringr",'adehabitatLT', "chron", "reshape2", "ggplot2")
already.installed <- necessary.packages%in%installed.packages()[, 'Package']
if (length(necessary.packages[!already.installed])>=1) {  
  install.packages(necessary.packages[!already.installed],dep=T)
}
sapply(necessary.packages,function(p) {require(p,quietly=T,character.only=T)})


### run files containing functions:

source("C:/Users/philip/OneDrive/RissaProject/RScripts/StephScripts/Track_calculations.R", echo=TRUE)
source("C:/Users/philip/OneDrive/RissaProject/RScripts/StephScripts/Trip_metrics.R", echo=TRUE)



###################################################################
############# Data Processing #####################################

### read in the GPS metadata (here I run this entire script on Kongsfjorden then Grumant data and rbind the final results, allbirds)
# filepath <- "/Kongsfjorden/GPS/GPS Kongsfjord 2017/"
# metafile <- read.csv("../Data/Kittiwakes/Kongsfjorden/GPS/GPS_2017.csv")
# filepath <- "/Grumant/GPS/GPS Grumant 2017/"
# metafile <- read.csv("../Data/Kittiwakes/Grumant/GPS/GPS_Grumant2017.csv")

#Here all lines fit one bird trips
Colony <- read.csv("C:/Users/philip/OneDrive/RissaProject/Data_ProjectKittiwakes/MAP/CoordColonies/CoordColonies.csv",sep=",", header=T, na.strings=c("","NA"))
metafile <- read.csv("C:/Users/philip/OneDrive/RissaProject/TRACKS/METADATA/METADATA_Kongsfjorden.csv",sep=",", header=T, na.strings=c("","NA"))
metafile <- metafile[!is.na (metafile$GPS.Recovered) & metafile$GPS.Recovered!="LOST",]

# define from when the tracks should begin and end: when logger was deployed/retrieved
metafile$start <- as.POSIXct(strptime(paste (as.character(metafile$Deployment),
                                             as.character(metafile$UTC.Deployment)),"%Y-%m-%d %H:%M", tz="GMT"), "GMT")
metafile$end   <- as.POSIXct(strptime(paste (as.character(metafile$Recapture.1),
                                             as.character(metafile$UTC.Retrieval)),"%Y-%m-%d %H:%M", tz="GMT"), "GMT")

metafile$CLatitude <- Colony$Latitude[match(metafile$Colony, Colony$Colony)]
metafile$CLongitude <- Colony$Longitude[match(metafile$Colony, Colony$Colony)]

## create a track ID synonymous with how the files are named:
## I use date of retrieval and ring number
metafile$ID <- paste(metafile$Year, "_", metafile$ACRNNID, "_", metafile$Metal.ring, "_", metafile$GPS.Name, "_", format(metafile$end,"%Y-%m-%d"),sep="")
#GPS.Units       <- GPS.Units[order(GPS.Units$ID),]

# Identifying potential mistakes in deployment/retrieval date
Date.diff <-  metafile$end - metafile$start;  metafile$ID[which(Date.diff <= 0)]

##  split by early or late incubation - there were two inc deployment sessions
# metafile$stage <- ifelse(as.Date(metafile$Deployment.date) > as.Date("2017-07-03"), "late", "early")

##Testing GPS concordance with St?ph
#setwd("~/Research/RissaProject/TRACKS/test2017")

### read in the GPS files
setwd("C:/Users/philip/OneDrive/RissaProject/TRACKS/Tracks_Ch2/")
Filext <- ".csv"
file.name <- list.files(getwd())[grep(Filext,list.files(getwd()))]; rm(Filext)
files   <- length(file.name)

### read in the GPS files
#allFiles <- list.files(paste0("../Data/Kittiwakes/",filepath))
#correctFiles <- allFiles[grep(".csv", allFiles)] # csv files only
#files <- length(correctFiles)

### files still contain some GPS points from before and after deployment, e.g. on the boat
### this loop subsets the files to just the data between deployment and retrieval times in the metafile
### and interpolates the files to a 2 minute/10 minute resolution (respective of breeding stage)

allbirds.list <- list() # this empty list will be populated with the GPS files
p.list <- list()
atS <- list()
correctFiles <- file.name

param <- seq(1, 10, by = 1)
#rs <- seq(1, length(correctFiles), by = 1)

for (i in 1:length(param)) {
  
  p <- param[i]
  
  for (f in 1:999) {
    
    #bird <- read.delim(paste("../Data/Kittiwakes/",filepath,correctFiles[i],sep=""),sep=",")
    
    r <- sample(1:length(correctFiles), 1)
    
    ## Partitionning between two types of GPS-based file (two format; 1GEN & CHIP-PATCH)
    GPSType <- metafile$GPS.Type[which(metafile$ID == gsub(".csv", "", correctFiles[r]))]
    
    if(GPSType == "1GEN"){ bird <- read.delim(paste(correctFiles[r],sep=""),sep=",")
    } else { bird <- read.delim(paste(correctFiles[r],sep=""),sep=",", skip = 6) }
    
    ## add track ID
    bird$trackID <- unlist(strsplit(correctFiles[r], "[.]"))[1]
    
    # extract start and end times for each file
    start <- metafile$start[which(metafile$ID == gsub(".csv", "", correctFiles[r]))]
    end <- metafile$end[which(metafile$ID == gsub(".csv", "", correctFiles[r]))]
    int <- interval(start, end, tzone = "GMT") ## must make sure it's the right timezone!!
    
    # extract the colony coords
    col  <- metafile$Colony[which(metafile$ID == gsub(".csv", "", correctFiles[r]))]
    Clat <- metafile$CLatitude[which(metafile$ID == gsub(".csv", "", correctFiles[r]))]
    Clong <- metafile$CLongitude[which(metafile$ID == gsub(".csv", "", correctFiles[r]))]
    
    ## make sure date in the right format - two different conversion methods cos multiple date formats
    
    mdy <- mdy(bird$Date)
    ymd <- ymd(bird$Date)
    mdy[is.na(mdy)] <- ymd[!is.na(ymd)] # give the working one precedence
    bird$Date <- format(mdy, "%Y/%m/%d")
    bird$Time <- chron(times=bird$Time)
    
    bird$datetime <- strptime(paste(gsub("/", "-", bird$Date), bird$Time), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    bird <- bird[order(bird$datetime , decreasing = FALSE ),]
    
    ## now subset to points within this time window
    bird <- bird[bird$datetime %within% int, ]
    
    ####### add speed (overwrite device data) m/s
    bird$Speed <- speed(bird)
    #buffer speed to <80km/h
    bird <- subset(bird, bird$Speed < 22.22222222222222)
    
    #### interpolate data so points are spread 2 or 10 mins apart (for chick/incubation trips)
    ## what breeding stage?
    #brstage <- as.character(metafile$Phenology[which(metafile$ID == gsub(".csv","",correctFiles[i]))])
    #resolution <- ifelse(brstage == "chick rearing", 120, 600) # 120 seconds for chick rearing, 600 seconds for incubation
    Sres <- (metafile$FIX[which(metafile$ID == gsub(".csv","",correctFiles[r]))])*60
    Mres <- (metafile$FIX[which(metafile$ID == gsub(".csv","",correctFiles[r]))])
    
    ##Regularization process later on in the script  
    refda <- min(bird$datetime)
    Nbird <- as.ltraj(xy = bird[,4:3], date = as.POSIXct(bird$datetime), id = bird$trackID)
    wost_NA <- setNA(Nbird,refda,Mres,units="min")
    wost_demo <- sett0(wost_NA,refda,Mres,units="min")
    Nbird <- redisltraj(na.omit(wost_demo), Sres, type = "time")
    
    if(nrow(Nbird[[1]]) > 1){
      
      if(Sres < 600) {
        Nbird <- subsample(Nbird, 10, units = c("min"))
      }
    }
    
    Nbird <- ld(Nbird)
    bird <- data.frame(datetime = Nbird$date, Latitude = Nbird$y, Longitude = Nbird$x, trackID = Nbird$id)
    
    # get distance flown from the colony
    #bird$ColonyDist <- Dist.from.colony(bird)
    CLL <- cbind(Clong, Clat)
    bird$ColonyDist <- TDist.from.colony(bird, CLL)
    #bird$ColonyDist = bird$ColonyDist - 0.3
    
    # set a buffer of 300m around the colony
    bird$ColonyorTrip <- ifelse(bird$ColonyDist > 0.2, "trip","colony")
    #bird$ColonyorTrip <- ifelse(bird$ColonyDist > 0.3, "trip","colony")
    #bird$ColonyorTrip <- ifelse(bird$ColonyDist < 0, "colony","trip")
    
    # set all colony points to 0
    bird$ColonyDist[which(bird$ColonyorTrip == "colony")] <- 0
    
    
    ######## add trip number
    tripID <- 1 # a counter, I changed it from 0 to 1
    nRows <- length(bird$ColonyorTrip) - 1
    bird$tripID <- 0
    
    ### this big for loop numbers all trips, including the one colony point before
    ### and after the last trip point
    
    ##Need to consider one colony only
    
    for (j in 2:nRows) {
      
      if(bird$ColonyorTrip[1] == "trip") {bird$tripID[1] <- 1}
      if(bird$ColonyorTrip[1] == "colony" && bird$ColonyorTrip[j] == "trip") {bird$tripID[1] <- 1}
      if(bird$ColonyorTrip[j] == "colony" && bird$ColonyorTrip[j] != bird$ColonyorTrip[j+1] &&
         bird$ColonyorTrip[j-1] != bird$ColonyorTrip[j+1]) {
        tripID <- tripID + 1
      } else {bird$tripID[j] <- tripID}
      if(bird$ColonyorTrip[j] == "trip" | bird$ColonyorTrip[j] != bird$ColonyorTrip[j-1]
         | bird$ColonyorTrip[j] != bird$ColonyorTrip[j+1]) {
        bird$tripID[j] <- tripID
      } else {
        bird$tripID[j] <- 0
      }
      
      if(bird$ColonyorTrip[nRows+1] == "trip") {
        bird$tripID[nRows+1] <- tripID ## set the last row correctly
        
      }
      
    }
    
    bird$birdTrip <- paste(bird$trackID, bird$tripID, sep = "_") # this will be the ID variable
    bird$speed <- speed(bird)
    #bird <- subset(bird, bird$tripID > 0)  # get rid of colony points
    
    p.list <- list()
    
    for (o in 1:length(unique(bird$tripID))) { ###Number of points per track
      
      test <- subset(bird, bird$tripID == unique(bird$tripID)[o])
      
      tripDurs <- as.data.frame(table(test$birdTrip))
      test$nPoints <- tripDurs$Freq[match(test$birdTrip, tripDurs$Var1)]
      test$maxDist <- max(test$ColonyDist, na.rm = TRUE)
      test$maxSpd <- max(test$speed, na.rm = TRUE)
      p.list[[o]] <- test  
      rm(test)
      
    }
    
    alltrips <- do.call(rbind, p.list)
    rm(p.list)
    
    notime <- alltrips[, c("birdTrip", "ColonyDist","ColonyorTrip")]
    
    trip.Bincomplete <- ddply(notime, .(birdTrip), function(x) x[1, ]) ## gets the first point for each trip
    colnames(trip.Bincomplete) <- c("birdTrip", "ColonyDist", "FR")
    trip.Eincomplete <- ddply(notime, .(birdTrip), function(x) x[nrow(x), ]) ## gets the last point for each trip
    colnames(trip.Eincomplete) <- c("birdTrip", "ColonyDist", "ER")
    
    alltrips$BegPoint <- trip.Bincomplete$FR[match(alltrips$birdTrip, trip.Bincomplete$birdTrip)]
    alltrips$EndPoint <- trip.Eincomplete$ER[match(alltrips$birdTrip, trip.Eincomplete$birdTrip)]
    
    #alltrips <- subset(alltrips, alltrips$BegPoint == "colony")  # get rid of incomplete trips
    #alltrips <- subset(alltrips, alltrips$EndPoint == "colony")  # get rid of incomplete trips
    
    ## first remove trips < 1 hour !!! Need to standardized before!!!
    alltrips <- droplevels(alltrips[alltrips$nPoints > p,])
    
    lall <- alltrips[!duplicated(alltrips$birdTrip),]
    lall$nbTrips <- length(unique(lall$birdTrip))
    lall <- lall[!duplicated(lall$trackID),]
    
    allbirds.list[[f]] <- lall # put all of these cut down frames in a list
    rm(alltrips)
    allbirds <- do.call(rbind, allbirds.list) # convert the list into one big dataframe
    
  }
  
  cat("Scenario =", p, "\n")
  allbirds$Scenario <- paste(p)
  atS[[i]] <- allbirds
  rm(allbirds)
  
}

d <- do.call(rbind, atS)
d$Scenario <- as.numeric(d$Scenario)
d$Scenario <- d$Scenario*10
p1 <- ggplot(d,aes(x = Scenario, y = nbTrips)) + 
           stat_summary(fun.data = "mean_cl_boot") +
           stat_summary(fun.y = mean, geom = "line") + theme_bw() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_vline(xintercept = 50, alpha = 0.1, col = "blue", size = 5)

ggdf <- ggplot_build(p1)
gdf <- ggdf$data[[1]]
df <- gdf[, c(1, 3)]
colnames(df) <- c("Scenario", "Mean")

C <- rep(NA, 10)
Ci <- rep(NA, 9)

for (i in 2:nrow(df)){
  C[i] <- df$Mean[[i]] - df$Mean[[i - 1]]
}

for (i in 2:nrow(df)-1){
  Ci[i] <- abs(C[i + 1] - C[i])
}

Ci[10] <- NA
df[, 3] <- Ci 

#df$Scenario <- mapvalues(df$Scenario, from=c("1000", "2000", "3000", "4000", "5000", "6000", "7000", "8000", "9000", "10000"), to=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100"))

p2 <- ggplot(df, aes(x = Scenario, y = V3)) +
  geom_point() + geom_line() + theme_bw() + theme(legend.position = "none", 
    axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_vline(xintercept = 50, alpha = 0.1, col = "blue", size = 5)

g <- grid.arrange(p1, p2, ncol=2,left = "Number of trips per individual", bottom = "Scenario (m)")
#setwd("C:/Users/philip/OneDrive/RissaProject/TRACKS/FinalFigure/ChpII")
#ggsave(g, filename = "atS.png", bg = "transparent", width = 14, height = 8, units = "cm")                     
