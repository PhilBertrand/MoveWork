###########################################################################
####################### Track filtering and numbered ######################
###########################################################################

#### Produced by Philip Bertrand, PhD Candidate
#### SEAPOP movement data
#### Depends: chron, adehabitatHR, plyr, trip
#### Version: 1.0
#### Raw data are expected to be in .csv format
#### @pathF is the path to your tracks
#### @pathM is the path to your metadata file
#### @metname is the name of your metadata file
#### @timezone is the time zone of your GPS file and metadata
#### @return allbirds, all trips delineated according to certain filters

pathF <- c("C:/Users/philip/OneDrive/NP_Kontrakt/StromTracks/files/allspecies/")
pathM <- c("C:/Users/philip/OneDrive/NP_Kontrakt/StromTracks/metadata/")
metname <- c("meta_bjorn.csv")
timezone <- c("GMT")


## need slots for colony, deployement, recapture, utc_recap, utc_dep, year, ring,
## longitude, latitude

f <- filt(pathF, pathM, metname, timezone, speedTresh = 25, gpst = "GPSType", 
          ddep = "deployment", drecap = "recapture", colony = "colony", year = "year", 
          ring = "ring", tdep = "utc_deployment", trecap = "utc_retrieval",
          BuffColony = 0.5, MinTripDur = 60, Complete = T, FixInt = 2, 
          Interpolate = T, filtNA = 0.8, splt = F)

filt <- function(pathF = ..., pathM = ..., metname = NULL, gpst = NULL, ddep = NULL, 
                 drecap = NULL, colony = NULL, year = NULL, ring = NULL, tdep = NULL, 
                 trecap = NULL, timezone = NULL, speedTresh = NULL, FixInt = NULL, 
                 BuffColony = NULL, MinTripDur = NULL, Complete = FALSE,
                 Interpolate = FALSE, filtNA = 1, splt = TRUE) {

  if (class(metname) != "character") 
    stop("metname should be a character")
  if (class(gpst) != "character") 
    stop("gpst should be a character")
  if (class(ddep) != "character") 
    stop("ddep should be a character")
  if (class(drecap) != "character") 
    stop("drecap should be a character")
  if (class(colony) != "character") 
    stop("colony should be a character")
  if (class(year) != "character") 
    stop("year should be a character")
  if (class(ring) != "character") 
    stop("ring should be a character")
  if (class(tdep) != "character") 
    stop("tdep should be a character")
  if (class(trecap) != "character") 
    stop("trecap should be a character")
  if (class(timezone) != "character") 
    stop("the timezone should be a character")
  if (!is.null(speedTresh) & class(speedTresh) != "numeric") 
    stop("the speed treshold should be numeric")
  if (!is.null(FixInt) & class(FixInt) != "numeric") 
    stop("the time interval between successive fixes should be numeric")
  if (!is.null(BuffColony) & class(BuffColony) != "numeric") 
    stop("the buffer size should be numeric")
  if (!is.null(MinTripDur) & class(MinTripDur) != "numeric") 
    stop("the minimum time duration (min) that a trip should have should be numeric")
  if (Complete != TRUE & Complete != FALSE)
    stop("Logical: Completness function should be approved (TRUE) or delcine (FALSE)")
  if (!is.null(FixInt) & class(FixInt) != "numeric") 
    stop("the time interval between successive fixes should be numeric")
  if (class(filtNA) != "numeric" | c(filtNA < 0 | filtNA > 1))
    stop("filtering NAs induced by interpolation needs a proportion as treshold; 0-1")
  


  pack <- c("chron", "adehabitatHR", "plyr", "trip", "lubridate")
  
  if (length(setdiff(pack, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(pack, rownames(installed.packages())))  
  }
  
  sapply(pack, function(p) {require(p, quietly=T, character.only = T)})
  
  ## Importing file's names
  Filext <- ".csv"
  file.name <- list.files(pathF)[grep(Filext,list.files(pathF))]; rm(Filext)
  files   <- length(file.name)
  
  ## Importing metadata; need to remove path to pathM, no permission is denied
  metafile <- read.csv(paste(pathM, metname, sep=""), sep = ",", header=T, na.strings=c("","NA"))
  
  ## define from when the tracks should begin and end: when logger was deployed/retrieved
  metafile$start <- as.POSIXct(strptime(paste (as.character(metafile[[ddep]]),
                     as.character(metafile[[tdep]])),"%Y-%m-%d %H:%M", tz=timezone), timezone)
  metafile$end   <- as.POSIXct(strptime(paste (as.character(metafile[[drecap]]),
                     as.character(metafile[[trecap]])),"%Y-%m-%d %H:%M", tz=timezone), timezone)
  
  ## create a track ID synonymous with how the files are named:
  metafile$ID <- paste(metafile[[year]], "_", metafile[[colony]], "_", metafile[[ring]], "_", format(metafile$end,"%Y-%m-%d"),sep="")

  Date.diff <-  metafile$end - metafile$start; if( length(metafile$ID[which(Date.diff <= 0)]) != 0 ) 
      stop('Date of retrieval should be later than date of deployment')
  
  allbirds.list <- list()
  
    for (i in 1:files) { ## Loop delineating trips on each file i.e. GPS deployement
    
  ## Partitionning between two types of GPS-based file
  ## Need to have a column specifying GPS type in the metadata file
  GPSType <- metafile[, gpst][which(metafile$ID == gsub(".csv", "", file.name[i]))]
  if(identical(GPSType, character(0))) {stop("You need to have a valid GPS type. Referring to gpst")}
  
  ## This line remove the 6 first unecassary lines in the CHIP-PATCH GPS Type
  ## The 1GEN is read as it is
  if(GPSType != "Catlog" & GPSType != "IGotU" & GPSType != "Pathtrack" & GPSType != "Ecotone") {stop("the GPS type should be either Catlog, IGotU, Ecotone or Pathtrack")}
  if(GPSType == "Catlog") { bird <- read.csv(paste(pathF, file.name[i], sep=""), sep = ",", skip = 6) } 
  if(GPSType == "IGotU"){ bird <- read.csv(paste(pathF, file.name[i], sep=""), sep = ",") }
  if(GPSType == "Ecotone"){ bird <- read.csv(paste(pathF, file.name[i], sep=""), sep = ",") 
    bird$Date <- as.Date(with(bird, paste(Year, Month, Day, sep="/")), "%Y/%m/%d")
    bird$Time <- sprintf("%s:%s:%s", bird$Hour, bird$Minute, bird$Second)
    bird <- subset(bird, !is.na(bird$Latitude) & !is.na(bird$Longitude))}
  if(GPSType == "Pathtrack"){ 
    bird <- read.csv(paste(pathF, file.name[i], sep=""), sep = ",", skip =5) 
    colnames(bird) <- c("Day", "Month", "Year", "Hour", "Minute", "Second", "SecondDay",
                        "NumberSats", "Latitude", "Longitude", "Altitude", "ClockOff", 
                        "Accuracy", "BatLevel")
    bird$Year <- bird$Year + 2000
    bird$Date <- as.Date(with(bird, paste(Year, Month, Day, sep="/")), "%Y/%m/%d")
    bird$Time <- sprintf("%s:%s:%s", bird$Hour, bird$Minute, bird$Second)
    bird <- subset(bird, bird$Latitude > 0 & bird$Longitude > 0)}
  
  ## This assume to have a meaningful file.name, will be used thereafter as trackID
  bird$trackID <- unlist(strsplit(file.name[i], "[.]"))[1]
  
  ## extract start and end times for track, needs to be linked to a metadata file
  ## should implement an object, for specifying start, end & timezone
  start <- metafile$start[which(metafile$ID == gsub(".csv", "", file.name[i]))]
  end <- metafile$end[which(metafile$ID == gsub(".csv", "", file.name[i]))]
  int <- interval(start, end, tzone = timezone)
  
  ## Extract colony corrdinates and building of a data.frame
  ## Will be used later in the script for colony-location distance
  CLong <- metafile$Clongitude[which(metafile$ID == gsub(".csv", "", file.name[i]))]
  CLat <- metafile$Clatitude[which(metafile$ID == gsub(".csv", "", file.name[i]))]
  CLL <- cbind(CLong, CLat)
  
  ## make sure date in the right format - ymd; creating the time object, then ordering since
  ## sometimes, data are not in chronological order
  mdy <- mdy(bird$Date, quiet = TRUE)
  ymd <- ymd(bird$Date, quiet = TRUE)
  mdy[is.na(mdy)] <- ymd[!is.na(ymd)] # give the working one precedence
  bird$Date <- format(mdy, "%Y/%m/%d")
  bird$Time <- chron(times=bird$Time)
  bird$datetime <- strptime(paste(gsub("/", "-", bird$Date), bird$Time), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  bird <- bird[order(bird$datetime , decreasing = FALSE ),] ## assuring the chronological order of fixes
  
  ## now subset to points within this time start/end window
  bird <- bird[bird$datetime %within% int, ]
  
  ## overwrite device speed data, in m/s
  trip.matrix <- data.matrix(bird[,c("Longitude","Latitude")], rownames.force = NA) #creates two column matrix of lat and long for trip trackDistance function
  between.point.distances <- trackDistance(trip.matrix, longlat = TRUE) #calculate distance in km between each GPS point, into vector
  bird$PointDist <- c(0, between.point.distances) #dist in km
  bird$TimeElapsed <- 0 #create empty column to fill with for loop
  
  for (k in 2:NROW(bird)){
      bird$TimeElapsed[k] <- difftime(bird$datetime[k], bird$datetime[k-1], units = "secs") #time diff in secs
    }
  
  bird$Speed <- (bird$PointDist * 1000)/bird$TimeElapsed ## m/s                                             
  
  if (!is.null(speedTresh)) {
      bird <- subset(bird, bird$Speed < speedTresh)
    }
  
  ## Extracting GPS interval, determined from the metadata
  Sres <- (metafile$FIX[which(metafile$ID == gsub(".csv","",file.name[i]))])*60
  Mres <- (metafile$FIX[which(metafile$ID == gsub(".csv","",file.name[i]))])
  
  if(Interpolate == TRUE) {
    
    refda <- min(bird$datetime)
    Nbird <- as.ltraj(xy = data.frame(bird$Longitude, bird$Latitude), date = as.POSIXct(bird$datetime), id = bird$trackID)
    wost_NA <- setNA(Nbird,refda,Mres,units="min")
    wost_demo <- sett0(wost_NA,refda,Mres,units="min")
    Nbird <- redisltraj(na.omit(wost_demo), u = Sres, type = "time")
    Nbird[[1]]$nbNA <- ifelse(Nbird[[1]]$x %in% wost_demo[[1]]$x, 0, 1)
    
    tmpN <- ld(Nbird)
    Nbird <- as.ltraj(xy = data.frame(tmpN$x, tmpN$y), date = as.POSIXct(tmpN$date), id = tmpN$id, infolocs = tmpN[, c(11, 12)])
    
    if(!is.null(FixInt)) {
     if(nrow(Nbird[[1]]) > 1){ ### Standardized as matter of time treshold
      if(Mres != FixInt) {
       if( FixInt < max(metafile$FIX, na.rm = TRUE)  ) stop("your selected treshold for ", print(bird$trackID[1]), " is smaller than the greater interval in your sampled tracks - meaning that your are creating points")
        Nbird <- subsample(Nbird, FixInt, units = c("min"))
       }
      }
     }
    
    Nbird <- ld(Nbird)
    bird <- data.frame(datetime = Nbird$date, Latitude = Nbird$y, Longitude = Nbird$x, trackID = Nbird$id, nbNA = Nbird$nbNA)
    
    } else { bird <- bird[, c("datetime", "Latitude", "Longitude", "trackID")]; bird$nbNA <- 0 }
  
  ## get distance flown from the colony
  trip.matrix <- data.matrix(bird[,c("Longitude","Latitude")], rownames.force = NA) #creates two column matrix of lat and long for trip trackDistance function
  bird$ColonyDist <- spDistsN1(trip.matrix, CLL, longlat = TRUE) #calculate distances between GPS points and initial GPS point in km
  between.point.distances <- trackDistance(trip.matrix, longlat = TRUE) #calculate distance in km between each GPS point, into vector
  bird$PointDist <- c(0, between.point.distances) #dist in km
  
  if(is.null(BuffColony)) {
    bird$ColonyorTrip <- c("trip")
    bird$tripID <- 1
    } else {

  ## Setting buffer treshold
  bird$ColonyorTrip <- ifelse(bird$ColonyDist > BuffColony, "trip","colony") #in km
  
  ## set all colony points to 0
  bird$ColonyDist[which(bird$ColonyorTrip == "colony")] <- 0
  
  ## add trip number
  tripID <- 1
  nRows <- length(bird$ColonyorTrip) - 1
  bird$tripID <- 0
  
  ### this big for loop numbers all trips, including the one colony point before
  ### and after the last trip point
  if(bird$ColonyorTrip[1] == "colony" && bird$ColonyorTrip[2] == "trip") {bird$tripID[1] <- tripID}  else {bird$tripID[1] <- 0}
  if(bird$ColonyorTrip[1] == "trip") {bird$tripID[1] <- tripID}
  
  for (j in 2:nRows) {
    if(bird$ColonyorTrip[j] == "colony" && bird$ColonyorTrip[j] != bird$ColonyorTrip[j+1] &&
       bird$ColonyorTrip[j-1] != bird$ColonyorTrip[j+1]) {
      tripID <- tripID + 1
    } else {bird$tripID[j] <- tripID}
    if(bird$ColonyorTrip[j] == "trip" | bird$ColonyorTrip[j] != bird$ColonyorTrip[j-1]
       | bird$ColonyorTrip[j] != bird$ColonyorTrip[j+1]) {
      bird$tripID[j] <- tripID
    } else {
      bird$tripID[j] <- 0} 
    if(bird$ColonyorTrip[nRows+1] == "trip") {
      bird$tripID[nRows+1] <- tripID ## set the last row correctly
     }
    }
   }
  
  ## Creating unique ID variable
  bird$birdTrip <- paste(bird$trackID, bird$tripID, sep = "_")
  
  ## Getting rid of colony points; perhaps YES or NO in the function()
  bird <- subset(bird, bird$tripID > 0)
  
  ## Loop estimating the number of points per trip
  p.list <- list()
  
  for (o in 1:length(unique(bird$tripID))) {
    
    test <- subset(bird, bird$tripID == unique(bird$tripID)[o])
    
    test$TripLength <- difftime(max(test$datetime),min(test$datetime),   #calculate time elapsed between start and end
                                units = ("min")) 
    
    trip.matrix <- data.matrix(test[,c("Longitude","Latitude")], rownames.force = NA) #creates two column matrix of lat and long for trip trackDistance function
    distbp <- trackDistance(trip.matrix, longlat = TRUE) #calculates distance between each GPS point, into vector
    TDist <- sum(distbp) ## Total distance travelled per trip (m)
    test$TripDist <- TDist
    
    tripDurs <- as.data.frame(table(test$birdTrip))
    test$nPoints <- tripDurs$Freq[match(test$birdTrip, tripDurs$Var1)] ## Nb of points per trips
    test$maxDist <- max(test$ColonyDist, na.rm = TRUE) ## Calculating maximum distance per trip
    
    test$propNA <- sum(test$nbNA)/length(test$nbNA) ## Proportion of interpolated NAs for each trip
      
    ## Calculation of the proportion of points over land
    ## Would require a appropriate shapefile for the calculation
    #pts <- SpatialPointsDataFrame(test[,c("Longitude", "Latitude")], test,proj4string=crdref)
    #pts$Land <- !is.na(over(pts, as(s, "SpatialPolygons")))
    #pts$Prop.Land <- mean(pts$Land)
    #test <- as.data.frame(pts)
    
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
  
      if ( Complete == TRUE ) { # get rid of incomplete trips
  alltrips <- subset(alltrips, alltrips$BegPoint == "colony")  
  alltrips <- subset(alltrips, alltrips$EndPoint == "colony")
    }
  
      if (!is.null(MinTripDur)) {
  alltrips <- subset(alltrips, alltrips$TripLength >= MinTripDur)
    }
  
  if (!is.null(filtNA)) {
  alltrips <- subset(alltrips, alltrips$propNA <= filtNA)
    }
  
    ## Adding some data
  alltrips$ring <- metafile$ring[match(alltrips$trackID, metafile$ID)]
  alltrips$colony <- metafile$colony[match(alltrips$trackID, metafile$ID)]
  alltrips$Nest <- metafile$NestID[match(alltrips$trackID, metafile$ID)]
  alltrips$Year <- metafile$year[match(alltrips$trackID, metafile$ID)]
  alltrips$species <- metafile$species[match(alltrips$trackID, metafile$ID)]
  
      if (nrow(alltrips) > 1){
  alltrips$ColLong <- CLong
  alltrips$ColLat <- CLat
      }
  
  allbirds.list[[i]] <- alltrips
  rm(alltrips)
  allbirds <- do.call(rbind, allbirds.list)
  cat("GPS file = ", i, "\n")
  
    }
    
  if(is.null(BuffColony)) {
    warning("No buffer has been drawn around the colony, meaning that trips have not been delineated. Resuming in one single track for each bird", call. = F)
      }
  
  if(splt == TRUE) { allbirds <- split(allbirds, allbirds$birdTrip)}
  
  return(allbirds)
  
}
