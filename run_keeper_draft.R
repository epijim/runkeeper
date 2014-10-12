# http://danielecook.com/how-to-plot-all-of-your-runkeeper-data/

####################################
### Variables to set      ##########
####################################
# Location of code folder
  data_location <- "~/funwithcode_local/runkeeper"
# Folder of data as run keeper gives it
  folder <- "runkeeper-data-export-35925704-2014-10-12-1323"
  num_locations <- 5
  
# packages
  library(plotKML)
  library(reshape) # rename
  library(plotKML)
  library(plyr)
  library(dplyr)
  library(fpc)
  library(mapproj)
  
# Get files
  # Set wd
  setwd(paste0(data_location,"/",folder))
  
  # GPX files downloaded from Runkeeper
  files <- dir(pattern = "\\.gpx")
  
  # Consolidate routes in one data frame
  index <- c()
  latitude <- c()
  longitude <- c()
  time <- c()
  file <- c()

  # Set up Counter
  c <- 1 

# 
  for (f in 1:length(files)) {
    curr_route <- readGPX(files[f])
    # Treat interrupted GPS paths as seperate routes (useful if you occasionally stop running..walk for a bit, and start again like I do.)
    for (i in curr_route$tracks[[1]]) {
      c <- c + 1
      location <- i
      file <- c(file,rep(files[f], dim(location)[1])) 
      index <- c(index, rep(c, dim(location)[1]))
      latitude <- c(latitude, location$lat)
      longitude <- c(longitude, location$lon)
      time <- c(time, location$time)
    }
  }
  routes <- data.frame(cbind(index, latitude, longitude, time,file))

# Fix data types
  routes$file <- as.character(routes$file)
  routes$latitude <- as.numeric(levels(routes$latitude)[routes$latitude])
  routes$longitude <- as.numeric(levels(routes$longitude)[routes$longitude])
  routes$time <- as.POSIXlt(as.character(routes$time))
  routes <- transform(routes, index = as.numeric(index))

# Load Meta Data
  meta_data <- read.csv("cardioActivities.csv", stringsAsFactors=FALSE)
  meta_data <- rename(meta_data, c("GPX.File" = "file"))

# Bind routes
  routes <- left_join(routes, meta_data, by="file") %.%
  arrange(index)


# Use this function specify activity color if you have multiple activities.
activity_color <- function(activity) {
  if (activity=="Cycling") {
    color = "#00000060"
  } else if (activity=="Hiking") {
    color = "#00000060"
  } else {
    color = "#0080ff60"
  }
  color
}

# Because the routes dataframe takes a while to generate for some folks - save it!
save(routes, file="routes.Rdata")
# Use to load as needed.
load("routes.Rdata")

# Identify clusters of points, which will correspond to locations you have run. For example,
# I have run in Boston, Iowa City, Chicago, and a few other cities. You will want to set the minimum krange
# to the number of cities you have run in (5 in my case).
  clusters <- pamk(routes[,c("latitude", "longitude")], krange=num_locations:2, diss=T, usepam=F)$pamobject$medoids

# Plot Everything
for (r in 1:max(row(clusters))) {
  print(r)
  lat_range <- clusters[r,][1] + rnorm(20, sd=0.1)
  lon_range <-clusters[r,][2] + rnorm(20, sd=0.1)
  setroutes <- filter(routes, (latitude > min(lat_range) & latitude < max(lat_range)),
                      longitude > min(lon_range) &  longitude < max(lon_range))
  
  routeIds <- unique(setroutes$index)
  
  # Albers projection
  locProj <- mapproject(setroutes$longitude, setroutes$latitude, "rectangular", par=38)
  setroutes$latproj <- locProj$x
  setroutes$lonproj <- locProj$y
  
  
  # Map the projected points
  pdf(sprintf("%s-all.pdf", r))
  
  plot(setroutes$latproj, setroutes$lonproj, type="n", asp=1, axes=FALSE, xlab="", ylab="")
  for (i in routeIds) {
    currRoute <- subset(setroutes, index==i)
    lines(currRoute$latproj, currRoute$lonproj, col=activity_color(currRoute$Type), lwd=0.4)
  }
  dev.off()
}
