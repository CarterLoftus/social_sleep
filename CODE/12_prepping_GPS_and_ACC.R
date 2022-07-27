
options( digits.secs = 6 )
library( data.table )
library( lubridate )
library( stringr )
library( hms )
library( ascii )
library( plyr )
library( tidyr )

lonlat_to_utm <- function( df, lon_name = 'lon', lat_name = 'lat', crs_utm = CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # this is for Panama: "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
)){
  library( sp )
  library( rgdal )
  crs_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  df_sp <- SpatialPointsDataFrame(coords = df[, c(lon_name, lat_name)], proj4string = crs_longlat, data = df)
  
  df_sp <- spTransform(df_sp, crs_utm)
  
  df <- as.data.frame(df_sp)
  
  names(df) [ names(df) == paste( lon_name, 1, sep = '.') ] <- 'x'
  names(df) [ names(df) == paste( lat_name, 1, sep = '.') ] <- 'y'
  
  return( df )
}


## this function standardizes a vector
stdize <- function( x ){
  
  return( ( x - mean( x, na.rm = T ) ) / sd( x, na.rm = T ) )
  
} 


## this function takes the sum of a vector ignoring NAs, but returns an NA if the vector only contains NAs
na.sum <- function( x ){
  
  if( sum( !is.na( x ) ) == 0 ){
    
    return( NA )
    
  }else{
    
    return( sum( x, na.rm = T ) )
  }
}



## load GPS/acc data
sens_dat <- fread( "DATA/gps_acc_data/Papio Anubis Mpala 2019.csv" )

sens_dat <- as.data.frame( sens_dat )

sort( unique( sens_dat$`tag-local-identifier` ) )

## remove tags that are on individuals that are not in the cliff group
meta_dat <- read.csv( "DATA/Collaring_data.csv" )

head( meta_dat )

sens_dat <- sens_dat[ sens_dat$`tag-local-identifier` %in% meta_dat$collar_id[ meta_dat$Group == 'Cliffs' ], ]

head( sens_dat )

sort( unique( sens_dat$`tag-local-identifier` ) )
### I guess 2448 (Etirr's collar never worked???)

unique( sens_dat$`sensor-type` ) # data frame contains both gps and acc data

names( sens_dat )

# turn the timestamp into a POSIX element
sens_dat$timestamp <- as.POSIXct( sens_dat$timestamp, tz = 'UTC' )

# create column for the local timestamp,in East Africa Time
sens_dat$local_timestamp <- sens_dat$timestamp + 3*60*60

### NOTE THAT WE ARE APPLYING THE TIME CORRECTION TO THE ACC AND GPS DATA, NOT THE TRACKLET DATA (because this makes it easier to visualize against the thermal videos and find relevant timestamps, if no time correction is applied to the thermal data)
sens_dat$corr_local_timestamp <- sens_dat$local_timestamp - 16

### visualize the data that we have

tag_names <- sort( unique( sens_dat$`tag-local-identifier` ) )

plot( 1, ylab = 'Tag ID', xlab = 'Timestamp', type = 'n', xlim = range( sens_dat$corr_local_timestamp ), ylim = c( 0.5, ( length( tag_names ) + 0.5 ) ), xaxt = 'n', yaxt = 'n' )

axis( 1, at = seq( min( sens_dat$corr_local_timestamp ), max( sens_dat$corr_local_timestamp ), by = '1 days' ), labels = as.Date( as.POSIXct( seq( min( sens_dat$corr_local_timestamp ), max( sens_dat$corr_local_timestamp ), by = "1 days" ), tz = 'UTC' ) ), las = 2 )
axis( 2, at = 1:length( tag_names ), labels = tag_names, las = 1 )

# this loop takes a long time to run and is only for visualization purposes. It can be skipped
for( i in 1:length( tag_names ) ){
  
  gps_tag <- sens_dat[ sens_dat$`sensor-type` == 'gps' & sens_dat$`tag-local-identifier` == tag_names[ i ], ]
  
  gps_tag <- gps_tag[ !is.na( gps_tag$`location-lat` ), ]
  
  
  acc_tag <- sens_dat[ sens_dat$`sensor-type` == 'acceleration' & sens_dat$`tag-local-identifier` == tag_names[ i ], ]
  
  sum( is.na( acc_tag$`eobs:acceleration-sampling-frequency-per-axis` ) )
  
  points( gps_tag$corr_local_timestamp, rep( i - 0.2 , length( gps_tag$corr_local_timestamp ) ), cex = 0.1, pch = 16, col = 'red' )
  
  points( acc_tag$corr_local_timestamp, rep( i + 0.2 , length( acc_tag$corr_local_timestamp ) ), cex = 0.1, pch = 16, col = 'blue' )
  
}

abline( h = seq( 0.5, length( tag_names ) + 0.5 , by = 1 ) )


# extract just the accelerometry data and the columns that are relevant for the accelerometry
acc_dat <- sens_dat[ sens_dat$`sensor-type` == "acceleration", c( 'tag-local-identifier', 'corr_local_timestamp', 'eobs:acceleration-axes', 'eobs:acceleration-sampling-frequency-per-axis', 'eobs:accelerations-raw', 'eobs:start-timestamp' ) ]

# rename the columns
names( acc_dat ) <- c( 'tag', 'corr_local_timestamp', 'acc_axes', 'acc_samp_freq', 'acc', 'burst_start_time')

head( acc_dat )

write.csv( acc_dat, "DATA/gps_acc_data/acc_dat.csv", row.names = F )

# plot(as.numeric(as.factor(acc_dat$tag))~acc_dat$corr_local_timestamp,cex=0.3,pch=16,main="Overnight acceleromter bursts",xlab="",xaxt='n',yaxt='n',ylab="ID" )
# axis(2,at=1:length(unique(acc_dat$tag)),labels=sort(unique(acc_dat$tag)),las=1,cex=0.3)
# axis.POSIXct(1,at=seq(min(acc_dat$corr_local_timestamp),max(acc_dat$corr_local_timestamp),by="1 day"),las=2)
# # 6932 has burst happening more or less all day for the first several days of data because we set his collar to not collect high-res GPS for the first several days so that his pinger was able to ping and we could find the group for the rest of the capture period. Not collecting high-res GPS during the day = collecting ACC bursts for the entire day
# 

gps_dat <- sens_dat[ sens_dat$`sensor-type` == "gps", c( 'tag-local-identifier', 'corr_local_timestamp', 'location-long', 'location-lat', 'height-above-ellipsoid', 'eobs:start-timestamp' ) ]

names( gps_dat ) <- c( 'tag', 'corr_local_timestamp', 'lon', 'lat', 'height', 'burst_start_time')

gps_dat <- gps_dat[ !is.na( gps_dat$lon ) , ]

gps_dat <- lonlat_to_utm( gps_dat )

gps_dat$corr_local_timestamp <- round_date( gps_dat$corr_local_timestamp, unit = 'second' )

gps_dat$local_time <- str_split_fixed( gps_dat$corr_local_timestamp, ' ', 2  )[ , 2 ]

head( gps_dat )

# plot(as.numeric(as.factor(gps_dat$tag))~gps_dat$corr_local_timestamp,cex=0.3,pch=16,main="Overnight acceleromter bursts",xlab="",xaxt='n',yaxt='n',ylab="ID" )
# axis(2,at=1:length(unique(gps_dat$tag)),labels=sort(unique(gps_dat$tag)),las=1,cex=0.3)
# axis.POSIXct(1,at=seq(min(gps_dat$corr_local_timestamp),max(gps_dat$corr_local_timestamp),by="1 day"),las=2)

gps_dat$day <- str_split_fixed( gps_dat$corr_local_timestamp, ' ', 2 )[ , 1 ]


# write the GPS data to a dataframe. We will remove it from the memory to clear up space for the acc calculations
write.csv( gps_dat, "DATA/gps_acc_data/gps_dat.csv", row.names = F )

