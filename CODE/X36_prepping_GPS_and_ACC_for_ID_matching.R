
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


## this function takes the rolling median but pads the ends with NAs so that the output vector is the same length as the input vector
pad_roll_med <- function( x, k ){
  
  library( zoo )
  
  return( c( rep( NA, floor( k/2 ) ), rollmedian( x, k, na.rm = T ), rep( NA, floor( k/2 ) ) ) )
  
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


dy_acc <- function(vect, win_size = 5){
  
  pad_size <- win_size/2 - 0.5
  
  padded <- unlist( c(rep(NA, pad_size), vect, rep(NA, pad_size)) )
  acc_vec <- rep(NA, length = length( vect ) )
  
  ## sliding window
  for(i in 1:length(vect)){
    win <- padded[i:(i+(2*pad_size))] ## subset the window
    m_ave <- mean( win ) ## take the average over the window
    acc_comp <- vect[ i ] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i] <- acc_comp 
  }
  
  return( unlist( acc_vec) )
}



dy_acc_cont <- function( vect, win_size = win_size_cont ){
  
  pad_size <- win_size/2 - 0.5
  
  acc_vec <- rep( NA, ( length( vect ) - 2*pad_size ) )
  
  ## sliding window
  for( i in ( pad_size + 1 ): ( length( vect ) -  pad_size ) ) {
    
    win <- vect[ ( i - pad_size ) : ( i+ pad_size ) ] ## subset the window
    m_ave <- mean( win ) ## take the average over the window
    acc_comp <- vect[ i ] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i - pad_size ] <- acc_comp 
  }
  
  return( unlist( acc_vec) )
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

# plot(as.numeric(as.factor(acc_dat$tag))~acc_dat$corr_local_timestamp,cex=0.3,pch=16,main="Overnight acceleromter bursts",xlab="",xaxt='n',yaxt='n',ylab="ID" )
# axis(2,at=1:length(unique(acc_dat$tag)),labels=sort(unique(acc_dat$tag)),las=1,cex=0.3)
# axis.POSIXct(1,at=seq(min(acc_dat$corr_local_timestamp),max(acc_dat$corr_local_timestamp),by="1 day"),las=2)
# # 6932 has burst happening more or less all day for the first several days of data because we set his collar to not collect high-res GPS for the first several days so that his pinger was able to ping and we could find the group for the rest of the capture period. Not collecting high-res GPS during the day = collecting ACC bursts for the entire day
# 
# gps_dat <- sens_dat[ sens_dat$`sensor-type` == "gps", c( 'tag-local-identifier', 'corr_local_timestamp', 'location-long', 'location-lat', 'height-above-ellipsoid', 'eobs:start-timestamp' ) ]
# 
# names( gps_dat ) <- c( 'tag', 'corr_local_timestamp', 'lon', 'lat', 'height', 'burst_start_time')
# 
# gps_dat <- gps_dat[ !is.na( gps_dat$lon ) , ]
# 
# gps_dat <- lonlat_to_utm( gps_dat )
# 
# gps_dat$corr_local_timestamp <- round_date( gps_dat$corr_local_timestamp, unit = 'second' )
# 
# gps_dat$local_time <- str_split_fixed( gps_dat$corr_local_timestamp, ' ', 2  )[ , 2 ]
# 
# head( gps_dat )
# 
# # plot(as.numeric(as.factor(gps_dat$tag))~gps_dat$corr_local_timestamp,cex=0.3,pch=16,main="Overnight acceleromter bursts",xlab="",xaxt='n',yaxt='n',ylab="ID" )
# # axis(2,at=1:length(unique(gps_dat$tag)),labels=sort(unique(gps_dat$tag)),las=1,cex=0.3)
# # axis.POSIXct(1,at=seq(min(gps_dat$corr_local_timestamp),max(gps_dat$corr_local_timestamp),by="1 day"),las=2)
# 
# ## spatially discretize the GPS data
# min( gps_dat$local_time )
# max( gps_dat$local_time )
# 
# gps_dat$day <- str_split_fixed( gps_dat$corr_local_timestamp, ' ', 2 )[ , 1 ]
# 
# gps_dat$spat.disc.dist <- NA
# 
# gps_dat$x_disc <- gps_dat$x
# 
# gps_dat$y_disc <- gps_dat$y
# 
# n <- 5 ### distance threshold to spatially discretize at
# 
# ids <- unique( gps_dat$tag )
# 
# for( id in ids ){
# 
#   print( id )
# 
#   id_dat <- gps_dat[gps_dat$tag == id,] ## subset the data for the given group
# 
#   ## for each day of the individual's data...
# 
#   for( d in unique( id_dat$day ) ){    ## for each day of the individual's data...
# 
#     print( d )
# 
#     ## subset the data to just that day
#     dayDat <- id_dat[ id_dat$day == d, ]
# 
#     ## instantiate the column for step lengths (redundant)
#     dayDat$spat.disc.dist <- NA
# 
# 
#     temp.x<-dayDat$x[ 1 ] ## set their first location to the first temp location
#     temp.y<-dayDat$y[ 1 ]
# 
#     temp.count <- 1
# 
#     dist <- 0
# 
#     i <- 0 ## set counter to 0
# 
#     totalDist <- 0 ## set step length distance to 0
# 
#     while(i <= nrow(dayDat)){
# 
#       while( dist < n ) {
# 
#         if(i == nrow(dayDat)){
#           break
# 
#         }
# 
#         if(i == 0 || i == temp.count){
# 
#           i <- i+1
# 
#           dist <- sqrt((dayDat$x[i]-temp.x)**2 + (dayDat$y[i]-temp.y)**2)
# 
#         }else{
# 
#           dayDat$spat.disc.dist[i] <- ifelse( is.na( dist ), NA, 0 )
#           dayDat$x_disc[i] <- dayDat$x[temp.count]
#           dayDat$y_disc[i] <- dayDat$y[temp.count]
#           i<-i+1
#           dist<-sqrt((dayDat$x[i]-temp.x)**2 + (dayDat$y[i]-temp.y)**2)
# 
#         }
#       }
# 
#       if(dist < n){
#         dayDat$spat.disc.dist[i] <- 0
#         break
# 
#       }else{
# 
#         dayDat$spat.disc.dist[i] <- dist
#         temp.x<-dayDat$x[i]
#         temp.y<-dayDat$y[i]
#         temp.count <- i
#         dist <- 0
# 
#       }
#     }
# 
#     id_dat[id_dat$day == d,] <- dayDat
# 
#   }
# 
#   gps_dat[ gps_dat$tag == id, ] <- id_dat
# }
# 
# 
# head( gps_dat )
# 
# # write the GPS data to a dataframe. We will remove it from the memory to clear up space for the acc calculations
# write.csv( gps_dat, "DATA/gps_acc_data/gps_dat.csv", row.names = F )
# 
# 
# 
# rm( gps_dat )

rm( sens_dat )


####### prep the ACC data ##############

unique( acc_dat$tag )

unique( acc_dat[ startsWith( as.character( acc_dat$tag ), "6" ) , "acc_samp_freq" ] ) # 6000s collars always have a sampling frequency of 20 Hz per axis (remember that the 6000s collars only have burst ACC -- no continuous)

# create column for the hms time
acc_dat$local_time <- str_split_fixed( acc_dat$corr_local_timestamp, " ", 2 )[ , 2 ]

## look at the tiems of day at which the bursts are taken for 6000s collars and 2000s collars
hist( as.numeric( hms::as_hms( acc_dat[ startsWith( as.character( acc_dat$tag ), "6" ) , "local_time" ] ) ) )

hist( as.numeric( hms::as_hms( acc_dat[ startsWith( as.character( acc_dat$tag ), "2" ) , "local_time" ] ) ) )

## 6000s collars take most of their acc at night (with exceptions because some the collars had their settings changed half way through the study to stop taking high resolution GPS (and thus start taking ACC bursts) in mid-morning, so their batteries would last longer)
## 2000s collars take most of their acc during the day (they take the same amount at night as the 6000s collars, but they take the majority during the day because they sample continuously during the day)

# make sure that the data is ordered by tag and timestamp
acc_dat <- acc_dat[ order( acc_dat$corr_local_timestamp ), ]
acc_dat <- acc_dat[ order( acc_dat$tag ), ]

## I am going to add a unique burst ID to each acc burst. Becasue the 6000s collars take 2 seconds worth of acc for each burst which I split into two seconds of log VeDBA. But I want to know which seconds resulted from the same burst, so I can either drop the 2nd second, or condense the two seconds, if need be
acc_dat$burst_id <- 1:nrow( acc_dat )

# extract the data from the 6000s collars
six_dat <- acc_dat[ startsWith( as.character( acc_dat$tag ), "6" ), ]

head( six_dat )

# save the time interveal between each burst
time_steps <- as.numeric( diff( six_dat$corr_local_timestamp ), units = 'secs' )

# remove the negative time intervals, which reflect moving from one individual's data to the next in the dataframe
time_steps <- time_steps[ time_steps > 0 ]

hist( time_steps[ time_steps < 400 ] ) ## most of the acc bursts are separated by 60 seconds (i.e. the burst is taken every minute on the minute), but many are also taken at 5 second intervals
hist( time_steps[ time_steps < 30 ] ) ## confirming the 5 second interval

## when do these 5 second interval bursts happen?

hist( six_dat$corr_local_timestamp[ which( time_steps < 7 ) + 1 ], breaks = 20 )

## 5 second intervals happen mostly starting on August 15. This is consistent with my notes, which confirm that we changed the settings of the D cell 6000s collars on August 15th to take accelerometry every 5 seconds to help with night-time identification

# split the acc bursts so that each individual value for each axis has is in its own column
d2 <- as.data.frame(str_split(six_dat$acc, " ", simplify = T))
ncol( d2 )
## the 6000s collars take 40 samples per axis in each burst. Because the sampling frequency is 20 Hz per axis, this means the collars take 2 second long bursts

# convert the values to numeric
for(i in 1:ncol(d2)){
  d2[,i] <- as.numeric(as.character((d2[,i])))
}

# names the column according according to which axis the value belongs to, and the number of the value from the start of the burst (per axis)
names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

# add the timestamp and the tag information to this dataframe, although it is not cmopletely necessary
d2$corr_local_timestamp <- six_dat$corr_local_timestamp
d2$tag <- six_dat$tag

# extract a dataframe of just the x-axis accelerations
x_d <- d2[,grepl('x',names(d2))]
head(x_d)

# extract a dataframe of just the y-axis accelerations
y_d <- d2[,grepl('y',names(d2))]
head(y_d)

# extract a dataframe of just the z-axis accelerations
z_d <- d2[,grepl('z',names(d2))]
head(z_d)

## calculate the VeDBA at each timestamp that an ACC sample is collected. Win_size is set to 11 because that is just over half a second with a sampling rate of 20 Hz
ved_mat <- sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 11 ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 11 ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 11 ) ) )**2)

## because the acc burst last for two seconds, we will calculate an average VeDBA across each second of each burst. So let's make one dataframe for the first second of the burst and another frame for the second second of the burst
six_dat_dup <- six_dat

## add a second to the timestamps of the first dataframe -- this will be our dataframe for the 2nd second of each burst
six_dat_dup$corr_local_timestamp <- six_dat$corr_local_timestamp + 1

# calculate the mean VeDBA across each second of each burst
six_dat$ave_vedba <- apply( ved_mat[ 1:20, ], 2, FUN = mean, na.rm = T )

# calculate the mean of the x, y, and z axes
six_dat$x_mean <- apply( x_d[ , 1:20 ], 1, FUN = mean )
six_dat$y_mean <- apply( y_d[ , 1:20 ], 1, FUN = mean )
six_dat$z_mean <- apply( z_d[ , 1:20 ], 1, FUN = mean )


six_dat_dup$ave_vedba <- apply( ved_mat[ 21:40, ], 2, FUN = mean, na.rm = T )

six_dat_dup$x_mean <- apply( x_d[ , 21:40 ], 1, FUN = mean )
six_dat_dup$y_mean <- apply( y_d[ , 21:40 ], 1, FUN = mean )
six_dat_dup$z_mean <- apply( z_d[ , 21:40 ], 1, FUN = mean )

# combine the dataframes for the first and second second of each burst
six_dat_full <- rbind( six_dat, six_dat_dup )

# reorder the dataframe by tag and timestamp
six_dat_full <- six_dat_full[ order( six_dat_full$corr_local_timestamp ), ]
six_dat_full <- six_dat_full[ order( six_dat_full$tag ), ]

# recreate the column for local time (it already exists, but it currently reflects the local time of the start of the burst, not of the actual second that the row refers to within the burst. We want to change it so it matches the local timestamp column)
six_dat_full$local_time <- str_split_fixed( six_dat_full$corr_local_timestamp, ' ', 2 )[ , 2 ]
head( six_dat_full )

## now if you want to get rid of the 2nd second of a burst, you just make sure the rows of the dataframe are ordered by timestamp, and remove duplicate burst_id's

# six_dat$ave_vedba <- apply( sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 11 ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 11 ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 11 ) ) )**2) , 2, FUN = mean, na.rm = T ) ## by removing the outer apply function, you could get the real VeDBA for each timestamp at which the ACC samples (i.e. subsecond VeDBAs without averaging, but I am not convinced that it will be more helpful than the VeDBA averaged over each second burst)

# take the log of the average vedba (this might not be completely necessary here, but typically creates more separation between stationary and non-stationary behavior)
six_dat_full$log_vedba <- log( six_dat_full$ave_vedba )

summary( six_dat_full$log_vedba )

### calculate the log of the average VeDBA for each second for the 2000s collars as well

# first extract the data from the 2000s collars
two_dat <- acc_dat[ startsWith( as.character( acc_dat$tag ), "2" ), ]

# extract the continuous acc data from the 2000s collars (this is the data collected during the day)
two_dat_day <- two_dat[ two_dat$acc_samp_freq == 12, ]

# extract the burst acc data from the 2000s collars (this is the data collected during the night)
two_dat_night <- two_dat[ two_dat$acc_samp_freq != 12, ]

## first calculate the average VeDBA over each nighttime burst, which lasts for 0.71 seconds (inspected using the Movebank acceleration viewer, and confirmed by dividing the 40 samples per axis by the 56.23 sampling rate per axis)
d2 <- as.data.frame(str_split(two_dat_night$acc, " ", simplify = T))
ncol( d2 )
## the 2000s collars take 40 samples per axis in each burst. Because the sampling frequency is 56.23 Hz per axis, this means the collars take 0.71 second long bursts

# if 40 samples = 0.71 seconds, how many samples = 0.5 seconds? (i.e. 40/0.71 = x/0.5 )?

( 0.5*40 ) / 0.71 # answer to above question is 28.17. So use 29 samples for the sliding time window for the VeDBA calculation

# turn the acc values into numeric
for(i in 1:ncol(d2)){
  d2[,i] <- as.numeric(as.character((d2[,i])))
}

## see above for commenting of this section, as it is the same as what happens above
names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

d2corr_local_timestamp <- two_dat_night$corr_local_timestamp
d2$tag <- two_dat_night$tag

x_d <- d2[,grepl('x',names(d2))]
head(x_d)

y_d <- d2[,grepl('y',names(d2))]
head(y_d)

z_d <- d2[,grepl('z',names(d2))]
head(z_d)

## calculate the VeDBA at each timestamp that an ACC sample is collected
ved_mat <- sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 29 ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 29 ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc( x, win_size = 29 ) ) )**2)

two_dat_night$ave_vedba <- apply( ved_mat, 2, FUN = mean, na.rm = T )

two_dat_night$x_mean <- apply( x_d[ , 1:20 ], 1, FUN = mean )
two_dat_night$y_mean <- apply( y_d[ , 1:20 ], 1, FUN = mean )
two_dat_night$z_mean <- apply( z_d[ , 1:20 ], 1, FUN = mean )

two_dat_night$log_vedba <- log( two_dat_night$ave_vedba )

summary( two_dat_night$log_vedba )


### now calculating log VeDBA for the daytime ACC data

win_size_cont <- 7 ## there are 12 samples per second, so a moving window of 0.5 seconds would encompass 6 samples. But the window size has to be odd so we will make it 7
pad_size <- win_size_cont/2 - 0.5 # this is the number of values (or NAs) that we need before our first and after our last timepoint for which we are calculating VeDBA

two_dat_day$corr_local_timestamp <- round_date( two_dat_day$corr_local_timestamp, unit = 'second' ) # we need to round the timestamp to the nearest second because we are going to merge this with a schedule of all seconds within the study period below. Note that we did not round the timestamps of the previous subsets of the ACC data. We should do that later

head(two_dat_day)

min_time <- min( two_dat_day$corr_local_timestamp )
max_time <- max( two_dat_day$corr_local_timestamp )

min_hms <- min( two_dat_day$local_time ) ## there appears to be one second of 12 Hz acc taken at 00:17:34
two_dat_day[ two_dat_day$local_time == min_hms, ]

min_hms_2nd <- min( two_dat_day$local_time[ two_dat_day$local_time != min( two_dat_day$local_time ) ] )
two_dat_day[ two_dat_day$local_time <= min_hms_2nd, ] ## confirms that there is only one aberrant 12Hz ACC burst that is taken before 6:30 AM. So let's just remove it

two_dat_day <- two_dat_day[ two_dat_day$local_time >= min_hms_2nd, ]

min_hms <- min( two_dat_day$local_time )
max_hms <- max( two_dat_day$local_time )

# make a vector of every second in the study period
all_times <- seq( min_time, max_time, by = '1 sec' )

# remove seconds that occurred at night
all_times_trim <- all_times[ as.numeric( as_hms( all_times ) ) >= as.numeric( ( as_hms( min_hms ) - 1 ) ) & as.numeric( as_hms( all_times ) ) <= as.numeric( ( as_hms( max_hms ) + 1 ) ) ]

timestamp_df <- data.frame( corr_local_timestamp = all_times_trim )

## confirm that we cut out the nighttime timestamps
as_hms( min( as_hms( all_times_trim ) ) )
as_hms( min( as_hms( all_times_trim ) ) )

wide_day <- reshape2::dcast( two_dat_day, corr_local_timestamp ~ tag, value.var = 'acc'  )
## the warning in the function above indicates that there are two rows of acc for the same second (or that round to the same second) for one individual

## save the times where there is a duplicate ACC burst for an individual
dup_times <- wide_day[ which( wide_day == 2, arr.ind =  T )[ , 1 ], 'corr_local_timestamp' ]


potential_dups <- two_dat_day[ two_dat_day$corr_local_timestamp %in% ( dup_times ) & two_dat_day$tag == 2428, ]
potential_dups[ order( potential_dups$corr_local_timestamp ), ]

#sum( duplicated( two_dat_day[ , c( 'tag', 'corr_local_timestamp' ) ] ) ) ## this is a non-insignificant problem, with 4538 duplicated timestamps

## is this a problem in the original data?
#sum( duplicated( acc_dat[ , c( 'tag', 'corr_local_timestamp' ) ] ) )

rm( acc_dat ) # we don't need this anymore. We will rebuild it from the 2000s and 6000s acc dataframes

## I am just going to go with the easy fix and remove duplicates very simply
two_dat_day <- two_dat_day[ !duplicated( two_dat_day[ , c( 'tag', 'corr_local_timestamp' ) ] ), ]

## now try again converting to wide format
wide_day <- reshape2::dcast( two_dat_day, corr_local_timestamp ~ tag, value.var = 'acc'  )

wide_day_full <- merge( x = timestamp_df, y = wide_day, by = 'corr_local_timestamp', all.x = T, all.y = T, sort = F )

head( wide_day_full )

wide_day_full <- wide_day_full[ order( wide_day_full$corr_local_timestamp ), ]

wide_day_full[ is.na( wide_day_full ) ] <- paste( rep( 'NA', 36 ), collapse = ' ' )

extract_front_pad <- function( x ) paste( str_split( x, pattern = ' ', simplify = T )[ , ( ncol( str_split( x, pattern = ' ', simplify = T ) ) - ( pad_size * 3 ) + 1 ) : ncol( str_split( x, pattern = ' ', simplify = T ) ) ], collapse =  ' ' )

front_pad <- apply( wide_day_full[ , - 1 ], c( 1, 2 ), extract_front_pad )

matrix( paste( rep( 'NA', pad_size*3 ), collapse = ' ' ), nrow = 1, ncol = ncol( front_pad ) )

front_pad <- rbind( matrix( paste( rep( 'NA', pad_size*3 ), collapse = ' ' ), nrow = 1, ncol = ncol( front_pad ) ),   front_pad[ - nrow( front_pad ), ] )


extract_back_pad <- function( x ) paste( str_split( x, pattern = ' ', simplify = T )[ , 1 : ( pad_size * 3 ) ], collapse =  ' ' )

back_pad <- apply( wide_day_full[ , - 1 ], c( 1, 2 ), extract_back_pad )

matrix( paste( rep( 'NA', pad_size*3 ), collapse = ' ' ), nrow = 1, ncol = ncol( back_pad ) )

back_pad <- rbind( back_pad[ -1, ], matrix( paste( rep( 'NA', pad_size*3 ), collapse = ' ' ), nrow = 1, ncol = ncol( back_pad ) ) )

full_padded_acc <- paste.matrix( front_pad, wide_day_full[ , -1 ], back_pad )

full_padded_acc <- as.data.frame( full_padded_acc )

names( full_padded_acc ) <- names( wide_day_full )[ - 1 ]

full_padded_acc$corr_local_timestamp <- wide_day_full$corr_local_timestamp

padded_acc_long <- gather_( full_padded_acc, 'tag', 'acc', names( wide_day_full )[ -1 ]  )

head( padded_acc_long )

padded_acc_for_ved <- padded_acc_long[ padded_acc_long$acc != paste( rep( NA,  ncol( str_split( padded_acc_long[ 1, ]$acc, ' ', simplify =  T ) ) ), collapse = ' ' ),  ]

write.csv( padded_acc_for_ved, 'DATA/gps_acc_data/padded_acc_for_ved.csv', row.names = F )

## now that the data is padded appropriately to allow a true sliding window technique (so that samples from one second can be included in the sliding window for samples from the next second or previous second), we can calculate the vedba

d2 <- as.data.frame( str_split( padded_acc_for_ved$acc, " ", simplify = T))
ncol( d2 )
## the 2000s collars take 12 samples per axis per second. This data frame has 54, not 36 (3*12) columns because it is already padded for the VeDBA calculation

# Because the following loop is coercing each column to numeric, and some of the columns have NAs in them because of the padding mechanism, the loop with throw a warning, that NAs were introduced by coercion. This is because the character string, "NA", cannot be coerced to a numeric. But this works out just fine, because it gets coerced to an NA, which is what we wanted anyway. There are not warnings when we process the bursts in this way because we don't pad the bursts, and so there are not NAs

for(i in 1:ncol(d2)){
  d2[,i] <- as.numeric(as.character((d2[,i])))
}

names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

d2$corr_local_timestamp <- padded_acc_for_ved$corr_local_timestamp
d2$tag <- padded_acc_for_ved$tag

x_d <- d2[,grepl('x',names(d2))]
head(x_d)

y_d <- d2[,grepl('y',names(d2))]
head(y_d)

z_d <- d2[,grepl('z',names(d2))]
head(z_d)

## calculate the VeDBA at each timestamp that an ACC sample is collected. Win_size is set to 7 because that is just over half a second with a sampling rate of 12 Hz
ved_mat <- sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc_cont( x, win_size = win_size_cont ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc_cont( x, win_size = win_size_cont ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc_cont( x, win_size = win_size_cont ) ) )**2)


padded_acc_for_ved$ave_vedba <- apply( ved_mat, 2, FUN = mean, na.rm = T )

padded_acc_for_ved$x_mean <- apply( x_d, 1, function( x ) mean( x[ ( pad_size + 1 ): ( ncol( x_d ) - pad_size ) ] ) )
padded_acc_for_ved$y_mean <- apply( y_d, 1, function( x ) mean( x[ ( pad_size + 1 ): ( ncol( y_d ) - pad_size ) ] ) )
padded_acc_for_ved$z_mean <- apply( z_d, 1, function( x ) mean( x[ ( pad_size + 1 ): ( ncol( z_d ) - pad_size ) ] ) )

padded_acc_for_ved$log_vedba <- log( padded_acc_for_ved$ave_vedba )

summary( padded_acc_for_ved$log_vedba )

two_dat_day_ved <- merge( x = two_dat_day, y = padded_acc_for_ved[ , c( 'tag', 'corr_local_timestamp', 'ave_vedba', 'x_mean', 'y_mean', 'z_mean', 'log_vedba' ) ], by = c( 'tag', 'corr_local_timestamp' ), all.x = T, all.y = F, sort = F )

## reorder the columns of the data frame to match the order of the columns of the other vedba dataframes so that we can merge them all
two_dat_day_ved <- two_dat_day_ved[ , names( two_dat_night ) ]

full_ved <- rbind( six_dat_full, two_dat_night, two_dat_day_ved )

full_ved <- full_ved[ order( full_ved$corr_local_timestamp ), ]
full_ved <- full_ved[ order( full_ved$tag ), ]

write.csv( full_ved, 'DATA/gps_acc_data/full_ved.csv', row.names = F )

## cut out all data during the day, that we won't be using to ID tracks

night_ved <- full_ved[ ( full_ved$local_time > '15:00:00' ) | ( full_ved$local_time < '09:30:00' ), ]

write.csv( night_ved, 'DATA/gps_acc_data/night_ved.csv', row.names = F )

## clear the environment. IF YOU RUN THIS NEXT LINE, YOU HAVE TO RERUN THE FUNCTIONS ABOVE
rm( list = ls( ) )

full_ved <- fread( 'DATA/gps_acc_data/full_ved.csv' )

full_ved <- as.data.frame( full_ved )

full_ved$corr_local_timestamp <- as.POSIXct( full_ved$corr_local_timestamp, tz = "UTC" )

full_ved$corr_local_timestamp <- round_date( full_ved$corr_local_timestamp, unit = 'second' )

## now put the ACC into wide form, smoothing the pieces that are continuous acc

## if you don't want to smooth the acc, just run this line below and then skip down to ordering ACC wide below
#vedba_wide <- reshape2::dcast( full_ved, corr_local_timestamp ~ tag, value.var = 'log_vedba'  )


## we are going to do everything from this point forward both with both seconds of the 6000s acc bursts represented, and with one second of the 6000s acc burst represented. On one hand, it is valuable information that shouldn't be thrown away (when comparing a tag to tracklets), but it makes it hard to compare a tracklet to many tags, if they have different amounts of data (6000s have 2 seconds of vedba every minute, and 2000s only have 1)

## make sure that the acc data is ordered by tag and timestamp
full_ved <- full_ved[ order( full_ved$corr_local_timestamp ), ]
full_ved <- full_ved[ order( full_ved$tag ), ]

full_ved_one_sec_burst <- full_ved[ !duplicated( full_ved$burst_id ), ]

## separate the data into
burst_acc <- full_ved[ full_ved$acc_samp_freq != 12, ]
burst_acc_one_sec <- full_ved_one_sec_burst[ full_ved_one_sec_burst$acc_samp_freq != 12, ]

## the continuous acc is not affected by the two second bursts of the 6000s collars, so we don't have to deal with that here
cont_acc <- full_ved[ full_ved$acc_samp_freq == 12, ]

cont_wide_temp <- reshape2::dcast( cont_acc, corr_local_timestamp ~ tag, value.var = 'log_vedba'  )

## now we will make sure all the timestamps are represented in the wide dataframe. Otherwise, the rolling median can be inaccurately calculated
min_time <- min( cont_wide_temp$corr_local_timestamp )
max_time <- max( cont_wide_temp$corr_local_timestamp )

hms_times <- str_split_fixed( cont_wide_temp$corr_local_timestamp, " ", 2 )[ , 2 ]

min_hms <- min( hms_times )
max_hms <- max( hms_times )

# make a vector of every second in the study period
all_times <- seq( min_time, max_time, by = '1 sec' )

# remove seconds that occurred at night
all_times_trim <- all_times[ as.numeric( as_hms( all_times ) ) >= as.numeric( ( as_hms( min_hms ) - 1 ) ) & as.numeric( as_hms( all_times ) ) <= as.numeric( ( as_hms( max_hms ) + 1 ) ) ]

all_times_trim_df <- data.frame( corr_local_timestamp = all_times_trim )

cont_wide <- merge( x = all_times_trim_df, y = cont_wide_temp, by = 'corr_local_timestamp', all.x = T, all.y = T, sort = F )
cont_wide <- cont_wide[ order( cont_wide$corr_local_timestamp ), ]

# remove the timestamp column. We will add it back on after smoothing the VeDBAs with the rolling median
acc_tms <- cont_wide$corr_local_timestamp

cont_wide <- cont_wide[ , names( cont_wide ) != 'corr_local_timestamp' ]

## create a matrix of the smoothed log_Vedba (using a rolling median)
smoothing_window <- 5

cont_vedba_smoothed <- apply( cont_wide, MARGIN = 2, FUN = pad_roll_med, k = smoothing_window )

cont_vedba_smoothed <- as.data.frame( cont_vedba_smoothed )

cont_vedba_smoothed$corr_local_timestamp <- acc_tms


burst_vedba <- reshape2::dcast( burst_acc, corr_local_timestamp ~ tag, value.var = 'log_vedba'  )

burst_vedba_one_sec <- reshape2::dcast( burst_acc_one_sec, corr_local_timestamp ~ tag, value.var = 'log_vedba'  )

write.csv( cont_vedba_smoothed, "DATA/gps_acc_data/cont_vedba_smoothed.csv", row.names = F )

write.csv( burst_vedba, "DATA/gps_acc_data/burst_vedba.csv", row.names = F )

write.csv( burst_vedba_one_sec, "DATA/gps_acc_data/burst_vedba_one_sec.csv", row.names = F )


rm( list = ls( ) ) ## again, run functions abvoe!!

## the following lines of code are for recombining the smoothed continuous VeDBA and the burst VeDBA. But I actually decided to keep these separate. I think it might cause problems to standardize the 2000s collars VeDBAs (with their burst and continuous data) and then compare that to standardized 6000s collars which only have bursts -- the distributions of their VeDBAs are likely fundamentally different because the continuous VeDBA taken during the day reflects a lot of active behavior. So I will standardize the continuous VeDBAs to compare them against each other, and then separately standardize the burst VeDBAs to compare them against each other.
# missing_cols <- names( burst_wide )[ !names( burst_wide ) %in% names( vedba_smoothed ) ]
# 
# for( col_name in missing_cols ){
#   
#   vedba_smoothed[ , col_name ] <- NA
#   
# }
# 
# ## make sure the column names are in the same order in the burst data and in the smoothed continuous data
# vedba_smoothed <- vedba_smoothed[ , names( burst_wide ) ]
# 
# vedba_wide <- rbind( vedba_smoothed, burst_wide )
# 
# vedba_wide <- vedba_wide[ order( vedba_wide$corr_local_timestamp ), ]

######## prep the GPS tracks #############

gps_dat <- fread( "DATA/gps_acc_data/gps_dat.csv" )

gps_dat <- as.data.frame( gps_dat )

gps_dat$corr_local_timestamp <- as.POSIXct( gps_dat$corr_local_timestamp, tz = "UTC" )

gps_dat$corr_local_timestamp <- round_date( gps_dat$corr_local_timestamp, unit = 'second' )

### remove the non 1 Hz GPS data

# first find where the data is not 1 Hz
gps_night_dat <- gps_dat[ which( as.numeric( diff( gps_dat$burst_start_time ), unit = 'secs' ) != 0 ) , ]

hist( gps_night_dat$corr_local_timestamp, breaks = 500 )

early_stud_period_night <- gps_night_dat[ as.Date( gps_night_dat$corr_local_timestamp ) < as.Date( '2019-08-05', tz = 'UTC' ),  ]

hist( as.numeric( as_hms( early_stud_period_night$local_time ) ), xaxt = 'n', breaks = 50 ) ## most of the non 1 Hz data starts at 18:00:00 and continues until 06:30:00 (at least up until 2019-08-05. After that date we changed the C cell 6000s collars to stop collecting 1 Hz data after the morning so that it would save battery)

axis( 1, at = seq( 0, 60*60*24, 60*60 ),  labels = as.character( as_hms ( seq( 0, 60*60*24, 60*60 ) ) ) )

# remove the gps data that is not 1 Hz. NOTE THAT THIS DOES NOT ACTUALLY REMOVE ALL NON-1Hz DATA!!! THERE ARE 6000s COLLARS (the C cells) THAT TOOK LOWER RESOLUTION DATA AFTER THE MORNING STARTING AROUND AUGUST 5TH 
gps_dat <- gps_dat[ gps_dat$local_time > '06:30:00' & gps_dat$local_time < '18:00:00', ]

# make a wide data frame with the x coordinates

xs_temp <- reshape2::dcast( gps_dat, corr_local_timestamp ~ tag, value.var = 'x'  )
ys_temp <- reshape2::dcast( gps_dat, corr_local_timestamp ~ tag, value.var = 'y'  )
identical( xs_temp$corr_local_timestamp, ys_temp$corr_local_timestamp )
identical( names( xs_temp ), names( ys_temp ) )

# make sure all timestamps are represented
all_times <- seq( min( gps_dat$corr_local_timestamp ), max( gps_dat$corr_local_timestamp ), by = '1 sec' )
all_hms <- str_split_fixed( all_times, ' ', 2 )[ , 2 ]
all_times <- all_times[  all_hms > '06:30:00' & all_hms < '18:00:00' ]
all_times_df <- data.frame( corr_local_timestamp = all_times )

xs <- merge( x = all_times_df, y = xs_temp, by = 'corr_local_timestamp', all.x = T, all.y = T, sort = F )
xs <- xs[ order( xs$corr_local_timestamp ), ]

ys <- merge( x = all_times_df, y = ys_temp, by = 'corr_local_timestamp', all.x = T, all.y = T, sort = F )
ys <- ys[ order( ys$corr_local_timestamp ), ]

tms <- xs$corr_local_timestamp

xs <- xs[ , names( xs ) != 'corr_local_timestamp' ]
ys <- ys[ , names( ys ) != 'corr_local_timestamp' ]

head( xs )

# calculate the speeds of the GPS tracks

## let's calculate speed the appropriate way (i.e. not across days) in case I use this code for another analysis, where it is important
days <- unique( as.Date( tms ) )

speeds <- matrix( nrow = nrow( xs ), ncol = ncol( xs ) )

# go through each day and calculate the speeds for that day
for( day in days ){
  
  # subset the location data to just the given day
  xs_day <- as.matrix( xs[ as.Date( tms ) == day, ] )
  
  ys_day <- as.matrix( ys[ as.Date( tms ) == day, ] )
  
  # calculate the speeds for this day
  day_speeds_temp <- sqrt( diff( xs_day )**2 + diff( ys_day )**2 ) / as.numeric( diff( tms[ as.Date( tms ) == day ] ), units = 'secs' ) ## find the speeds of each GPS track
  
  # confirm that all the time steps for the day's data are 1 second
  print( sum( round( as.numeric( diff( tms[ as.Date( tms ) == day ] ), units = 'secs' ) ) != 1 ) )
  
  day_speeds <- rbind( rep( NA, ncol = day_speeds_temp ), day_speeds_temp ) ## add a row of NAs at the top because we don't know the speed at the first timestamp
  
  # insert this day's data into the full speeds matrix
  speeds[ as.Date( tms ) == day, ] <- day_speeds
  
}

colnames( speeds ) <- names( xs )

## create a matrix of the smoothed speeds (using a rolling median) 
smoothing_window <- 7

speeds_smoothed <- apply( speeds, MARGIN = 2, FUN = pad_roll_med, k = smoothing_window )

speeds_smoothed <- as.data.frame( speeds_smoothed )

speeds_smoothed$corr_local_timestamp <- tms

write.csv( speeds_smoothed, "DATA/gps_acc_data/speeds_smoothed.csv", row.names = F )

