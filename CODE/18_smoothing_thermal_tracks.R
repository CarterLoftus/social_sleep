

library( plyr )
library( stringr )
library( hms )
library( data.table )
library( lubridate )
library( doParallel )
library( foreach )

na_sum <- function( x ){
  
  if( sum( !is.na( x ) ) == 0 ){
    
    return( NA )
    
  }else{
    
    return( sum( x, na.rm = T ) )
  }
}

input_data <- 'uncorrected'

files_to_merge <- list.files( paste( "DATA/thermal_tracks/spatially_corrected_tracks", input_data, sep = '/' ), full.names = T )

list_of_dfs <- lapply( files_to_merge, FUN = readRDS )

rem_thresh <- 0 # tracks shorter duration than this number of seconds will be removed 

dir.create( paste( getwd(), 'DATA/thermal_tracks/smooth_tracks', sep = '/' ) )
dir.create( paste( getwd(), 'DATA/thermal_tracks/smooth_tracks', input_data, sep = '/' ) )



stopImplicitCluster()

if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  registerDoParallel( length( list_of_dfs ) )
  
}else{
  
  registerDoParallel( 4 )
  
}

foreach( d = 1:length( list_of_dfs ), .packages = .packages() ) %dopar% {
  
  #for( d in 1:length( list_of_dfs ) ){
  
  print( d ) 
  
  df <- list_of_dfs[[ d ]]
  
  names( df )[ names( df ) == 'unique_ID' ] <- 'id'
  
  df$local_timestamp <- as.POSIXct( df$local_timestamp, tz = 'UTC' )
  
  df$night <- as.Date( df$local_timestamp - 12*60*60 )
  
  head( df )
  
  df$unique_id <- paste( strftime( df$night, format = '%m%d' ), df$id, sep = '_' )
  
  ### remove tracks that are short ###
  # start_times <- aggregate( df$local_timestamp, by = list( df$unique_id ), FUN = min )
  # 
  # names( start_times ) <- c( 'id', 'start' )
  # 
  # end_times <- aggregate( df$local_timestamp, by = list( df$unique_id ), FUN = max )
  # 
  # names( end_times ) <- c( 'id', 'end' )
  # 
  # dur_dat <- merge( x = start_times, y = end_times, by = c( 'id' ), all = T )
  # 
  # dur_dat$duration <- as.numeric( dur_dat$end - dur_dat$start, unit = 'secs' )
  # 
  # ids_to_keep <- dur_dat$id[ dur_dat$duration >= rem_thresh ]
  # 
  # df <- df[ df$unique_id %in% ids_to_keep, ]
  # 
  ### smooth the tracks. Take the mean x and mean y location for every second
  
  df$sec_timestamp <- round_date( df$local_timestamp, unit = 'second' ) # make a column of timestamps rounded to the nearest second
  
  smooth_tracks <- aggregate( df[ , c( 'x_final', 'y_final' ) ], by = list( df$unique_id, df$sec_timestamp ), FUN = mean, na.rm = T ) # take the mean of the x and y locations for each individual and each second
  
  ## I switch from calling the unique_id's to just id's here
  names( smooth_tracks )[ 1:2 ] <- c( 'id', 'local_timestamp' ) # rename the columns
  
  smooth_tracks <- smooth_tracks[ order( smooth_tracks$id ), ] # order the tracks by unique identifier
  
  smooth_tracks$speed <- NA # create a column for speed
  
  track_ids <- as.character( unique( smooth_tracks$id ) ) # save a vector of the unique identifiers of each track
  
  ## speed is in meters per second (sub in df for smooth_tracks if you want to find the speed in the original tracks)
  
  for( track in track_ids ){ # for each unique tracklet...
    
    id_dat <- smooth_tracks[ smooth_tracks$id == track, ] # subset the data to this individual track
    
    id_dat$speed <- c( NA, sqrt( diff( id_dat$x_final )**2 + diff( id_dat$y_final )**2 ) / as.numeric( diff( id_dat$local_timestamp ), units = 'secs' ) ) # calculate the speed
    
    smooth_tracks[ smooth_tracks$id == track, ] <- id_dat # add this individual track data back into the full data frame
    
  }
  
  smooth_tracks$night <- as.character( unique( df$night ) )
  
  smooth_tracks$local_time <- str_split_fixed( smooth_tracks$local_timestamp, ' ', 2 )[ , 2 ]
  
  night_to_save <- unique( smooth_tracks$night )
  
  night_to_save <- gsub( '-', '', night_to_save )
  
  saveRDS( smooth_tracks, paste0( 'DATA/thermal_tracks/smooth_tracks/', input_data, '/', night_to_save, '_smooth_tracks.rds' ) )
  
}


stopImplicitCluster()


