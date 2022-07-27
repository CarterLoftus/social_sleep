

library( data.table )
library( stringr )
library( plyr )
library( lubridate )

######## comparing focals to sleep algorithm ##########

## first pull in the focals from Loopy

options( digits.secs = 6 )

## load in the original focal follow observation data #####
loopy_files <- list.files(path = "DATA/thermal_focal_follows/loopy_focal_follows_2021_09_17/", pattern = ".csv", full.names = TRUE, recursive = TRUE)

loopy_dfs <- lapply(as.list( loopy_files ), read.csv)



## load in the original focal follow observation data #####
boris_files <- list.files(path = "DATA/thermal_focal_follows/boris_focal_follows_2022_04_05/", pattern = ".csv", full.names = TRUE, recursive = TRUE)

boris_dfs_raw <- lapply(as.list(boris_files), read.csv)

boris_dfs <- lapply( boris_dfs_raw, FUN = function( x ) x[ - c( 1:15 ), ] ) ## removes the first 15 rows that contain the metadata for Boris observations


## now make the boris observation data look like the loopy data
for( i in 1:length( boris_dfs ) ){
  
  df <- boris_dfs[[ i ]] 
  
  names( df ) <- c( 'Start', 'file', 'vid_duration', 'fps', 'Subject', 'Value', 'Behaviour', 'Modifiers', 'start_stop' )
  
  counter <- 1
  
  df$Stop <- NA
  
  while( counter < nrow( df ) ){
    
    curr_behav <- df$Value[ counter ]
    
    behav_inds <- which( df$Value == curr_behav )
    
    stop_ind <- min( behav_inds[ behav_inds > counter ] ) 
    
    if( df$start_stop[ stop_ind ] != 'STOP' ) stop( 'problem' )
    
    df$Stop[ counter ] <- df$Start[ stop_ind ]
    
    df <- df[ - stop_ind, ]
    
    counter <- counter + 1
  }
  
  df$Start_Frame <- round( as.numeric( df$Start )*as.numeric( df$fps ) )
  df$Stop_Frame <- round( as.numeric( df$Stop )*as.numeric( df$fps ) )
  
  file_split <- str_split( df$file, '/', simplify = T )
  
  file_name <- file_split[ , ncol( file_split ) ]
  
  df$Scoring <- str_split_fixed( file_name, pattern = '.mp4', 2 )[ , 1 ]
  
  boris_dfs[[ i ]] <- df
  
}


dfs <- c( loopy_dfs, boris_dfs )


## make a list of new dfs which are going to have the timestamps of behaviors based on the frame timestamps when they actually occur
new_dfs <- vector( mode = 'list', length = length( dfs ) )


for( i in 1:length( dfs ) ){
  
  temp_dat <- dfs[[ i ]] 
  
  temp_dat <- temp_dat[ temp_dat$Behaviour != 'tree', ]
  
  vid_name <- as.character( unique( temp_dat$Scoring ) )
  
  vid_date <- str_split_fixed( vid_name, '_', 2 )[ ,1 ]
  
  if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] != 'jcl273' ){
    
    ## read in frame information
    frame_info <- read.table( paste0( "Z:\\baboon\\archive\\processed\\video\\thermal\\2019_summer\\cliff_data\\mp4_files\\viewpoint_1\\T1020\\", vid_date, '\\', vid_name, '.txt' ) )
    
  }else{
    
    frame_info <- read.table( paste0( "RESULTS/tracking_output/viewpoint_1/uncorrected_tracks/", vid_date, '/', vid_name, '.txt' ) )
    
  }

  
  ## use the frame information to associate a timestamp with each frame
  names( frame_info ) <- c( "file", "frame_info" )
  
  frame_timestamps_temp <- str_split_fixed( frame_info$frame_info, "_", 2 )[, 2 ]
  
  frame_timestamps <- str_split_fixed( frame_timestamps_temp, ".tiff", 2 )[, 1 ]
  
  ## reformat to the typical timestamp format by parsing the string (using the format field of as.POSIX can get me part way there to the reformatting, but causes problems with the milliseconds)
  
  final_timestamps <- paste0( substring( frame_timestamps, 1, 4 ), "-", substring( frame_timestamps, 5, 6 ), "-", substring( frame_timestamps, 7, 8 ), " ", substring( frame_timestamps, 10, 11 ), ":", substring( frame_timestamps, 12, 13 ), ":", substring( frame_timestamps, 14, 15 ), ".", substring( frame_timestamps, 16, 21 ) )
  
  ## associate each frame number in the tracklets dataframe with the timestamp of that frame
  behav_start_timestamps <- final_timestamps[ ifelse( temp_dat$Start_Frame == length( final_timestamps ), temp_dat$Start_Frame, (temp_dat$Start_Frame + 1 ) ) ]
  
  behav_stop_timestamps <- final_timestamps[ ifelse( temp_dat$Stop_Frame == length( final_timestamps ), temp_dat$Stop_Frame, (temp_dat$Stop_Frame + 1 ) ) ]
  
  if( sum( is.na( behav_stop_timestamps ) ) > 0 |  sum( is.na( behav_start_timestamps ) ) > 0 ) stop( 'problem' )
  
  temp_dat$start_timestamp <- as.POSIXct( behav_start_timestamps, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS" ) 
  
  temp_dat$stop_timestamp <- as.POSIXct( behav_stop_timestamps, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS" ) 
  
  temp_dat <- temp_dat[ , c( 'Subject', 'start_timestamp', 'stop_timestamp', 'Start_Frame', 'Stop_Frame', 'Value', 'Behaviour', 'Modifiers', 'Scoring' ) ]
  
  new_dfs[[ i ]] <- temp_dat
  
  
}

full_focal <- ldply( new_dfs, rbind )

collar_dat <- read.csv( "DATA/Collaring_data.csv" )

collar_dat <- collar_dat[ , c( 'collar_id', 'collar_color', 'battery' ) ]

collar_dat <- collar_dat[ !is.na( collar_dat$collar_id ), ]

collar_dat$name <- paste( collar_dat$collar_color, collar_dat$battery, sep = '_' )

fuller_focal <- merge( x = full_focal, y = collar_dat[ , c( 'name', 'collar_id' ) ], by.x = 'Subject', by.y = 'name', all.x = T, all.y = F, sort = F )

fuller_focal$start_timestamp <- as.POSIXct( fuller_focal$start_timestamp, tz = 'UTC' )

fuller_focal$stop_timestamp <- as.POSIXct( fuller_focal$stop_timestamp, tz = 'UTC' )

fuller_focal$duration <- as.numeric( fuller_focal$stop_timestamp - fuller_focal$start_timestamp, unit = 'secs' )


full_trim_focal <- fuller_focal[ fuller_focal$Value %in% c( 'Active', 'Alert', 'Unalert' ), ]

full_trim_focal[ full_trim_focal$Subject == 'green_yellow_D', ]

sum( full_trim_focal$duration )/60/60


# full_trim_focal <- fuller_focal[ fuller_focal$Behaviour %in% c( 'position' ), ]
# 
# full_trim_focal[ full_trim_focal$Subject == 'green_yellow_D', ]
# 
# sum( full_trim_focal$duration )/60/60

length( unique( full_trim_focal$Subject ) )


#### now we will make it into a second-by-second format, which will make it easier to merge into the thermal tracklet and/or sensor data

tags <- as.character( unique( full_trim_focal$collar_id ) )

sec_focal_dat_sleep <- data.frame( matrix( NA, nrow = 0, ncol = 3 ) )

names( sec_focal_dat_sleep ) <- c( 'tag', 'timestamp', 'sleep_behavior' )

for( focal_tag in tags ){
  
  trim_focal <- full_trim_focal[ full_trim_focal$collar_id == focal_tag & full_trim_focal$Value %in% c( 'Active', 'Alert', 'Unalert' ), ]
  
  trim_focal <- trim_focal[ order( trim_focal$start_timestamp ),]
  
  tag_sec_focal_dat_sleep <- data.frame( tag = focal_tag, timestamp = seq( floor_date( min( trim_focal$start_timestamp ), unit = "min" ), ceiling_date( max( trim_focal$stop_timestamp ), unit = "min" ), by = '1 secs' ), sleep_behavior = NA )
  
  for( i in 1:nrow( trim_focal ) ){
    
    start_ind <- min( which( tag_sec_focal_dat_sleep$timestamp > trim_focal$start_timestamp[ i ] ) )
    
    stop_ind <- max( which( tag_sec_focal_dat_sleep$timestamp < ( trim_focal$stop_timestamp[ i ] - 1 ) ) )
    
    if( start_ind <= stop_ind ){ # this prevents problems caused by when behaviors last a second or less
      
      tag_sec_focal_dat_sleep$sleep_behavior[ start_ind:stop_ind ] <- trim_focal$Value[ i ]
      
    }
  }
  
  sec_focal_dat_sleep <- rbind( sec_focal_dat_sleep, tag_sec_focal_dat_sleep )
  
}

sec_focal_dat_sleep <- sec_focal_dat_sleep[ !is.na( sec_focal_dat_sleep$sleep_behavior ), ]


## now do the same for the position state

position_focal <- fuller_focal[ fuller_focal$Behaviour == "position", ]

tags <- as.character( unique( position_focal$collar_id ) )

sec_focal_dat_position <- data.frame( matrix( NA, nrow = 0, ncol = 3 ) )

names( sec_focal_dat_position ) <- c( 'tag', 'timestamp', 'position' )

for( focal_tag in tags ){
  
  trim_focal <- position_focal[ position_focal$collar_id == focal_tag , ]
  
  trim_focal <- trim_focal[ order( trim_focal$start_timestamp ),]
  
  tag_sec_focal_dat_position <- data.frame( tag = focal_tag, timestamp = seq( floor_date( min( trim_focal$start_timestamp ), unit = "min" ), ceiling_date( max( trim_focal$stop_timestamp ), unit = "min" ), by = '1 secs' ), position = NA )
  
  for( i in 1:nrow( trim_focal ) ){
    
    start_ind <- min( which( tag_sec_focal_dat_position$timestamp > trim_focal$start_timestamp[ i ] ) )
    
    stop_ind <- max( which( tag_sec_focal_dat_position$timestamp < ( trim_focal$stop_timestamp[ i ] - 1 ) ) )
    
    if( start_ind <= stop_ind ){ # this prevents problems caused by when behaviors last a second or less
      
      tag_sec_focal_dat_position$position[ start_ind:stop_ind ] <- trim_focal$Value[ i ]
      
    }
  }
  
  sec_focal_dat_position <- rbind( sec_focal_dat_position, tag_sec_focal_dat_position )
  
}


sec_focal_dat_position <- sec_focal_dat_position[ !is.na( sec_focal_dat_position$position ), ]


sec_focal_dat <- merge( x = sec_focal_dat_sleep, y = sec_focal_dat_position, by = c( 'tag', 'timestamp' ), all = T, sort = F )

sec_focal_dat <- sec_focal_dat[ order( sec_focal_dat$timestamp ), ] 
sec_focal_dat <- sec_focal_dat[ order( sec_focal_dat$tag ), ] 

sec_focal_dat$local_timestamp <- sec_focal_dat$timestamp + 3*60*60

sec_focal_dat <- sec_focal_dat[ , names( sec_focal_dat ) != 'timestamp' ]

head( sec_focal_dat )

table( sec_focal_dat[ , c( 'tag', 'sleep_behavior' ) ] )
  
table( sec_focal_dat[ , c( 'tag', 'position' ) ] )

## write the second-by-second focal data to a csv
write.csv( sec_focal_dat, "DATA/thermal_focal_follows/sec_focal_dat.csv", row.names = F )
  
