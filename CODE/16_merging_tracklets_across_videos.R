

options( digits.secs = 6 ) #reset options, in order to see the milliseconds

library( stringr )
library( lubridate )
library( dtw )
library( plyr )
library( zoo )
library( doParallel )
library( foreach )

input_data <- 'corrected' ## can be 'corrected' or 'uncorrected'

# if we aren't working on the Davis computer and therefore have access to the department server, extract the relevant files from the department server onto the local machine
if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] != 'jcl273' ){
  
  if( input_data == 'uncorrected' ){
    
    dir.create( paste( getwd(), 'DATA/tracking_output/uncorrected_tracks', sep = '/' ) )
    
    input_vids_path <- 'Z:/baboon/archive/processed/video/thermal/2019_summer/cliff_data/mp4_files/viewpoint_1/T1020'
    
    # input_tracks_path <- "Z:/baboon/archive/pubs/Loftus_dissertation/social_sleep/DATA/tracking_output/fewer_tracks_parameters"
    input_tracks_path <- "DATA/tracking_output/fewer_tracks_parameters"
    
    folders <- list.files( input_vids_path )
    
    for( folder in folders ){
      
      txt_files <- list.files( paste( input_vids_path, folder, sep = '/' ), pattern = '.txt' )
      
      dir.create( paste( getwd(), 'DATA/tracking_output/uncorrected_tracks', folder, sep = '/' ) )
      
      file.copy( from = paste( input_vids_path, folder, txt_files, sep = '/' ), to = paste( getwd(), 'DATA/tracking_output/uncorrected_tracks', folder, txt_files, sep = '/' ), overwrite = T )
      
      vid_names <- str_split_fixed( txt_files, '.txt', 2 )[ , 1 ]
      
      csv_names <- paste0( 'tracks_', vid_names, '.csv' )
      
      file.copy( from = paste( input_tracks_path, folder, csv_names, sep = '/' ), to = paste( getwd(), 'DATA/tracking_output/uncorrected_tracks', folder, csv_names, sep = '/' ), overwrite = T )
      
    }
  }
}



if( input_data == 'corrected' ){
  
  nights <- list.files( "DATA/tracking_output/finalized_tracks/" )
  
}else{
  
  if( input_data == 'uncorrected' ){
    
    nights <- list.files( "DATA/tracking_output/uncorrected_tracks/" )
    
  }
  
} 

dir.create( paste0( getwd(), "/DATA/thermal_tracks/auto_stitch_across_vids/", input_data ), recursive = T )


stopImplicitCluster()

if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  registerDoParallel( length( nights ) )
  
}else{
  
  registerDoParallel( 4 )
  
}

therm_tracks_list <- foreach( ni = 1:length( nights ), .packages = .packages() ) %dopar% {
  
  night <- nights[ ni ]
  
  #for( night in nights ){
  
  ## read in tracks
  if( input_data == 'corrected' ){
    
    #files <- list.files( path = paste0( "Z:/baboon/archive/pubs/Loftus_dissertation/social_sleep/DATA\\tracking_output\\finalized_tracks\\", night ), pattern = '.csv', full.names = T, recursive = T )
    
    files <- list.files( path = paste0( "DATA/tracking_output/finalized_tracks/", night, "/" ), pattern = '.csv', full.names = T, recursive = T )
    
  }else{
    
    if( input_data == 'uncorrected' ){
      
      files <- list.files( path = paste0( "DATA/tracking_output/uncorrected_tracks/", night, "/" ), pattern = '.csv', full.names = T, recursive = T )
      
    }
  }
  
  
  files <-  sort( files )
  
  dfs <- lapply( as.list( files ), read.csv )
  
  print( files )
  
  ## view the thermal tracking data
  lapply( dfs, head )
  
  ## pair the tracks that end at the end of a video with the tracks that start nearby at the beginning of the next video
  
  last_time_buffer <- 1 # determines the number of seconds before the end of a given video to subset the tracking data to. We will search for a possible continuation of each track in this subset in the following video's tracking data.
  
  first_time_buffer <- 3 # determines the number of seconds after the start of a given video to subset the tracking data to. Any track in this subset represents will be considered a candidate for a continuation of a track from the previous video
  
  dist_thresh <- 25 # determines the distance that the start of a track in the following video has to be within the end of a track in the current video in order to be assigned as a continuation of that track (i.e. the search radius of a current track when looking for a next track that will be assigned as its continuation)
  
  ## read in frame information
  if( input_data == 'corrected' ){
    
    files <- list.files( path = paste0( "DATA/tracking_output/finalized_tracks/", night, "/" ), pattern = '.txt', full.names = T, recursive = T )
    
  }else{
    
    if( input_data == 'uncorrected' ){
      
      files <- list.files( path = paste0( "DATA/tracking_output/uncorrected_tracks/", night, "/" ), pattern = '.txt', full.names = T, recursive = T )
      
    }
  }
  
  
  files <-  sort( files )
  
  timestamp_txts <- lapply( as.list( files ), read.table ) # read in each txt file with the frame information. These txt files give the timestamp of each frame
  
  ## add timestamp to the thermal tracking data frames
  for( i in 1:length( timestamp_txts ) ){ # for each hour of video recording (i.e. each separate video)...
    
    frame_info <- timestamp_txts[[ i ]] # save the dataframe with the timestamps from this video
    
    vid_name <- str_split_fixed( basename( files[ i ] ), ".txt", 2 )[ , 1 ] # split the string to extract the name of the video
    
    if( nrow( dfs[[ i ]] ) == 0 ){
      
      next 
      
    }
    
    if( vid_name != as.character( unique( dfs[[ i ]]$vid_name ) ) ){ # check to make sure that the video name from this txt file matches the name of the video from the tracklet data before we merge the timestamps from this txt file dataframe into the tracklet data frame
      
      stop( ".txt file does not match .mp4 video file" )
      
    }
    
    ## use the frame information to associate a timestamp with each frame
    names( frame_info ) <- c( "file", "frame_info" ) # name the columns
    
    # split the string twice to extract the timestamp associated with each frame
    frame_timestamps_temp <- str_split_fixed( frame_info$frame_info, "_", 2 )[, 2 ]
    
    frame_timestamps <- str_split_fixed( frame_timestamps_temp, ".tiff", 2 )[, 1 ]
    
    
    ## reformat to the typical timestamp format by parsing the string (using the format field of as.POSIX can get me part way there to the reformatting, but causes problems with the milliseconds)
    final_timestamps <- paste0( substring( frame_timestamps, 1, 4 ), "-", substring( frame_timestamps, 5, 6 ), "-", substring( frame_timestamps, 7, 8 ), " ", substring( frame_timestamps, 10, 11 ), ":", substring( frame_timestamps, 12, 13 ), ":", substring( frame_timestamps, 14, 15 ), ".", substring( frame_timestamps, 16, 21 ) )
    
    ## change the timestamps into posix elements
    final_timestamps_posx <- as.POSIXct( final_timestamps, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS" )
    
    ## associate each frame number in the tracklets dataframe with the timestamp of that frame (+1 because of the difference between python and R indexing)
    tmstp <- final_timestamps_posx[ ( dfs[[ i ]]$frame + 1 ) ]
    
    dfs[[ i ]]$timestamp <- tmstp # add this timestamp information to the tracklet data frame
    
    ## save the start time of the video (we will use this start time to look for tracks that start right around it, as potential candidates for continuations)
    dfs[[ i ]]$vid_start <- min( final_timestamps_posx )
    
    ## save the end time of the video (we will use this end time to look for tracks that end just before it, to stitch them to tracklets that start in the next video)
    dfs[[ i ]]$vid_end <- max( final_timestamps_posx )
    
    if( sum( is.na( dfs[[ i ]]$timestamp ) ) > 0 ){ # check to make sure all locations in the tracklet dataframe received a timestamp
      
      stop( "some timestamps not matched up" )
    }
  }
  
  #### Automated merging of tracks across videos #####
  ## now string together tracks that cross videos and actually represent one track
  
  if( length( dfs ) > 1 ){ # only string together if there is more than one video to actually string together
    
    
    for( i in 1:( length( dfs ) - 1 ) ){ # for each hour of tracking (each video from a night represents one hour of recording from that night) before the last hour...
      
      df <- dfs[[ i ]] # save the tracking data from this hour
      
      last_time <- unique( df$vid_end ) # find the last timestamp of this video
      
      vid_end_df <- df[ df$timestamp > ( last_time - last_time_buffer ), ] # subset the data to the last n seconds of the video (n = last_time_buffer)
      
      tracks_present <- unique( vid_end_df$id ) # save a vector of the tracks that exist within this end subset of the data from this video (i.e. from this hour of tracking)
      
      if( length( tracks_present ) != 0 ){
        
        matching_df <- data.frame( current_track = tracks_present, next_track = NA, distance = NA ) # create a data frame of the tracks that exist at the end of the video. We will fill this in with the track in the following video that each track matches with, and the distance to that track that it gets matched with
        
        next_df <- dfs[[ i + 1 ]] # save the tracking data from the next hour
        
        first_time <- unique( next_df$vid_start ) # save the start time of the video for the next hour
        
        if( first_time < ( last_time + 5 ) ){ # if the first frame of the next video is not within five seconds of the last frame of the previous video (this only happens when the videos are separated by hours because not all of the videos were stitched -- some at the beginning were stitched and some at the end were stitched) 
          
          
          vid_start <- next_df[ next_df$timestamp < ( first_time + first_time_buffer ), ] # subset the next hour of tracking data to the first n seconds of that video (n = first_time_buffer)
          
          vid_start <- vid_start[ order( vid_start$frame ), ] # reorder this subset by frame number
          
          next_tracks_present <- unique( vid_start$id ) # save a vector of the tracks that exist within this early subset of the next video's data
          
          first_locations <- vid_start[ !duplicated( vid_start$id ), ] # remove the duplicates so that we just have one location for each track that is present at the beginning of the next video
          
          for( track in tracks_present ){ # for each track that exists at the end of the current video...
            
            id_dat <- vid_end_df[ vid_end_df$id == track, ] # subset the end-of-video data to just this individual's data
            
            last_x_pos <- tail( id_dat$x, 1 ) # save this individual's last x location
            
            last_y_pos <- tail( id_dat$y, 1 ) # save this individual's last y location
            
            distances <- sqrt( ( first_locations$x - last_x_pos )**2 + ( first_locations$y - last_y_pos )**2 ) # create a vector of the distances from this track to the first location of the tracks starting at the beginning of the following video
            
            if( min( distances ) < dist_thresh ){ # if the closest track at the beginning of the following video is within the defined search radius of the current track
              
              matching_track <- first_locations$id[ which.min( distances ) ] # save the identity of this closest track
              
              matching_df[ matching_df$current_track == track, 'next_track' ] <- matching_track # store the identity of this closest track in the data frame used for track matching across videos
              
              matching_df[ matching_df$current_track == track, 'distance' ] <- min( distances ) # store the distance from the end of the current track to the beginning of this closest track in the following video
              
            }
            
          }
          
          while( sum( duplicated( matching_df$next_track, incomparables = NA ) ) > 0 ){ # while there is more than one track in the current video paired to the same track in the following video... 
            
            print( "competition" ) # print competition to let me know at least two tracks are competiing over the same track
            
            competed_over_tracks <- unique( matching_df$next_track[ ( duplicated( matching_df$next_track, incomparables = NA ) ) ] ) # save a vector containing the IDs of the tracks being competed over
            
            for( next_track in competed_over_tracks ){ # for each track that is being competed over
              
              track_df <- matching_df[ matching_df$next_track == next_track & !( is.na( matching_df$next_track ) ), ] # subset the data from the current video to just the tracks that are competing over the given track in the next video
              
              closest_track <- track_df$current_track[ which.min( track_df$distance ) ] # find and save the track that is closest to the track being competed over
              
              first_locations <- first_locations[ first_locations$id != next_track, ] # now that we have confirmed the closest track to this next track, remove this next track from the candidate options from which the current tracks can choose
              
              ousted_tracks <- track_df$current_track[ track_df$current_track != closest_track ] # save a vector of the tracks that weren't the closest to the next track being competed over
              
              matching_df[ matching_df$current_track %in% ousted_tracks, c( 'next_track', 'distance' ) ] <- NA # for these tracks that weren't the closest, reset their 'next_track' and 'distance' columns
              
              ## give these ousted tracks another chance to pick a new candidate track that can serve as its continuation in the following video
              for( ousted_track in ousted_tracks ){ # for each track that wasn't the closest to the competed over track in the next video...
                
                id_dat <- vid_end_df[ vid_end_df$id == ousted_track, ] # subset the end-of-video data to just this individual track's end-of-video data
                
                last_x_pos <- tail( id_dat$x, 1 ) # save this individual's last x position
                
                last_y_pos <- tail( id_dat$y, 1 ) # save this individual's last y position
                
                distances <- sqrt( ( first_locations$x - last_x_pos )**2 + ( first_locations$y - last_y_pos )**2 ) # find the distance of from this individual's last location to the first location of all the candidate tracks in the next video
                
                if( min( distances ) < dist_thresh ){ # if the closest candidate track is within the allowable search radius...
                  
                  matching_track <- first_locations$id[ which.min( distances ) ]  # save the identity of this closest track
                  
                  matching_df[ matching_df$current_track == ousted_track, 'next_track' ] <- matching_track # store the identity of this closest track in the data frame used for track matching across videos
                  
                  matching_df[ matching_df$current_track == ousted_track, 'distance' ] <- min( distances ) # store the distance from the end of the current track to the beginning of this closest track in the following video
                  
                }
              }
            }
          }
        }else{
          
          print( night )
          print( last_time )
          print( first_time )
          
        } 
        
        merge_df <- merge( x = df, y = matching_df[ , 1:2 ], by.x = 'id', by.y = 'current_track', all.x = T, all.y = F ) # merge the track matching information, containing information about the next track to pair this track with, into the tracklet data
        
        dfs[[ i ]] <- merge_df # replace the tracklet data in the list with this new merged dataframe with the information about which next track is a continuation of each track
        
      }else{
        
        if( nrow( df ) != 0 ){
          
          df$next_track <- NA
          
        }
        
        dfs[[ i ]] <- df
        
      } 
      
      
    }
    lapply( dfs, head )
    
    
    ident_df_temp <- unique( dfs[[ 1 ]][ , c( 'id', 'next_track' ) ] ) # make a dataframe of each track in the first video and the track it matches with in the second video
    
    names( ident_df_temp ) <- c( 'id_vid_01', 'id_vid_02' ) # rename the columns accordingly
    
    if( length( files ) > 2 ){
      
      for( i in 2:( length( dfs ) - 1 ) ){ # for each hour of data, except for the first and last...
        
        if( nrow( dfs[[ i ]] ) != 0 ){
          
          ident_df_temp <- merge( x = ident_df_temp, y = unique( dfs[[ i ]][ , c( 'id', 'next_track' ) ] ), by.x = paste0( 'id_vid_', str_pad( string = i, width = 2, side = 'left', pad = 0  ) ), by.y = 'id', all.x = T, all.y = T, sort = F ) # merge the data to a running data frame containing the matching key of the tracklet IDs from each video
          
          names( ident_df_temp )[ names( ident_df_temp ) == 'next_track' ] <- paste0( 'id_vid_', str_pad( string = ( i + 1 ), width = 2, side = 'left', pad = 0  ) ) # rename the column merged in (so that the next merge in the loop will function)
          
        }
        
      }
      
    }
    
    ident_df_temp <- ident_df_temp[ , order( names( ident_df_temp ) ) ] # reorder the column of the data frame by their names (so that vid 1 comes fist and the last video comes last, etc.)
    
    if( nrow( dfs[[ length( dfs ) ]] ) != 0 ){
      
      excluded <- unique( dfs[[ length( dfs ) ]]$id[ ! dfs[[ length( dfs ) ]]$id %in% ident_df_temp[ , paste0( 'id_vid_', str_pad( string = length( dfs ), width = 2, side = 'left', pad = 0  ) ) ] ] ) # any tracks in the last video that don't get paired to a track in the previous video have been left out of the data frame that we built with the merges above in the loop. Save a vector of these tracks in the last video that have been left out
      
      df_to_add <- as.data.frame( matrix( NA, nrow = length( excluded ), ncol = length( dfs ) ) ) # instantiate an empty data frame with a row for each track in the last video that was excluded from the identification data frame 
      
      names( df_to_add ) <- names( ident_df_temp ) # name the columns with the same names as ident_df_temp
      
      df_to_add[ , which( names( df_to_add ) == paste0( 'id_vid_', str_pad( string = length( dfs ), width = 2, side = 'left', pad = 0  ) ) ) ] <- excluded # add the id of each tracklet from the last video in the last column of the empty data frame. The other columns, representing the other the tracklets id in the other videos, will appropriately remain NAs because these tracklets did not pair with any previous videos
      
      ident_df <- rbind( ident_df_temp, df_to_add ) # merge the data frames
      
    }else{
      
      ident_df <- ident_df_temp
    }
    
    if( input_data == 'corrected' ){
      
      ident_df$unique_ID <- apply( ident_df, 1, FUN = function( x ) paste( str_pad( x, 3, side = 'left', pad = 0 ), collapse = '_' ) ) # paste the ids from each video to generate a unique identifier for each tracklet for the night
      
    }else{
      
      ident_df$unique_ID <- apply( ident_df, 1, FUN = function( x ) paste( str_pad( x, 4, side = 'left', pad = 0 ), collapse = '_' ) ) # paste the ids from each video to generate a unique identifier for each tracklet for the night
      
    }
    
    #length( unique( dfs[[ length( dfs ) ]]$id ) ) # these two rows should produce the same number, indicating all tracklets in the last video have been accounted for. But I commented it out for now because it causes an error if the last video doesn't have any tracks in it
    #length( sort( ident_df[ , paste0( "id_vid_", length( dfs ) ) ] ) )
    
    ## merge the unique identifier information back into the original tracking data frames
    for( i in 1:length( dfs ) ){
      
      if( nrow( dfs[[ i ]] ) != 0 ){
        
        dfs[[ i ]] <- merge( x = dfs[[ i ]], y = ident_df[ , c( paste0( 'id_vid_', str_pad( string = i, width = 2, side = 'left', pad = 0  ) ), 'unique_ID' ) ], by.x = 'id', by.y = paste0( 'id_vid_', str_pad( string = i, width = 2, side = 'left', pad = 0  ) ), all.x = T, all.y = F, sort = F )
        
      }
      
    }
    
    therm_tracks <- ldply( dfs ) # combine tracklet data frames from each hour into a single data frame
    
  }else{
    
    therm_tracks <- dfs[[ 1 ]]
    
    therm_tracks$next_track <- NA
    
    if( input_data == 'corrected' ){
      
      therm_tracks$unique_ID <- str_pad( therm_tracks$id, 3, side = 'left', pad = 0 )
      
    }else{
      
      therm_tracks$unique_ID <- str_pad( therm_tracks$id, 4, side = 'left', pad = 0 )
      
    }
    
  }
  
  
  
  head( therm_tracks )
  
  therm_tracks <- therm_tracks[ order( therm_tracks$timestamp ), ] # order by timestamp
  therm_tracks <- therm_tracks[ order( therm_tracks$unique_ID ), ] # order by ID
  
  if( sum( is.na( therm_tracks$unique_ID ) ) > 0 ){
    
    stop( 'there are some ids without a unique identifier' )
    
    unique( therm_tracks[  is.na( therm_tracks$unique_ID ), 'vid_name' ] )
    
  } # check if there are any tracklets with no unique identifier
  
  # remove the following lines because we are doing the timestamp correction in the sensor data now, not the thermal data
  
  # # add the 16 second correction to the thermal tracklets to align them with GPS time
  # 
  # therm_tracks$corr_timestamp <- therm_tracks$timestamp + 16
  # 
  
  ## remove everything after 15:40:40 on 20190809, because this is when the baboon pulls down the camera
  if( night == '20190809' ){ 
    
    therm_tracks <- therm_tracks[ therm_tracks$timestamp < as.POSIXct( '2019-08-09 15:40:40', tz = 'UTC' ), ]
    
  }
  
  
  saveRDS( therm_tracks, paste0( "DATA/thermal_tracks/auto_stitch_across_vids/", input_data, '/', night, "_tracks.rds" ) )
  
  return( therm_tracks )
  
}

stopImplicitCluster( )


### this just helps to determine the full unique identifiers given the track ID number in a given video of the night

therm_tracks <- thermal_tracks[[ 1 ]] 

vid_num <- 2

track_num <- 176

tracklets <- unique( therm_tracks$unique_ID )

tracklets[ which( as.numeric( str_split( tracklets, "_", simplify = T )[ , vid_num ] ) == track_num ) ]







#### Manual merging of tracks across videos. Note that manual merge is only relevant for corrected tracks, and this code will run on tracks in the finalized_tracks folder, regardless of the 'input_data' that is input above ####


input_data <- 'corrected'

dir.create( paste0( getwd(), "/DATA/thermal_tracks/manual_stitch_across_vids/" ) )
dir.create( paste0( getwd(), "/DATA/thermal_tracks/manual_stitch_across_vids/", input_data ) )



if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  nights <- str_split_fixed( list.files( "DATA/vid_to_vid_id_matching/", pattern = '.csv' ), '.csv', 2 )[ , 1 ]
  
}else{
  
  nights <- str_split_fixed( list.files("Z:\\baboon\\working\\video\\thermal\\thermal_baboon\\DATA\\vid_to_vid_id_matching\\",  pattern = '.csv'  ), '.csv', 2 )[ , 1 ]
  
}


if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  registerDoParallel( length( nights ) )
  
}else{
  
  registerDoParallel( 4 )
  
}


thermal_tracks <- foreach( ni =  1:length( nights ), .packages = .packages() ) %dopar% {
  
  #for( ni in 1:length( nights ) ) {
  
  night <- nights[ ni ]
  
  
  # read in manual matching data
  if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
    
    matching_dat <- read.csv( paste0( "DATA/vid_to_vid_id_matching/", night, ".csv" ) )
    
  }else{
    
    matching_dat <- read.csv( paste0( "Z:\\baboon\\working\\video\\thermal\\thermal_baboon\\DATA\\vid_to_vid_id_matching\\", night, ".csv" ) )
    
  }
  
  matching_dat$unique_ID <- apply( matching_dat, 1, FUN = function( x ) paste( str_pad( x, 3, side = 'left', pad = 0 ), collapse = '_' ) ) # paste the ids from each video to generate a unique identifier for each tracklet for the night
  
  
  ## read in tracks
  #t_files <- list.files( path = paste0( "Z:\\baboon\\working\\video\\thermal\\thermal_baboon\\RESULTS\\tracking_output\\viewpoint_1\\finalized_tracks\\", night ), pattern = '.csv', full.names = T, recursive = T )
  t_files <- list.files( path = paste0( "RESULTS/tracking_output/viewpoint_1/finalized_tracks/", night, "/" ), pattern = '.csv', full.names = T, recursive = T )
  
  t_files <-  sort( t_files )
  
  dfs <- lapply( as.list( t_files ), read.csv )
  
  print( t_files )
  
  ## read in frame information
  if( input_data == 'corrected' ){
    
    files <- list.files( path = paste0( "RESULTS/tracking_output/viewpoint_1/finalized_tracks/", night, "/" ), pattern = '.txt', full.names = T, recursive = T )
    
  }else{
    
    if( input_data == 'uncorrected' ){
      
      files <- list.files( path = paste0( "RESULTS/tracking_output/viewpoint_1/uncorrected_tracks/", night, "/" ), pattern = '.txt', full.names = T, recursive = T )
      
    }
  }
  
  
  files <-  sort( files )
  
  timestamp_txts <- lapply( as.list( files ), read.table ) # read in each txt file with the frame information. These txt files give the timestamp of each frame
  
  ## add timestamp to the thermal tracking data frames
  for( i in 1:length( timestamp_txts ) ){ # for each hour of video recording (i.e. each separate video)...
    
    frame_info <- timestamp_txts[[ i ]] # save the dataframe with the timestamps from this video
    
    vid_name <- str_split_fixed( basename( files[ i ] ), ".txt", 2 )[ , 1 ] # split the string to extract the name of the video
    
    if( nrow( dfs[[ i ]] ) == 0 ){
      
      next 
      
    }
    
    if( vid_name != as.character( unique( dfs[[ i ]]$vid_name ) ) ){ # check to make sure that the video name from this txt file matches the name of the video from the tracklet data before we merge the timestamps from this txt file dataframe into the tracklet data frame
      
      stop( ".txt file does not match .mp4 video file" )
      
    }
    
    ## use the frame information to associate a timestamp with each frame
    names( frame_info ) <- c( "file", "frame_info" ) # name the columns
    
    # split the string twice to extract the timestamp associated with each frame
    frame_timestamps_temp <- str_split_fixed( frame_info$frame_info, "_", 2 )[, 2 ]
    
    frame_timestamps <- str_split_fixed( frame_timestamps_temp, ".tiff", 2 )[, 1 ]
    
    
    ## reformat to the typical timestamp format by parsing the string (using the format field of as.POSIX can get me part way there to the reformatting, but causes problems with the milliseconds)
    final_timestamps <- paste0( substring( frame_timestamps, 1, 4 ), "-", substring( frame_timestamps, 5, 6 ), "-", substring( frame_timestamps, 7, 8 ), " ", substring( frame_timestamps, 10, 11 ), ":", substring( frame_timestamps, 12, 13 ), ":", substring( frame_timestamps, 14, 15 ), ".", substring( frame_timestamps, 16, 21 ) )
    
    ## change the timestamps into posix elements
    final_timestamps_posx <- as.POSIXct( final_timestamps, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS" )
    
    ## associate each frame number in the tracklets dataframe with the timestamp of that frame (+1 because of the difference between python and R indexing)
    tmstp <- final_timestamps_posx[ ( dfs[[ i ]]$frame + 1 ) ]
    
    dfs[[ i ]]$timestamp <- tmstp # add this timestamp information to the tracklet data frame
    
    ## save the start time of the video (we will use this start time to look for tracks that start right around it, as potential candidates for continuations)
    dfs[[ i ]]$vid_start <- min( final_timestamps_posx )
    
    ## save the end time of the video (we will use this end time to look for tracks that end just before it, to stitch them to tracklets that start in the next video)
    dfs[[ i ]]$vid_end <- max( final_timestamps_posx )
    
    if( sum( is.na( dfs[[ i ]]$timestamp ) ) > 0 ){ # check to make sure all locations in the tracklet dataframe received a timestamp
      
      stop( "some timestamps not matched up" )
    }
  }
  
  
  ## merge the unique identifier information back into the original tracking data frames
  
  for( i in 1:length( dfs ) ){
    
    df_vid_name <- unique( dfs[[ i ]]$vid_name )
    
    col_ind <- which( grepl( df_vid_name, names( matching_dat ) ) )
    
    col_name <- names( matching_dat )[ col_ind ]
    
    ids_included <- sort( unique( matching_dat[ , col_name ] ) )
    
    all_ids <- unique( dfs[[ i ]]$id )
    
    ids_excluded <- all_ids[ ! all_ids %in% ids_included ]
    
    df_to_add <- data.frame( matrix( NA, nrow = length( ids_excluded ), ncol = length( dfs ) ) )
    
    df_to_add[ , col_ind ] <- ids_excluded
    
    df_to_add$unique_ID <- apply( df_to_add, 1, FUN = function( x ) paste( str_pad( x, 3, side = 'left', pad = 0 ), collapse = '_' ) ) # paste the ids from each video to generate a unique identifier for each tracklet for the night
    
    names( df_to_add ) <- names( matching_dat )
    
    dat_to_merge_nonas <- matching_dat[ !is.na( matching_dat[ , col_name ] ), ]
    
    final_merge_dat <- rbind( dat_to_merge_nonas, df_to_add )
    
    dfs[[ i ]] <- merge( x = dfs[[ i ]], y = final_merge_dat[ , c( col_name, 'unique_ID' ) ], by.x = 'id', by.y = col_name, all.x = T, all.y = F, sort = F )
    
  }
  
  therm_tracks <- ldply( dfs ) # combine tracklet data frames from each hour into a single data frame
  
  head( therm_tracks )
  
  therm_tracks <- therm_tracks[ order(therm_tracks$timestamp ), ] # order by timestamp
  therm_tracks <- therm_tracks[ order(therm_tracks$unique_ID ), ] # order by ID
  
  sum( is.na( therm_tracks$unique_ID ) ) # check if there are any tracklets with no unique identifier
  
  # add the 16 second correction to the thermal tracklets to align them with GPS time. Nevermind, I am doing the time correction in the sensor data now
  
  # therm_tracks$corr_timestamp <- therm_tracks$timestamp + 16
  
  saveRDS( therm_tracks, paste0( "DATA/thermal_tracks/manual_stitch_across_vids/", input_data, "/", night, "_tracks.rds" ) )
  
  return( therm_tracks )
}

stopImplicitCluster()








