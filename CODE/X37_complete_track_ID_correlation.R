


options( digits.secs = 6 )
library( data.table )
library( lubridate )
library( stringr )
library( hms )
library( ascii )
library( plyr )
library( tidyr )
library( DescTools )
library(foreach)
library(doParallel)

######### functions #########
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

cor_function <- function( x, y ){
  
  if( sum( !is.na( x ) ) > 2 & sum( !is.na( y ) ) > 2 & sum( complete.cases( x, y ) ) > 3 ){
    
    return( cor.test( x, y )$estimate )
    
  }else{
    
    return( NA )
    
  }
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
##############


input_data <- 'corrected' ## can be 'corrected' or 'uncorrected'

should_smooth_tracklets <- F

acc_informed <- T



files <- list.files( path = paste0( "DATA/thermal_tracks/smooth_tracks/", input_data ) )

nights <- str_split_fixed( files, "_", 2 )[, 1 ]




#### read in the smoothed speeds from the GPS ####

speeds_smoothed <- fread( "DATA/gps_acc_data/speeds_smoothed.csv" )

speeds_smoothed <- as.data.frame( speeds_smoothed )

speeds_smoothed$corr_local_timestamp <- as.POSIXct( speeds_smoothed$corr_local_timestamp, tz = 'UTC' )


#### read in the log VeDBA data for the continuous ACC ####
cont_vedba_smoothed <- fread( "DATA/gps_acc_data/cont_vedba_smoothed.csv" )

cont_vedba_smoothed <- as.data.frame( cont_vedba_smoothed )

cont_vedba_smoothed$corr_local_timestamp <- as.POSIXct( cont_vedba_smoothed$corr_local_timestamp, tz = 'UTC' )

#### read in the log VeDBA data from the ACC bursts ####
burst_vedba <- fread( "DATA/gps_acc_data/burst_vedba.csv" )

burst_vedba <- as.data.frame( burst_vedba )

burst_vedba$corr_local_timestamp <- as.POSIXct( burst_vedba$corr_local_timestamp, tz = 'UTC' )

#### read in the log VeDBA data from the ACC bursts that has the 2nd second of each burst for the 6000s collars removed, to make them more comparable to the 2000s collars ####
burst_vedba_one_sec <- fread( "DATA/gps_acc_data/burst_vedba_one_sec.csv" )

burst_vedba_one_sec <- as.data.frame( burst_vedba_one_sec )

burst_vedba_one_sec$corr_local_timestamp <- as.POSIXct( burst_vedba_one_sec$corr_local_timestamp, tz = 'UTC' )

options( warn = 0 )

dir.create( paste( getwd(), "DATA/thermal_tracks/identified_tracks", sep = '/' ) )
dir.create( paste( getwd(), "DATA/thermal_tracks/identified_tracks", input_data, sep = '/' ) )

detectCores()

stopImplicitCluster()


if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  registerDoParallel( length( nights ) )
  
}else{
  
  registerDoParallel( 2 )
  
}


identified_tracks <- foreach( ni = 1:length( nights ) ) %dopar% {
  
#for( ni in 1:length( nights ) ){
    
  night_of_int <- nights[ ni ]
  
  print( night_of_int )
  
  ###### read in thermal tracklet data and add speeds ######
  
  ##### read in tracklet data and fix timestamp #####

  smooth_tracks <- readRDS( paste0( "DATA/thermal_tracks/smooth_tracks/", input_data, '/', night_of_int, "_smooth_tracks.rds" ) )
  
  smooth_tracks$local_timestamp <- as.POSIXct( smooth_tracks$local_timestamp, tz = 'UTC' )
  
  ## fills in potential gaps in time in the thermal tracklets so that there is a row for each second
  min_time <- min( smooth_tracks$local_timestamp )
  max_time <- max( smooth_tracks$local_timestamp )
  
  all_times <- seq( min_time, max_time, by = '1 sec' )

  # ## can later add spatial discretization to this dataframe (pull it in from 02a_track_ID_pipeline_...)
  # 

  ####### trim the GPS and ACC data ###########
  
  #### trim the GPS speed data to only the time when the thermal tracklets are present ####
  trim_gps_speeds_smooth <- merge( x = data.frame( corr_local_timestamp = all_times ), y = speeds_smoothed, by = 'corr_local_timestamp', all.x = T, all.y = F, sort = F )
  
  trim_gps_speeds_smooth <- trim_gps_speeds_smooth[ order( trim_gps_speeds_smooth$corr_local_timestamp ), ]
  
  #### trim the continuous log VeDBA data to only the time when the thermal tracklets are present ####
  trim_vedba_smooth <- merge( x = data.frame( corr_local_timestamp = all_times ), y = cont_vedba_smoothed, by = 'corr_local_timestamp', all.x = T, all.y = F, sort = F )
  
  trim_vedba_smooth <- trim_vedba_smooth[ order( trim_vedba_smooth$corr_local_timestamp ), ]
  
  
  identical( trim_gps_speeds_smooth$corr_local_timestamp, trim_vedba_smooth$corr_local_timestamp ) 
  
  
  ## now make the GPS acc-informed 
  def_not_moving_thresh_2000s <- 3.3
  
  if( acc_informed ){
    
    gps_not_in_ved <- trim_gps_speeds_smooth[ , !names( trim_gps_speeds_smooth ) %in% names( trim_vedba_smooth ) ]
    
    gps_not_in_ved <- apply( gps_not_in_ved, c( 1, 2 ), FUN = function( x ) x*NA )
    
    acc_inform_dat <- cbind( trim_vedba_smooth[ , names( trim_vedba_smooth ) != 'corr_local_timestamp' ], gps_not_in_ved ) 
    
    temp_gps <- trim_gps_speeds_smooth[ , names( trim_gps_speeds_smooth ) != 'corr_local_timestamp' ]
    
    # make sure columns are in the same order as temp_gps
    acc_inform_dat <- acc_inform_dat[ , names( temp_gps ) ]
    
    stationary_inds <- which( acc_inform_dat < def_not_moving_thresh_2000s, arr.ind = T )
    
    temp_gps[ stationary_inds ] <- 0
    
    trim_gps_speeds_smooth[ , names( trim_gps_speeds_smooth ) != 'corr_local_timestamp' ] <- temp_gps
    
  }
  
  
  ### trim the burst log VeDBA data (both the full bursts, as well as the bursts that only have the first second of the burst represented (so that the 2000s and 6000s collars are comparable) ) to the times when the thermal tracklets are present
  trim_burst_vedba <- merge( x = data.frame( corr_local_timestamp = all_times ), y = burst_vedba, by = 'corr_local_timestamp', all.x = T, all.y = F, sort = F )
  
  trim_burst_vedba <- trim_burst_vedba[ order( trim_burst_vedba$corr_local_timestamp ), ]
  
  
  trim_burst_vedba_one_sec <- merge( x = data.frame( corr_local_timestamp = all_times ), y = burst_vedba_one_sec, by = 'corr_local_timestamp', all.x = T, all.y = F, sort = F )
  
  trim_burst_vedba_one_sec <- trim_burst_vedba_one_sec[ order( trim_burst_vedba_one_sec$corr_local_timestamp ), ]
  
  
  ###### smoooth and trim the tracklet speeds #########
  
  ## put the speeds of the tracklets into wide format as well
  tracklet_speeds_temp <- reshape2::dcast( smooth_tracks, local_timestamp ~ id, value.var = 'speed'  )
  
  #### smooth the tracklet speeds, if we input above that smoothing is desirable ####
  if( should_smooth_tracklets ){
    
    tracklet_speeds_unsmooth <- merge( x = data.frame( local_timestamp = all_times ), y = tracklet_speeds_temp, by = 'local_timestamp', all.x = T, all.y = T, sort = F )
    
    tracklet_speeds_unsmooth <- tracklet_speeds_unsmooth[ order( tracklet_speeds_unsmooth$local_timestamp ) , ]
    
    smoothing_window <- 5
    
    tracklet_speeds <- apply( tracklet_speeds_unsmooth[ , names( tracklet_speeds_unsmooth ) != 'local_timestamp' ], MARGIN = 2, FUN = pad_roll_med, k = smoothing_window )
    
    tracklet_speeds <- as.data.frame( tracklet_speeds )
    
    tracklet_speeds$local_timestamp <- tracklet_speeds_unsmooth$local_timestamp
    
  }else{
    
    tracklet_speeds <- merge( x = data.frame( local_timestamp = all_times ), y = tracklet_speeds_temp, by = 'local_timestamp', all.x = T, all.y = T, sort = F )
    
    tracklet_speeds <- tracklet_speeds[ order( tracklet_speeds$local_timestamp ) , ]
    
  }
  
  # 
  # #### trim the tracklets to the times when the continuous ACC and GPS are available (this will be everything prior to 18:00) #### Nevermind, not going to do this -- instead just going to keep everything as full as possible
  
  # trim_tracklet_speeds <- tracklet_speeds[ tracklet_speeds$local_timestamp <= max( trim_gps_speeds_smooth$corr_local_timestamp ), ]
  
  trim_tracklet_speeds <- tracklet_speeds
  
  
  # all of these time-series (ACC, GPS, and tracklet) should have one row for each second from the start of the tracklets to to the end of the tracklets. Let's confirm that
  identical( trim_tracklet_speeds$local_timestamp, trim_gps_speeds_smooth$corr_local_timestamp )
  identical( trim_tracklet_speeds$local_timestamp, trim_vedba_smooth$corr_local_timestamp )
  identical( trim_gps_speeds_smooth$corr_local_timestamp, trim_vedba_smooth$corr_local_timestamp )
  
  
  ##### identification pipeline #####
  
  ## first with GPS
  
  window_size <- 10*60
  
  step_size <- 1*60
  
  start_time <- min( trim_gps_speeds_smooth$corr_local_timestamp )
  
  end_time <- max( trim_gps_speeds_smooth$corr_local_timestamp )
  
  window_start_times <- seq( from = start_time, to = ( end_time - window_size + 1 ), by = step_size )
  
  window_end_times <- seq( from = ( start_time + window_size - 1 ), to = end_time, by = step_size )
  
  tags <- names( trim_gps_speeds_smooth )[ names( trim_gps_speeds_smooth ) != 'corr_local_timestamp' ]
  
  #cor_array <- array( NA, dim = c( length( track_ids ), length( tags ), length( window_start_times ) ), dimnames = list( track_ids, tags, window_start_times ) )
  
  identity_scores_gps <- data.frame( tag = rep( tags, each = length( window_start_times ) ), window_start_time = rep( window_start_times, times = length( tags ) ), candidate_tracklet = NA, score = NA, tracklet_moved = NA )
  
  speed_thresh <- 0.2 # this is the speed in meters per second that a baboon needs to exceed to be considered moving 
  
  # 
  # 
  # ### coming back here to do a spot check:
  # 
  # time_to_check <- '2019-08-06 17:00:37'
  # 
  # tag <- '2433'
  # 
  # i <- which(  as.character( as.POSIXct( window_start_times, origin = '1970-01-01 00:00:00', tz = 'UTC' ) ) == time_to_check )
  #
  
  for( tag in tags ){
    
    print( tag ) 
    
    tag_dat <- trim_gps_speeds_smooth[ , tag ]
    
    for( i in 1:length( window_start_times ) ){
      
      score <- 0 
      
      tag_window_dat <- tag_dat[ trim_gps_speeds_smooth$corr_local_timestamp >= window_start_times[ i ] & trim_gps_speeds_smooth$corr_local_timestamp <= window_end_times[ i ] ]
      
      if( sum( !is.na( tag_window_dat ) ) > 0 ){ 
        
        tracklet_window_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= window_start_times[ i ] & trim_tracklet_speeds$local_timestamp <= window_end_times[ i ] , names( trim_tracklet_speeds ) != 'local_timestamp' ]
        
        cors <- apply( tracklet_window_dat, 2, FUN = function( x ) cor_function( x, tag_window_dat ) )
        
        #cor_array[ , tag, as.character( as.numeric( window_start_times[ i ] ) ) ] <- cors
        
        tracklet_lengths <- apply( tracklet_window_dat, MARGIN = 2, FUN = function( x ) sum( !is.na( x )  ) )
        
        longest_track <- max( tracklet_lengths )
        
        if( longest_track == 0 ){  # if there are not tracklets during this time window (that pretty much only happens when we have completed tracks at the beginning of the night and at the end of the night, but not in the middle of the night -- and now we are looking to match the tag with tracklets in the middle of the night), then skip this time window and move on to the next
          
          next 
          
        }
        
        inds_to_remove <- which( tracklet_lengths < 0.9*longest_track )
        
        full_tracklets_window <- tracklet_window_dat[ , - inds_to_remove ]
        
        rm( closest_tracklet ) # just to be safe
        
        if( 'numeric' %in% class( full_tracklets_window ) ){ # if there is only one tracklet that takes up the full window
          
          closest_tracklet <- names( which( tracklet_lengths == longest_track ) )
          
          correl <- cor_function( full_tracklets_window, tag_window_dat )
          
          if( is.na( correl ) ){
            
            next
            
          }
          
        }else{
          
          cors <- apply( full_tracklets_window, 2, FUN = function( x ) cor_function( x, tag_window_dat ) )
          
          # this needs to be here because sometimes there is tracklets that reach the duration requirements, but can still have little or no overlap with the tag data becase the tag data itself is quite lacking
          cors <- cors[ !is.na( cors ) ]
          
          if( sum( !is.na( cors ) ) == 0 ){
            
            next
            
          }
          
          closest_tracklet <- names( which.max( cors ) )
          
          if( length( cors ) >= 4 ){
            
            outlier_scores <- DescTools::LOF( cors, max( floor( length( cors ) / 3 ), 2 ) )
            
            cand_outlier_score <- outlier_scores[ names( cors ) == closest_tracklet ]
            
            score <- ifelse( cand_outlier_score > 2, score + 1, score )  
          }
          
        }
        
        identity_scores_gps[ identity_scores_gps$tag == tag & identity_scores_gps$window_start_time == window_start_times[ i ], 'candidate_tracklet' ] <- closest_tracklet
        
        ## subset to the tracklet data during this time window
        focal_tracklet_window_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= window_start_times[ i ] & trim_tracklet_speeds$local_timestamp <= window_end_times[ i ] , closest_tracklet ]
        
        tags_window_dat <- trim_gps_speeds_smooth[ trim_gps_speeds_smooth$corr_local_timestamp >= window_start_times[ i ] & trim_gps_speeds_smooth$corr_local_timestamp <= window_end_times[ i ] , names( trim_gps_speeds_smooth ) != 'corr_local_timestamp' ]
        
        focal_tracklet_window_dat[ !is.na( focal_tracklet_window_dat ) ]
        
        tracklet_cors <- apply( tags_window_dat, 2, FUN = function( x ) cor_function( x, focal_tracklet_window_dat ) )
        
        if( names( tracklet_cors )[ which.max( tracklet_cors ) ] == tag ){
          
          score <- score + 1
          
          tracklet_cors <- tracklet_cors[ !is.na( tracklet_cors ) ]
          
          if( length( tracklet_cors ) >= 4 ){
            
            outlier_scores <- DescTools::LOF( tracklet_cors, max( floor( length( tracklet_cors ) / 3 ), 2 ) )
            
            tag_outlier_score <- outlier_scores[ names( tracklet_cors ) == tag ]
            
            score <- ifelse( tag_outlier_score > 2, score + 1, score )  
            
          }
        }
        
        moved <- sum( focal_tracklet_window_dat > speed_thresh, na.rm = T )
        
        if( moved > 0 ){
          
          score <- score + 1
          
          identity_scores_gps[ identity_scores_gps$tag == tag & identity_scores_gps$window_start_time == window_start_times[ i ], 'tracklet_moved' ] <- 1
          
        }else{
          
          identity_scores_gps[ identity_scores_gps$tag == tag & identity_scores_gps$window_start_time == window_start_times[ i ], 'tracklet_moved' ] <- 0 # could just instantiate this column filled with 0s and then leave this else clause out, but this will be more likely to catch mistakes in the code, as there shouldn't be any Nas left
          
        }
        
        identity_scores_gps[ identity_scores_gps$tag == tag & identity_scores_gps$window_start_time == window_start_times[ i ], 'score' ] <- score
        
      }
    }
  }
  
  
  
  
  identity_scores_gps$window_start_time <- as.POSIXct( identity_scores_gps$window_start_time, origin = '1970-01-01', tz = 'UTC' )
  
  identity_scores_gps[ which( identity_scores_gps$score >= 2 ), ]
  
  dir.create( paste0( getwd(), '/DATA/thermal_tracks/tracklet_identification/' ) )
  
  dir.create( paste0( getwd(), '/DATA/thermal_tracks/tracklet_identification/', night_of_int ) )
  
  write.csv( identity_scores_gps, paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/identity_scores_gps.csv' ), row.names = F )
  
  
  
  window_size <- 10*60
  
  step_size <- 1*60
  
  start_time <- min( trim_vedba_smooth$corr_local_timestamp )
  
  end_time <- max( trim_vedba_smooth$corr_local_timestamp )
  
  window_start_times <- seq( from = start_time, to = ( end_time - window_size + 1 ), by = step_size )
  
  window_end_times <- seq( from = ( start_time + window_size - 1 ), to = end_time, by = step_size )
  
  tags <- names( trim_vedba_smooth )[ names( trim_vedba_smooth ) != 'corr_local_timestamp' ]
  
  identity_scores_cont_ved <- data.frame( tag = rep( tags, each = length( window_start_times ) ), window_start_time = rep( window_start_times, times = length( tags ) ), candidate_tracklet = NA, score = NA, tracklet_moved = NA )
  
  speed_thresh <- 0.2 # this is the speed in meters per second that a baboon needs to exceed to be considered moving 
  
  # 
  # 
  # ### coming back here to do a spot check:
  # 
  # time_to_check <- '2019-08-06 17:00:37'
  # 
  # tag <- '2433'
  # 
  # i <- which(  as.character( as.POSIXct( window_start_times, origin = '1970-01-01 00:00:00', tz = 'UTC' ) ) == time_to_check )
  #
  
  for( tag in tags ){
    
    print( tag ) 
    
    tag_dat <- trim_vedba_smooth[ , tag ]
    
    for( i in 1:length( window_start_times ) ){
      
      score <- 0 
      
      tag_window_dat <- tag_dat[ trim_vedba_smooth$corr_local_timestamp >= window_start_times[ i ] & trim_vedba_smooth$corr_local_timestamp <= window_end_times[ i ] ]
      
      if( sum( !is.na( tag_window_dat ) ) > 0 ){ 
        
        tracklet_window_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= window_start_times[ i ] & trim_tracklet_speeds$local_timestamp <= window_end_times[ i ] , names( trim_tracklet_speeds ) != 'local_timestamp' ]
        
        cors <- apply( tracklet_window_dat, 2, FUN = function( x ) cor_function( x, tag_window_dat ) )
        
        tracklet_lengths <- apply( tracklet_window_dat, MARGIN = 2, FUN = function( x ) sum( !is.na( x )  ) )
        
        longest_track <- max( tracklet_lengths )
        
        if( longest_track == 0 ){  # if there are not tracklets during this time window (that pretty much only happens when we have completed tracks at the beginning of the night and at the end of the night, but not in the middle of the night -- and now we are looking to match the tag with tracklets in the middle of the night), then skip this time window and move on to the next
          
          next 
          
        }
        
        inds_to_remove <- which( tracklet_lengths < 0.9*longest_track )
        
        full_tracklets_window <- tracklet_window_dat[ , - inds_to_remove ]
        
        rm( closest_tracklet ) # just to be safe
        
        if( 'numeric' %in% class( full_tracklets_window ) ){ # if there is only one tracklet that takes up the full window
          
          closest_tracklet <- names( which( tracklet_lengths == longest_track ) )
          
          correl <- cor_function( full_tracklets_window, tag_window_dat )
          
          if( is.na( correl ) ){
            
            next
            
          }
          
        }else{
          
          cors <- apply( full_tracklets_window, 2, FUN = function( x ) cor_function( x, tag_window_dat ) )
          
          
          # this needs to be here because sometimes there is tracklets that reach the duration requirements, but can still have little or no overlap with the tag data becase the tag data itself is quite lacking
          cors <- cors[ !is.na( cors ) ]
          
          if( sum( !is.na( cors ) ) == 0 ){
            
            next
            
          }
          
          closest_tracklet <- names( which.max( cors ) )
          
          if( length( cors ) >= 4 ){
            
            outlier_scores <- DescTools::LOF( cors, max( floor( length( cors ) / 3 ), 2 ) )
            
            cand_outlier_score <- outlier_scores[ names( cors ) == closest_tracklet ]
            
            score <- ifelse( cand_outlier_score > 2, score + 1, score )  
          }
          
        }
        
        identity_scores_cont_ved[ identity_scores_cont_ved$tag == tag & identity_scores_cont_ved$window_start_time == window_start_times[ i ], 'candidate_tracklet' ] <- closest_tracklet
        
        ## subset to the tracklet data during this time window
        focal_tracklet_window_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= window_start_times[ i ] & trim_tracklet_speeds$local_timestamp <= window_end_times[ i ] , closest_tracklet ]
        
        tags_window_dat <- trim_vedba_smooth[ trim_vedba_smooth$corr_local_timestamp >= window_start_times[ i ] & trim_vedba_smooth$corr_local_timestamp <= window_end_times[ i ] , names( trim_vedba_smooth ) != 'corr_local_timestamp' ]
        
        tracklet_cors <- apply( tags_window_dat, 2, FUN = function( x ) cor_function( x, focal_tracklet_window_dat ) )
        
        if( names( tracklet_cors )[ which.max( tracklet_cors ) ] == tag ){
          
          score <- score + 1
          
          tracklet_cors <- tracklet_cors[ !is.na( tracklet_cors ) ]
          
          if( length( tracklet_cors ) >= 4 ){
            
            outlier_scores <- DescTools::LOF( tracklet_cors, max( floor( length( tracklet_cors ) / 3 ), 2 ) )
            
            tag_outlier_score <- outlier_scores[ names( tracklet_cors ) == tag ]
            
            score <- ifelse( tag_outlier_score > 2, score + 1, score )  
            
          }
        }
        
        moved <- sum( focal_tracklet_window_dat > speed_thresh, na.rm = T )
        
        if( moved > 0 ){
          
          score <- score + 1
          
          identity_scores_cont_ved[ identity_scores_cont_ved$tag == tag & identity_scores_cont_ved$window_start_time == window_start_times[ i ], 'tracklet_moved' ] <- 1
          
        }else{
          
          identity_scores_cont_ved[ identity_scores_cont_ved$tag == tag & identity_scores_cont_ved$window_start_time == window_start_times[ i ], 'tracklet_moved' ] <- 0 # could just instantiate this column filled with 0s and then leave this else clause out, but this will be more likely to catch mistakes in the code, as there shouldn't be any Nas left
          
        }
        
        identity_scores_cont_ved[ identity_scores_cont_ved$tag == tag & identity_scores_cont_ved$window_start_time == window_start_times[ i ], 'score' ] <- score
        
      }
    }
  }
  
  
  identity_scores_cont_ved$window_start_time <- as.POSIXct( identity_scores_cont_ved$window_start_time, origin = '1970-01-01', tz = 'UTC' )
  
  identity_scores_cont_ved[ which( identity_scores_cont_ved$score >= 2 ), ]
  
  write.csv( identity_scores_cont_ved, paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/identity_scores_cont_ved.csv' ), row.names = F )
  
  window_size <- 10*60
  
  step_size <- 1*60
  
  start_time <- min( trim_burst_vedba$corr_local_timestamp )
  
  end_time <- max( trim_burst_vedba$corr_local_timestamp )
  
  window_start_times <- seq( from = start_time, to = ( end_time - window_size + 1 ), by = step_size )
  
  window_end_times <- seq( from = ( start_time + window_size - 1 ), to = end_time, by = step_size )
  
  tags <- names( trim_burst_vedba )[ names( trim_burst_vedba ) != 'corr_local_timestamp' ]
  
  identity_scores_burst_ved <- data.frame( tag = rep( tags, each = length( window_start_times ) ), window_start_time = rep( window_start_times, times = length( tags ) ), candidate_tracklet = NA, score = NA, tracklet_moved = NA )
  
  speed_thresh <- 0.2 # this is the speed in meters per second that a baboon needs to exceed to be considered moving 
  
  
  trim_tracklet_speeds_to_bursts <- tracklet_speeds[ tracklet_speeds$local_timestamp %in% trim_burst_vedba$corr_local_timestamp, ]
  
  for( tag in tags ){
    
    print( tag ) 
    
    tag_dat <- trim_burst_vedba[ , tag ]
    
    dat_times <- trim_burst_vedba$corr_local_timestamp[ !is.na( trim_burst_vedba[ , tag ] ) ] 
    
    ## rel_times is short for relevant times
    rel_times_smooth_tracks <- smooth_tracks[ smooth_tracks$local_timestamp %in% dat_times, ]
    
    for( i in 1:length( window_start_times ) ){
      
      score <- 0 
      
      tag_window_dat <- tag_dat[ trim_burst_vedba$corr_local_timestamp >= window_start_times[ i ] & trim_burst_vedba$corr_local_timestamp <= window_end_times[ i ] ]
      
      if( sum( !is.na( tag_window_dat ) ) > 0 ){ 
        
        tracklet_window_dat <- trim_tracklet_speeds_to_bursts[ trim_tracklet_speeds_to_bursts$local_timestamp >= window_start_times[ i ] & trim_tracklet_speeds_to_bursts$local_timestamp <= window_end_times[ i ] , names( trim_tracklet_speeds_to_bursts ) != 'local_timestamp' ]
        
        cors <- apply( tracklet_window_dat, 2, FUN = function( x ) cor_function( x, tag_window_dat ) )
        
        tracklet_lengths <- apply( tracklet_window_dat, MARGIN = 2, FUN = function( x ) sum( !is.na( x )  ) )
        
        longest_track <- max( tracklet_lengths )
        
        if( longest_track == 0 ){  # if there are not tracklets during this time window (that pretty much only happens when we have completed tracks at the beginning of the night and at the end of the night, but not in the middle of the night -- and now we are looking to match the tag with tracklets in the middle of the night), then skip this time window and move on to the next
          
          next 
          
        }
        
        inds_to_remove <- which( tracklet_lengths < 0.9*longest_track )
        
        full_tracklets_window <- tracklet_window_dat[ , - inds_to_remove ]
        
        rm( closest_tracklet ) # just to be safe
        
        if( 'numeric' %in% class( full_tracklets_window ) ){ # if there is only one tracklet that takes up the full window
          
          closest_tracklet <- names( which( tracklet_lengths == longest_track ) )
          
          correl <- cor_function( full_tracklets_window, tag_window_dat )
          
          if( is.na( correl ) ){
            
            next
            
          }
          
        }else{
          
          cors <- apply( full_tracklets_window, 2, FUN = function( x ) cor_function( x, tag_window_dat ) )
          
          # this needs to be here because sometimes there is tracklets that reach the duration requirements, but can still have little or no overlap with the tag data becase the tag data itself is quite lacking
          cors <- cors[ !is.na( cors ) ]
          
          if( sum( !is.na( cors ) ) == 0 ){
            
            next
            
          }
          
          closest_tracklet <- names( which.max( cors ) )
          
          if( length( cors ) >= 4 ){
            
            outlier_scores <- DescTools::LOF( cors, max( floor( length( cors ) / 3 ), 2 ) )
            
            cand_outlier_score <- outlier_scores[ names( cors ) == closest_tracklet ]
            
            score <- ifelse( cand_outlier_score > 2, score + 1, score )  
          }
          
        }
        
        identity_scores_burst_ved[ identity_scores_burst_ved$tag == tag & identity_scores_burst_ved$window_start_time == window_start_times[ i ], 'candidate_tracklet' ] <- closest_tracklet
        
        ## subset to the tracklet data during this time window
        focal_tracklet_window_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= window_start_times[ i ] & trim_tracklet_speeds$local_timestamp <= window_end_times[ i ] , closest_tracklet ]
        
        tags_window_dat <- trim_burst_vedba_one_sec[ trim_burst_vedba_one_sec$corr_local_timestamp >= window_start_times[ i ] & trim_burst_vedba_one_sec$corr_local_timestamp <= window_end_times[ i ] , names( trim_burst_vedba_one_sec ) != 'corr_local_timestamp' ]
        
        tracklet_cors <- apply( tags_window_dat, 2, FUN = function( x ) cor_function( x, focal_tracklet_window_dat ) )
        
        if( sum( !is.na( tracklet_cors ) ) > 0 ){
          
          if( names( tracklet_cors )[ which.max( tracklet_cors ) ] == tag ){
            
            score <- score + 1
            
            tracklet_cors <- tracklet_cors[ !is.na( tracklet_cors ) ]
            
            if( length( tracklet_cors ) >= 4 ){
              
              outlier_scores <- DescTools::LOF( tracklet_cors, max( floor( length( tracklet_cors ) / 3 ), 2 ) )
              
              tag_outlier_score <- outlier_scores[ names( tracklet_cors ) == tag ]
              
              score <- ifelse( tag_outlier_score > 2, score + 1, score )  
              
            }
          }
          
        }
        
        moved <- sum( rel_times_smooth_tracks[ rel_times_smooth_tracks$local_timestamp >= window_start_times[ i ] & rel_times_smooth_tracks$local_timestamp <= window_end_times[ i ] & rel_times_smooth_tracks$id == closest_tracklet, 'speed' ] > speed_thresh, na.rm = T )
        
        if( moved > 0 ){
          
          score <- score + 1
          
          identity_scores_burst_ved[ identity_scores_burst_ved$tag == tag & identity_scores_burst_ved$window_start_time == window_start_times[ i ], 'tracklet_moved' ] <- 1
          
        }else{
          
          identity_scores_burst_ved[ identity_scores_burst_ved$tag == tag & identity_scores_burst_ved$window_start_time == window_start_times[ i ], 'tracklet_moved' ] <- 0 # could just instantiate this column filled with 0s and then leave this else clause out, but this will be more likely to catch mistakes in the code, as there shouldn't be any Nas left
          
        }
        
        identity_scores_burst_ved[ identity_scores_burst_ved$tag == tag & identity_scores_burst_ved$window_start_time == window_start_times[ i ], 'score' ] <- score
        
      }
    }
  }
  
  identity_scores_burst_ved$window_start_time <- as.POSIXct( identity_scores_burst_ved$window_start_time, origin = '1970-01-01', tz = 'UTC' )
  
  identity_scores_burst_ved[ which( identity_scores_burst_ved$score >= 3 ), ]
  
  write.csv( identity_scores_burst_ved, paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/identity_scores_burst_ved.csv' ), row.names = F )
  
  
  # save.image( paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/tracklet_id_dat.RData' )  )
  # 
  # load( paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int,  '/tracklet_id_dat.RData' )  )
  # 
  
  ####### assigning identities with burst ved ##########
  
  # a score equal to or greater than the score_thresh will be considered as contributing the identity assignment
  score_thresh <- 3
  
  # as soon as an tag pairs with a tracklet with a score higher than the score_thresh for number_needed different times (within the consec window), we will assign the tracklet as the identity of the tag in the thermal imagery. Note the number_needed will get reset after an identity gets assigned
  number_needed <- 3
  
  # consec_window is the amount of time (in seconds) within which the number_needed of instances of the score_thresh being exceeded needs to be reached to in order to assign a tracklet identity to a tag (just make this really large if you don't really want to limit it). This is good to set just to prevent tracklets randomly accumulating scores over the score_thresh and then eventually reaching the number_needed
  consec_window <- 20*60 # this parameter seems to have little affect
  
  tags <- as.character( unique( identity_scores_burst_ved$tag ) ) 
  
  ident_dat_burst_ved <- as.data.frame( matrix( NA, nrow = 0, ncol = 3 ) )
  
  ## identity start time will represent the time at which the thresholds above were reached. So it is not the first time the tracklet and the tag show a good match, but rather, the time at which they have first shown a sustained match within a given time period
  names( ident_dat_burst_ved ) <- c( 'tag', 'ident_start_time', 'tracklet' )
  
  options( warn = 0 )
  
  for( tag in tags ){
    
    switch <- 0
    
    tag_dat <- identity_scores_burst_ved[ identity_scores_burst_ved$tag == tag, ]
    
    for( i in 1:nrow( tag_dat ) ){
      
      if( switch == 0 ){
        
        inds_in_play <- which( tag_dat$window_start_time > ( tag_dat$window_start_time[ i ] - consec_window ) )
        
      }else{
        
        inds_in_play <- which( tag_dat$window_start_time > ( tag_dat$window_start_time[ i ] - consec_window ) & tag_dat$window_start_time > id_time )
      }
      
      window_dat <- tag_dat[ min( inds_in_play ):i, ]
      
      high_score_dat <- window_dat[ which( window_dat$score >= score_thresh ), ]
      
      num_instances_tab <- table( high_score_dat$candidate_tracklet )             
      
      if( sum( num_instances_tab >= number_needed ) > 0  ){
        
        id_tracklets <- names( num_instances_tab )[ which( num_instances_tab >= number_needed ) ]
        
        for( id_tracklet in id_tracklets ){
          
          tracklet_dat <- high_score_dat[ high_score_dat$candidate_tracklet == id_tracklet, ]
          
          if( sum( tracklet_dat$tracklet_moved ) > 0 ){ ###### TAKE OUT THE EQUALS SIGN IF YOU WANT THE CONDITION THAT IS HAS TO BE MOVING TO 
            
            switch <- 1
            
            id_time <- tracklet_dat$window_start_time[ nrow( tracklet_dat ) ]
            
            ident_dat_burst_ved <- rbind( ident_dat_burst_ved, data.frame( tag = tag, ident_start_time = id_time, tracklet = id_tracklet ) )
            
            break
          }
          
        }
        
      }
      
    }
    
  }
  
  
  
  ident_dat_burst_ved 
  
  length( unique( ident_dat_burst_ved$tag ) )
  
  if( nrow( ident_dat_burst_ved ) != 0 ){
    
    ## remove the duplicates (aka consecutive reindification of the same tracklet)
    rem_inds <- c()
    
    if( nrow( ident_dat_burst_ved ) > 1 ){
      
      for( i in 2:nrow( ident_dat_burst_ved ) ){
        
        if( ident_dat_burst_ved$tag[ i ] == ident_dat_burst_ved$tag[ i - 1 ] & ident_dat_burst_ved$tracklet[ i ] == ident_dat_burst_ved$tracklet[ i - 1 ] ){
          
          rem_inds <- c( rem_inds, i )
          
        }
        
      }
      
      if( length( rem_inds ) != 0 ){
        
        ident_dat_burst_ved <- ident_dat_burst_ved[ - rem_inds, ]
        
      }
      
    }

    
    ## determine the end time of the identification. This is either the end time of the tracklet or the time when the identification switches to a different tracklet
    ident_dat_burst_ved$ident_end_time <- NA
    
    tags <- as.character( unique( ident_dat_burst_ved$tag ) )
    
    for( tag in tags ){
      
      tag_dat <- ident_dat_burst_ved[ ident_dat_burst_ved$tag == tag, ]
      
      for( i in 1:nrow( tag_dat ) ){
        
        target_tracklet <- tag_dat$tracklet[ i ]
        
        tracklet_end <- max( smooth_tracks[ smooth_tracks$id == target_tracklet, 'local_timestamp' ] )
        
        if( i == nrow( tag_dat ) ){
          
          tag_dat$ident_end_time[ i ] <- tracklet_end
          
        }else{
          
          next_tracklet_start <- tag_dat$ident_start[ i + 1 ]
          
          tag_dat$ident_end_time[ i ] <- min( tracklet_end, next_tracklet_start )
          
        }
        
      }
      
      ident_dat_burst_ved[ ident_dat_burst_ved$tag == tag, ] <- tag_dat
      
    }
    
    ident_dat_burst_ved$ident_end_time <- as.POSIXct( ident_dat_burst_ved$ident_end_time, origin = '1970-01-01 00:00:00', tz = 'UTC' )
    
    
    ## solve conflicts over two individuals wanting the same tracklet
    dup_tracklets <- as.character( unique( ident_dat_burst_ved$tracklet[ duplicated( ident_dat_burst_ved$tracklet ) ] ) )
    
    for( tracklet in dup_tracklets ){
      
      tracklet_dat <- ident_dat_burst_ved[ ident_dat_burst_ved$tracklet == tracklet, ]
      
      if( length( unique( tracklet_dat$tag ) ) != 1 ){
        
        # remove the data from the ident_dat_burst_ved dataframe
        to_remove <- apply( tracklet_dat, 1 , paste, collapse = '' )
        
        removed_dat <- ident_dat_burst_ved[ apply( ident_dat_burst_ved, 1 , paste, collapse = '' ) %in% to_remove, ]
          
        ident_dat_burst_ved <- ident_dat_burst_ved[ ! apply( ident_dat_burst_ved, 1 , paste, collapse = '' ) %in% to_remove, ]
        
        replacement_dat <- tracklet_dat[ 0, ]
        
        times <- vector( mode = 'list' )
        
        earliest_time <- min( tracklet_dat$ident_start_time )
        
        latest_time <- max( tracklet_dat$ident_end_time )
        
        whole_ts <- seq( earliest_time, latest_time, by = '1 sec' )
        
        contested_score <- rep( 0, length( whole_ts ) )
        
        for( i in 1:nrow( tracklet_dat) ){
          
          times[[ i ]] <- seq( tracklet_dat$ident_start_time[ i ], tracklet_dat$ident_end_time[ i ], by = '1 sec' )
          
          contested_score <- contested_score + whole_ts %in% times[[ i ]]
          
        }
        
        contested_times <- whole_ts[ contested_score > 1 ]
        
        if( length( contested_times ) == 0 ){ # if there is actually no overlap in the times, then just add the removed rows back to the dataframe and move on to the next duplicate
          
          ident_dat_burst_ved <- rbind( ident_dat_burst_ved, removed_dat )
          
        }else{
          
          period <- c( 0, cumsum( diff( contested_times ) != 1 )  ) + 1 
          
          un_periods <- as.numeric( as.character( unique( period ) ) )
          
          for( per in un_periods ){
            
            contested_per <- contested_times[ period == per ]
            
            contestant_inds <- which( sapply( times, FUN = function( x ) sum( contested_per %in% x ) > 0 ) )
            
            order_of_go_scores <- c()
            
            for( j in contestant_inds ){
              
              start_window_comp <- tracklet_dat$ident_start_time[ j ]
              
              end_window_comp <- tracklet_dat$ident_end_time[ j ]
              
              ## find the correlation between this individual and the tracklet for the contested period (using the one sec)
              tracklet_per_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= start_window_comp & trim_tracklet_speeds$local_timestamp <= end_window_comp, tracklet ]
              
              tag_per_dat <- trim_burst_vedba_one_sec[ trim_burst_vedba_one_sec$corr_local_timestamp >= start_window_comp & trim_burst_vedba_one_sec$corr_local_timestamp <= end_window_comp, tracklet_dat$tag[ j ] ]
              
              final_corr <- cor_function( tracklet_per_dat, tag_per_dat )
              
              order_of_go_scores <- c( order_of_go_scores, final_corr )
              
            }
            
            order_of_go <- contestant_inds[ order( order_of_go_scores, decreasing = T ) ] 
            
            already_taken <- c()
            
            for( ind in order_of_go ){
              
              desired_times <- times[[ ind ]]
              
              confirmed_times <- desired_times[ !desired_times %in% already_taken ]
              
              times[[ ind ]] <- confirmed_times
              
              times_removed <- contested_per[ contested_per %in% confirmed_times ]
              
              already_taken <- c( already_taken, times_removed )
              
            }
          }
          
          for( k in 1:length( times ) ){
            
            time_vec <- sort( times[[ k ]] )
            
            if( length( time_vec ) != 0 ){
              
              diffs <- c( as.numeric( diff( time_vec ), unit = 'secs' ) )
              
              end_inds <- which( diffs > 1 ) 
              
              id_start_time_inds <- c( 1, end_inds + 1 ) 
              
              id_end_time_inds <- c( end_inds, length( time_vec ) )
              
              replacement_dat <- rbind( replacement_dat, data.frame( tag = tracklet_dat$tag[ k ], ident_start_time = time_vec[ id_start_time_inds ], tracklet = tracklet, ident_end_time = time_vec[ id_end_time_inds ] ) )
              
            }
          }
          
          ident_dat_burst_ved <- rbind( ident_dat_burst_ved, replacement_dat )
          
        }
      }
    }
  }
  
  
  write.csv( ident_dat_burst_ved,  paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/ident_dat_burst_ved.csv' ), row.names = F )
  
  
  ########### assigning identities with continuous vedba #############
  
  
  # a score equal to or greater than the score_thresh will be considered as contributing the identity assignment
  score_thresh <- 2
  
  # as soon as an tag pairs with a tracklet with a score higher than the score_thresh for number_needed different times (within the consec window), we will assign the tracklet as the identity of the tag in the thermal imagery. Note the number_needed will get reset after an identity gets assigned
  number_needed <- 3
  
  # consec_window is the amount of time (in seconds) within which the number_needed of instances of the score_thresh being exceeded needs to be reached to in order to assign a tracklet identity to a tag (just make this really large if you don't really want to limit it). This is good to set just to prevent tracklets randomly accumulating scores over the score_thresh and then eventually reaching the number_needed
  consec_window <- 40*60 # this parameter seems to have little affect
  
  tags <- as.character( unique( identity_scores_cont_ved$tag ) ) 
  
  ident_dat_cont_ved <- as.data.frame( matrix( NA, nrow = 0, ncol = 3 ) )
  
  ## identity start time will represent the time at which the thresholds above were reached. So it is not the first time the tracklet and the tag show a good match, but rather, the time at which they have first shown a sustained match within a given time period
  names( ident_dat_cont_ved ) <- c( 'tag', 'ident_start_time', 'tracklet' )
  
  options( warn = 0 )
  for( tag in tags ){
    
    switch <- 0
    
    tag_dat <- identity_scores_cont_ved[ identity_scores_cont_ved$tag == tag, ]
    
    for( i in 1:nrow( tag_dat ) ){
      
      if( switch == 0 ){
        
        inds_in_play <- which( tag_dat$window_start_time > ( tag_dat$window_start_time[ i ] - consec_window ) )
        
      }else{
        
        inds_in_play <- which( tag_dat$window_start_time > ( tag_dat$window_start_time[ i ] - consec_window ) & tag_dat$window_start_time > id_time )
      }
      
      window_dat <- tag_dat[ min( inds_in_play ):i, ]
      
      high_score_dat <- window_dat[ which( window_dat$score >= score_thresh ), ]
      
      num_instances_tab <- table( high_score_dat$candidate_tracklet )             
      
      if( sum( num_instances_tab >= number_needed ) > 0  ){
        
        id_tracklets <- names( num_instances_tab )[ which( num_instances_tab >= number_needed ) ]
        
        for( id_tracklet in id_tracklets ){
          
          tracklet_dat <- high_score_dat[ high_score_dat$candidate_tracklet == id_tracklet, ]
          
          if( sum( tracklet_dat$tracklet_moved ) > 0 ){ ###### TAKE OUT THE EQUALS SIGN IF YOU WANT THE CONDITION THAT IS HAS TO BE MOVING TO 
            
            switch <- 1
            
            id_time <- tracklet_dat$window_start_time[ nrow( tracklet_dat ) ]
            
            ident_dat_cont_ved <- rbind( ident_dat_cont_ved, data.frame( tag = tag, ident_start_time = id_time, tracklet = id_tracklet ) )
            
            break
          }
          
        }
        
      }
      
    }
    
  }
  
  
  
  ident_dat_cont_ved 
  
  length( unique( ident_dat_cont_ved$tag ) )
  
  if( nrow( ident_dat_cont_ved ) != 0 ){
    
    ## remove the duplicates (aka consecutive reidentification of the same tracklet)
    rem_inds <- c()
    
    if( nrow( ident_dat_cont_ved ) > 1 ){
      
      for( i in 2:nrow( ident_dat_cont_ved ) ){
        
        if( ident_dat_cont_ved$tag[ i ] == ident_dat_cont_ved$tag[ i - 1 ] & ident_dat_cont_ved$tracklet[ i ] == ident_dat_cont_ved$tracklet[ i - 1 ] ){
          
          rem_inds <- c( rem_inds, i )
          
        }
        
      }
      
      if( length( rem_inds ) != 0 ){
        
        ident_dat_cont_ved <- ident_dat_cont_ved[ - rem_inds, ]
        
      }
      
    }
    
    ## determine the end time of the identification. This is either the end time of the tracklet or the time when the identification switches to a different tracklet
    ident_dat_cont_ved$ident_end_time <- NA
    
    tags <- as.character( unique( ident_dat_cont_ved$tag ) )
    
    for( tag in tags ){
      
      tag_dat <- ident_dat_cont_ved[ ident_dat_cont_ved$tag == tag, ]
      
      for( i in 1:nrow( tag_dat ) ){
        
        target_tracklet <- tag_dat$tracklet[ i ]
        
        tracklet_end <- max( smooth_tracks[ smooth_tracks$id == target_tracklet, 'local_timestamp' ] )
        
        if( i == nrow( tag_dat ) ){
          
          tag_dat$ident_end_time[ i ] <- tracklet_end
          
        }else{
          
          next_tracklet_start <- tag_dat$ident_start[ i + 1 ]
          
          tag_dat$ident_end_time[ i ] <- min( tracklet_end, next_tracklet_start )
          
        }
        
      }
      
      ident_dat_cont_ved[ ident_dat_cont_ved$tag == tag, ] <- tag_dat
      
    }
    
    ident_dat_cont_ved$ident_end_time <- as.POSIXct( ident_dat_cont_ved$ident_end_time, origin = '1970-01-01 00:00:00', tz = 'UTC' )
    
    
    ## solve conflicts over two individuals wanting the same tracklet
    dup_tracklets <- as.character( unique( ident_dat_cont_ved$tracklet[ duplicated( ident_dat_cont_ved$tracklet ) ] ) )
    
    for( tracklet in dup_tracklets ){
      
      tracklet_dat <- ident_dat_cont_ved[ ident_dat_cont_ved$tracklet == tracklet, ]
      
      if( length( unique( tracklet_dat$tag ) ) != 1 ){
        
        # remove the data from the ident_dat_cont_ved dataframe
        to_remove <- apply( tracklet_dat, 1 , paste, collapse = '' )
        
        removed_dat <- ident_dat_cont_ved[ apply( ident_dat_cont_ved, 1 , paste, collapse = '' ) %in% to_remove, ]
        
        ident_dat_cont_ved <- ident_dat_cont_ved[ ! apply( ident_dat_cont_ved, 1 , paste, collapse = '' ) %in% to_remove, ]
        
        replacement_dat <- tracklet_dat[ 0, ]
        
        times <- vector( mode = 'list' )
        
        earliest_time <- min( tracklet_dat$ident_start_time )
        
        latest_time <- max( tracklet_dat$ident_end_time )
        
        whole_ts <- seq( earliest_time, latest_time, by = '1 sec' )
        
        contested_score <- rep( 0, length( whole_ts ) )
        
        for( i in 1:nrow( tracklet_dat) ){
          
          times[[ i ]] <- seq( tracklet_dat$ident_start_time[ i ], tracklet_dat$ident_end_time[ i ], by = '1 sec' )
          
          contested_score <- contested_score + whole_ts %in% times[[ i ]]
          
        }
        
        contested_times <- whole_ts[ contested_score > 1 ]
        
        if( length( contested_times ) == 0 ){ # if there is actually no overlap in the times, then just add the removed rows back to the dataframe and move on to the next duplicate
          
          ident_dat_cont_ved <- rbind( ident_dat_cont_ved, removed_dat )
          
        }else{
          
          
          period <- c( 0, cumsum( diff( contested_times ) != 1 )  ) + 1 
          
          un_periods <- as.numeric( as.character( unique( period ) ) )
          
          for( per in un_periods ){
            
            contested_per <- contested_times[ period == per ]
            
            contestant_inds <- which( sapply( times, FUN = function( x ) sum( contested_per %in% x ) > 0 ) )
            
            order_of_go_scores <- c()
            
            for( j in contestant_inds ){
              
              start_window_comp <- tracklet_dat$ident_start_time[ j ]
              
              end_window_comp <- tracklet_dat$ident_end_time[ j ]
              
              ## find the correlation between this individual and the tracklet for the contested period (using the one sec)
              tracklet_per_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= start_window_comp & trim_tracklet_speeds$local_timestamp <= end_window_comp, tracklet ]
              
              tag_per_dat <- trim_vedba_smooth[ trim_vedba_smooth$corr_local_timestamp >= start_window_comp & trim_vedba_smooth$corr_local_timestamp <= end_window_comp, tracklet_dat$tag[ j ] ]
              
              final_corr <- cor_function( tracklet_per_dat, tag_per_dat )
              
              order_of_go_scores <- c( order_of_go_scores, final_corr )
              
            }
            
            order_of_go <- contestant_inds[ order( order_of_go_scores, decreasing = T ) ] 
            
            already_taken <- c()
            
            for( ind in order_of_go ){
              
              desired_times <- times[[ ind ]]
              
              confirmed_times <- desired_times[ !desired_times %in% already_taken ]
              
              times[[ ind ]] <- confirmed_times
              
              times_removed <- contested_per[ contested_per %in% confirmed_times ]
              
              already_taken <- c( already_taken, times_removed )
              
            }
          }
          
          for( k in 1:length( times ) ){
            
            time_vec <- sort( times[[ k ]] )
            
            if( length( time_vec ) != 0 ){
              
              diffs <- c( as.numeric( diff( time_vec ), unit = 'secs' ) )
              
              end_inds <- which( diffs > 1 ) 
              
              id_start_time_inds <- c( 1, end_inds + 1 ) 
              
              id_end_time_inds <- c( end_inds, length( time_vec ) )
              
              replacement_dat <- rbind( replacement_dat, data.frame( tag = tracklet_dat$tag[ k ], ident_start_time = time_vec[ id_start_time_inds ], tracklet = tracklet, ident_end_time = time_vec[ id_end_time_inds ] ) )
              
            }
          }
          
          ident_dat_cont_ved <- rbind( ident_dat_cont_ved, replacement_dat )
          
        }
      }
    }
  }
  
  
  ident_dat_cont_ved
  
  write.csv( ident_dat_cont_ved,  paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/ident_dat_cont_ved.csv' ), row.names = F )
  
  
  ########## assigning identities with GPS speeds #########
  
  
  # a score equal to or greater than the score_thresh will be considered as contributing the identity assignment
  score_thresh <- 3
  
  # as soon as an tag pairs with a tracklet with a score higher than the score_thresh for number_needed different times (within the consec window), we will assign the tracklet as the identity of the tag in the thermal imagery. Note the number_needed will get reset after an identity gets assigned
  number_needed <- 3
  
  # consec_window is the amount of time (in seconds) within which the number_needed of instances of the score_thresh being exceeded needs to be reached to in order to assign a tracklet identity to a tag (just make this really large if you don't really want to limit it). This is good to set just to prevent tracklets randomly accumulating scores over the score_thresh and then eventually reaching the number_needed
  consec_window <- 40*60 # this parameter seems to have little affect
  
  tags <- as.character( unique( identity_scores_gps$tag ) ) 
  
  ident_dat_gps_speed <- as.data.frame( matrix( NA, nrow = 0, ncol = 3 ) )
  
  ## identity start time will represent the time at which the thresholds above were reached. So it is not the first time the tracklet and the tag show a good match, but rather, the time at which they have first shown a sustained match within a given time period
  names( ident_dat_gps_speed ) <- c( 'tag', 'ident_start_time', 'tracklet' )
  
  options( warn = 0 )
  for( tag in tags ){
    
    switch <- 0
    
    tag_dat <- identity_scores_gps[ identity_scores_gps$tag == tag, ]
    
    for( i in 1:nrow( tag_dat ) ){
      
      if( switch == 0 ){
        
        inds_in_play <- which( tag_dat$window_start_time > ( tag_dat$window_start_time[ i ] - consec_window ) )
        
      }else{
        
        inds_in_play <- which( tag_dat$window_start_time > ( tag_dat$window_start_time[ i ] - consec_window ) & tag_dat$window_start_time > id_time )
      }
      
      window_dat <- tag_dat[ min( inds_in_play ):i, ]
      
      high_score_dat <- window_dat[ which( window_dat$score >= score_thresh ), ]
      
      num_instances_tab <- table( high_score_dat$candidate_tracklet )             
      
      if( sum( num_instances_tab >= number_needed ) > 0  ){
        
        id_tracklets <- names( num_instances_tab )[ which( num_instances_tab >= number_needed ) ]
        
        for( id_tracklet in id_tracklets ){
          
          tracklet_dat <- high_score_dat[ high_score_dat$candidate_tracklet == id_tracklet, ]
          
          if( sum( tracklet_dat$tracklet_moved ) > 0 ){ ###### TAKE OUT THE EQUALS SIGN IF YOU WANT THE CONDITION THAT IS HAS TO BE MOVING TO 
            
            switch <- 1
            
            id_time <- tracklet_dat$window_start_time[ nrow( tracklet_dat ) ]
            
            ident_dat_gps_speed <- rbind( ident_dat_gps_speed, data.frame( tag = tag, ident_start_time = id_time, tracklet = id_tracklet ) )
            
            break
          }
          
        }
        
      }
      
    }
    
  }
  
  
  
  ident_dat_gps_speed 
  
  length( unique( ident_dat_gps_speed$tag ) )
  
  if( nrow( ident_dat_gps_speed != 0 ) ){
    
    ## remove the duplicates (aka consecutive reindification of the same tracklet)
    rem_inds <- c()
    
    if( nrow( ident_dat_gps_speed ) > 1 ){
      
      for( i in 2:nrow( ident_dat_gps_speed ) ){
        
        if( ident_dat_gps_speed$tag[ i ] == ident_dat_gps_speed$tag[ i - 1 ] & ident_dat_gps_speed$tracklet[ i ] == ident_dat_gps_speed$tracklet[ i - 1 ] ){
          
          rem_inds <- c( rem_inds, i )
          
        }
        
      }
      
      
      if( length( rem_inds ) != 0 ){
        
        ident_dat_gps_speed <- ident_dat_gps_speed[ - rem_inds, ]
        
      }
      
    }
    
    ## determine the end time of the identification. This is either the end time of the tracklet or the time when the identification switches to a different tracklet
    ident_dat_gps_speed$ident_end_time <- NA
    
    tags <- as.character( unique( ident_dat_gps_speed$tag ) )
    
    for( tag in tags ){
      
      tag_dat <- ident_dat_gps_speed[ ident_dat_gps_speed$tag == tag, ]
      
      for( i in 1:nrow( tag_dat ) ){
        
        target_tracklet <- tag_dat$tracklet[ i ]
        
        tracklet_end <- max( smooth_tracks[ smooth_tracks$id == target_tracklet, 'local_timestamp' ] )
        
        if( i == nrow( tag_dat ) ){
          
          tag_dat$ident_end_time[ i ] <- tracklet_end
          
        }else{
          
          next_tracklet_start <- tag_dat$ident_start[ i + 1 ]
          
          tag_dat$ident_end_time[ i ] <- min( tracklet_end, next_tracklet_start )
          
        }
        
      }
      
      ident_dat_gps_speed[ ident_dat_gps_speed$tag == tag, ] <- tag_dat
      
    }
    
    ident_dat_gps_speed$ident_end_time <- as.POSIXct( ident_dat_gps_speed$ident_end_time, origin = '1970-01-01 00:00:00', tz = 'UTC' )
    
    
    ## solve conflicts over two individuals wanting the same tracklet
    dup_tracklets <- as.character( unique( ident_dat_gps_speed$tracklet[ duplicated( ident_dat_gps_speed$tracklet ) ] ) )
    
    for( tracklet in dup_tracklets ){
      
      tracklet_dat <- ident_dat_gps_speed[ ident_dat_gps_speed$tracklet == tracklet, ]
      
      if( length( unique( tracklet_dat$tag ) ) != 1 ){
        
        # remove the data from the ident_dat_gps_speed dataframe
        to_remove <- apply( tracklet_dat, 1 , paste, collapse = '' )
        
        removed_dat <- ident_dat_gps_speed[ apply( ident_dat_gps_speed, 1 , paste, collapse = '' ) %in% to_remove, ]
        
        ident_dat_gps_speed <- ident_dat_gps_speed[ ! apply( ident_dat_gps_speed, 1 , paste, collapse = '' ) %in% to_remove, ]
        
        replacement_dat <- tracklet_dat[ 0, ]
        
        times <- vector( mode = 'list' )
        
        earliest_time <- min( tracklet_dat$ident_start_time )
        
        latest_time <- max( tracklet_dat$ident_end_time )
        
        whole_ts <- seq( earliest_time, latest_time, by = '1 sec' )
        
        contested_score <- rep( 0, length( whole_ts ) )
        
        for( i in 1:nrow( tracklet_dat) ){
          
          times[[ i ]] <- seq( tracklet_dat$ident_start_time[ i ], tracklet_dat$ident_end_time[ i ], by = '1 sec' )
          
          contested_score <- contested_score + whole_ts %in% times[[ i ]]
          
        }
        
        contested_times <- whole_ts[ contested_score > 1 ]
        
        if( length( contested_times ) == 0 ){ # if there is actually no overlap in the times, then just add the removed rows back to the dataframe and move on to the next duplicate
          
          ident_dat_gps_speed <- rbind( ident_dat_gps_speed, removed_dat )
          
        }else{
          
          period <- c( 0, cumsum( diff( contested_times ) != 1 )  ) + 1 
          
          un_periods <- as.numeric( as.character( unique( period ) ) )
          
          for( per in un_periods ){
            
            contested_per <- contested_times[ period == per ]
            
            contestant_inds <- which( sapply( times, FUN = function( x ) sum( contested_per %in% x ) > 0 ) )
            
            order_of_go_scores <- c()
            
            for( j in contestant_inds ){
              
              start_window_comp <- tracklet_dat$ident_start_time[ j ]
              
              end_window_comp <- tracklet_dat$ident_end_time[ j ]
              
              ## find the correlation between this individual and the tracklet for the contested period (using the one sec)
              tracklet_per_dat <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= start_window_comp & trim_tracklet_speeds$local_timestamp <= end_window_comp, tracklet ]
              
              tag_per_dat <- trim_gps_speeds_smooth[ trim_gps_speeds_smooth$corr_local_timestamp >= start_window_comp & trim_gps_speeds_smooth$corr_local_timestamp <= end_window_comp, tracklet_dat$tag[ j ] ]
              
              final_corr <- cor_function( tracklet_per_dat, tag_per_dat )
              
              order_of_go_scores <- c( order_of_go_scores, final_corr )
              
            }
            
            order_of_go <- contestant_inds[ order( order_of_go_scores, decreasing = T ) ] 
            
            already_taken <- c()
            
            for( ind in order_of_go ){
              
              desired_times <- times[[ ind ]]
              
              confirmed_times <- desired_times[ !desired_times %in% already_taken ]
              
              times[[ ind ]] <- confirmed_times
              
              times_removed <- contested_per[ contested_per %in% confirmed_times ]
              
              already_taken <- c( already_taken, times_removed )
              
            }
          }
          
          for( k in 1:length( times ) ){
            
            time_vec <- sort( times[[ k ]] )
            
            if( length( time_vec ) != 0 ){
              
              diffs <- c( as.numeric( diff( time_vec ), unit = 'secs' ) )
              
              end_inds <- which( diffs > 1 ) 
              
              id_start_time_inds <- c( 1, end_inds + 1 ) 
              
              id_end_time_inds <- c( end_inds, length( time_vec ) )
              
              replacement_dat <- rbind( replacement_dat, data.frame( tag = tracklet_dat$tag[ k ], ident_start_time = time_vec[ id_start_time_inds ], tracklet = tracklet, ident_end_time = time_vec[ id_end_time_inds ] ) )
              
            }
          }
          
          ident_dat_gps_speed <- rbind( ident_dat_gps_speed, replacement_dat )
        }
      }
    }
    
    ident_dat_gps_speed
    
  }
  
  write.csv( ident_dat_gps_speed,  paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/ident_dat_gps_speed.csv' ), row.names = F )
  
  
  smooth_tracks$tag <- NA
  
  if( nrow( ident_dat_gps_speed ) != 0 ){
    
    for( i in 1:nrow( ident_dat_gps_speed ) ){
      
      smooth_tracks$tag[ which( smooth_tracks$local_timestamp >= ident_dat_gps_speed$ident_start_time[ i ] & smooth_tracks$local_timestamp < ident_dat_gps_speed$ident_end_time[ i ] & smooth_tracks$id == ident_dat_gps_speed$tracklet[ i ] ) ] <- ident_dat_gps_speed$tag[ i ]
      
    }
    
  }
  
  if( nrow( ident_dat_burst_ved ) != 0 ){
    
    for( i in 1:nrow( ident_dat_burst_ved ) ){
      
      smooth_tracks$tag[ which( smooth_tracks$local_timestamp >= ident_dat_burst_ved$ident_start_time[ i ] & smooth_tracks$local_timestamp < ident_dat_burst_ved$ident_end_time[ i ] & smooth_tracks$id == ident_dat_burst_ved$tracklet[ i ] ) ] <- ident_dat_burst_ved$tag[ i ]
      
  
      #smooth_tracks$tag[ which( smooth_tracks$local_timestamp >= ident_dat_gps_speed$ident_start_time[ i ] & smooth_tracks$local_timestamp <= ident_dat_gps_speed$ident_end_time[ i ] & smooth_tracks$id != ident_dat_gps_speed$tracklet[ i ] & smooth_tracks$tag == ident_dat_gps_speed$tag[ i ] ) ] <- ident_dat_gps_speed$tag[ i ]
      
      ## remove other identifications that have been made of this tag at the same time (by previous identification method (i.e. the gps speeds))
      
      if( length( which( smooth_tracks$local_timestamp >= ident_dat_burst_ved$ident_start_time[ i ] & smooth_tracks$local_timestamp < ident_dat_burst_ved$ident_end_time[ i ] & smooth_tracks$id != ident_dat_burst_ved$tracklet[ i ] & smooth_tracks$tag == ident_dat_burst_ved$tag[ i ] ) ) > 0  ){
        
        smooth_tracks$tag[ which( smooth_tracks$local_timestamp >= ident_dat_burst_ved$ident_start_time[ i ] & smooth_tracks$local_timestamp < ident_dat_burst_ved$ident_end_time[ i ] & smooth_tracks$id != ident_dat_burst_ved$tracklet[ i ] & smooth_tracks$tag == ident_dat_burst_ved$tag[ i ] ) ] <- NA
       
         
      }
    }
    
  }
  
  if( nrow( ident_dat_cont_ved ) != 0 ){
    
    for( i in 1:nrow( ident_dat_cont_ved ) ){
      
      smooth_tracks$tag[ which( smooth_tracks$local_timestamp >= ident_dat_cont_ved$ident_start_time[ i ] & smooth_tracks$local_timestamp < ident_dat_cont_ved$ident_end_time[ i ] & smooth_tracks$id == ident_dat_cont_ved$tracklet[ i ] ) ] <- ident_dat_cont_ved$tag[ i ]
      
      ## remove other identifications that have been made of this tag at the same time (by previous identification method (i.e. the gps speeds, or burst vedba))
      
      if( length( which( smooth_tracks$local_timestamp >= ident_dat_cont_ved$ident_start_time[ i ] & smooth_tracks$local_timestamp < ident_dat_cont_ved$ident_end_time[ i ] & smooth_tracks$id != ident_dat_cont_ved$tracklet[ i ] & smooth_tracks$tag == ident_dat_cont_ved$tag[ i ] ) ) > 0  ){
        
        smooth_tracks$tag[ which( smooth_tracks$local_timestamp >= ident_dat_cont_ved$ident_start_time[ i ] & smooth_tracks$local_timestamp < ident_dat_cont_ved$ident_end_time[ i ] & smooth_tracks$id != ident_dat_cont_ved$tracklet[ i ] & smooth_tracks$tag == ident_dat_cont_ved$tag[ i ] ) ] <- NA
        
      }
      
    }
    
  }
  
  saveRDS( smooth_tracks, paste0( "DATA/thermal_tracks/identified_tracks/", input_data, '/', night_of_int, "_ident_tracks.rds" ) )
  
  return( smooth_tracks )
  
}


stopImplicitCluster()

