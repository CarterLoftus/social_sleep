



library( raster )
library( doParallel )
library( foreach )
library( igraph )
library( plyr )


files_to_read <- list.files( path = paste0( "DATA/thermal_tracks/smooth_tracks/corrected" ), full.names = T )

final_tracks_list <- lapply( files_to_read, readRDS )



# how much tracking data did we have in the end

full_tracks_df <- ldply( final_tracks_list )
length( unique( full_tracks_df$local_timestamp ) )/60/60


spat_disc_thresh <- 1


registerDoParallel( cores = length( final_tracks_list ) )

spat_disc_tracks <- foreach( ni = 1:length( final_tracks_list ), .packages = .packages() ) %dopar% {
  
  #for( ni in 1:length( final_tracks_list ) ){
  
  final_tracks <- final_tracks_list[[ ni ]]
  
  final_tracks$local_timestamp <- as.POSIXct( final_tracks$local_timestamp, tz = 'UTC' )
  
  final_tracks$spat.disc.dist <- NA
  
  final_tracks$x_final_disc <- final_tracks$x_final
  
  final_tracks$y_final_disc <- final_tracks$y_final
  
  n <- spat_disc_thresh ### distance threshold to spatially discretize at
  
  ids <- unique( final_tracks$id )
  
  for( id in ids ){
    
    id_dat <- final_tracks[final_tracks$id == id,] ## subset the data for the given group
    
    temp.x_final<-id_dat$x_final[ 1 ] ## set their first location to the first temp location
    temp.y_final<-id_dat$y_final[ 1 ]
    
    temp.count <- 1
    
    dist <- 0
    
    i <- 0 ## set counter to 0
    
    totalDist <- 0 ## set step length distance to 0
    
    while(i <= nrow(id_dat)){
      
      while( ( dist < n ) | is.na( dist ) ) {
        
        if( i == nrow(id_dat) ){
          break
          
        }
        
        if( i == 0 || i == temp.count ){
          
          i <- i+1
          
          dist <- sqrt((id_dat$x_final[i]-temp.x_final)**2 + (id_dat$y_final[i]-temp.y_final)**2)
          
          if( is.na( dist ) ){
            
            id_dat$x_final_disc[ i ] <- NA
            id_dat$y_final_disc[ i ] <- NA
            
            next
            
          }
          
        }else{
          
          id_dat$spat.disc.dist[i] <- ifelse( is.na( dist ), NA, 0 )
          id_dat$x_final_disc[i] <- id_dat$x_final[temp.count]
          id_dat$y_final_disc[i] <- id_dat$y_final[temp.count]
          i<-i+1
          dist<-sqrt((id_dat$x_final[i]-temp.x_final)**2 + (id_dat$y_final[i]-temp.y_final)**2)
          
          if( is.na( dist ) ){
            
            next
            
          }
        }
      }
      
      if(dist < n){
        
        id_dat$spat.disc.dist[i] <- 0
        id_dat$x_final_disc[i] <- id_dat$x_final[temp.count]
        id_dat$y_final_disc[i] <- id_dat$y_final[temp.count]
        
        break
        
      }else{
        
        id_dat$spat.disc.dist[i] <- dist
        temp.x_final<-id_dat$x_final[i]
        temp.y_final<-id_dat$y_final[i]
        temp.count <- i
        dist <- 0
        
        if( i == nrow( id_dat ) ){
          
          break
          
        }
      }
    }
    
    
    final_tracks[final_tracks$id == id,] <- id_dat
    
  }
  
  return( final_tracks )  
  
}


saveRDS( spat_disc_tracks, 'DATA/thermal_tracks/spat_disc_tracks.rds' )

spat_disc_tracks <- readRDS( 'DATA/thermal_tracks/spat_disc_tracks.rds' )





moving_recoil <- 1*60 # seconds

displacement_recoil <-5*60 #seconds

stat_thresh_move_for_displace <- 10 #seconds

future_window <- 1*60 #seconds

dist_to_others_thresh <- 1

tracker_vec <- c()

along_way_p_value_vec <- c()

end_p_value_vec <- c()

run_num <- 1 





stopImplicitCluster()

registerDoParallel( cores = length( spat_disc_tracks ) )


save_me_some_times <- c()

#exit_sem_df <- displacement_events_df[ 0, ]

spatial_prop_result <- foreach( nighty_night = 1:length( spat_disc_tracks ), .packages = .packages() ) %dopar%{
  
  #for( nighty_night in 1:length( spat_disc_tracks ) ){
  
  track_df <- spat_disc_tracks[[ nighty_night ]]
  
  night_dat <- track_df[ track_df$local_time > '20:00:00' | track_df$local_time < '05:00:00', ]
  
  if( nrow( night_dat ) == 0 ){
    
    return( list( NULL, NULL, NULL, NULL, NULL, NULL ) )
    
  }else{
    
    move_events_df <- as.data.frame( matrix( nrow = 0, ncol = 3 ) )
    
    names( move_events_df ) <- c( 'id', 'move_start', 'move_end' )
    
    
    move_for_displace_df <- as.data.frame( matrix( nrow = 0, ncol = 3 ) )
    
    names( move_for_displace_df ) <- c( 'id', 'move_start', 'move_end' )
    
    
    
    displacement_events_df <- as.data.frame( matrix( nrow = 0, ncol = 3 ) )
    
    names( displacement_events_df ) <- c( 'id', 'displacement_start', 'displacement_end' )
    
    un_tracks <- unique( night_dat$id )
    
    night_dat$move_transition <- NA
    
    for( track in un_tracks ){
      
      track_dat <- night_dat[ night_dat$id == track, ]
      
      if( nrow( track_dat ) > 1 ){
        
        ## first make move_event_df
        run_length_encoding <- rle( track_dat$moving_predicted )
        
        rle_values <- run_length_encoding$values
        
        rle_lengths <- run_length_encoding$lengths
        
        track_runs <- as.numeric( rep( rle_lengths > moving_recoil, rle_lengths ) )
        
        confirmed_stationary <- as.numeric( track_runs == 1 & track_dat$moving_predicted == 'stationary' )
        
        confirmed_stationary[ is.na( track_dat$moving_predicted ) ] <- NA
        
        movement_transitions <- ifelse( c( NA, diff( confirmed_stationary ) ) == -1, 1, ifelse(  c( NA, diff( confirmed_stationary ) ) == 1, -1, 0 ) )
        
        up_trans <- which( movement_transitions == 1 )
        
        first_move <- which( track_dat$moving_predicted == 'non_stationary' )[ 1 ]
        
        if( !is.na( first_move ) ){
          
          if( ! first_move %in% up_trans ){
            
            up_trans <- c( first_move, up_trans )
            
          }
          
        }
        
        down_trans <- which( movement_transitions == -1 )
        
        if( length( up_trans ) > 1 & length( down_trans ) > 0 ){
          
          if( sum( order( c( up_trans[ 1 ], down_trans[ 1 ], up_trans[ 2 ] ) ) != c( 1, 2 ,3 ) ) > 0 ) stop( dafs )
          
        }
        
        if( length( up_trans ) > 0 ){
          
          moving_event_starts <- track_dat$local_timestamp[ up_trans ]
          
          moving_event_ends <- track_dat$local_timestamp[ down_trans ]
          
          if( length( moving_event_starts ) > length( moving_event_ends ) ){
            
            moving_event_ends <- c( moving_event_ends, track_dat$local_timestamp[ max( which( track_dat$moving_predicted == 'non_stationary' ) ) ]  )
          }
          
          move_events_df_temp <- data.frame( id = track, move_start = moving_event_starts, move_end = moving_event_ends )
          
          plot( track_dat$local_timestamp, track_dat$speed )
          
          abline( v = move_events_df_temp$move_start, col = 'green' )
          
          abline( v = move_events_df_temp$move_end, col = 'red' )
          
          move_events_df <- rbind( move_events_df, move_events_df_temp )
          
        }
        
        
        
        ## then make move_for_displace_df
        run_length_encoding <- rle( track_dat$moving_predicted )
        
        rle_values <- run_length_encoding$values
        
        rle_lengths <- run_length_encoding$lengths
        
        track_runs <- as.numeric( rep( rle_lengths > stat_thresh_move_for_displace, rle_lengths ) )
        
        confirmed_stationary <- as.numeric( track_runs == 1 & track_dat$moving_predicted == 'stationary' )
        
        confirmed_stationary[ is.na( track_dat$moving_predicted ) ] <- NA
        
        movement_transitions <- ifelse( c( NA, diff( confirmed_stationary ) ) == -1, 1, ifelse(  c( NA, diff( confirmed_stationary ) ) == 1, -1, 0 ) )
        
        up_trans <- which( movement_transitions == 1 )
        
        first_move <- which( track_dat$moving_predicted == 'non_stationary' )[ 1 ]
        
        if( !is.na( first_move ) ){
          
          if( ! first_move %in% up_trans ){
            
            up_trans <- c( first_move, up_trans )
            
          }
          
        }
        
        down_trans <- which( movement_transitions == -1 )
        
        if( length( up_trans ) > 1 & length( down_trans ) > 0 ){
          
          if( sum( order( c( up_trans[ 1 ], down_trans[ 1 ], up_trans[ 2 ] ) ) != c( 1, 2 ,3 ) ) > 0 ) stop( dafs )
          
        }
        
        if( length( up_trans ) > 0 ){
          
          moving_event_starts <- track_dat$local_timestamp[ up_trans ]
          
          moving_event_ends <- track_dat$local_timestamp[ down_trans ]
          
          if( length( moving_event_starts ) > length( moving_event_ends ) ){
            
            moving_event_ends <- c( moving_event_ends, track_dat$local_timestamp[ max( which( track_dat$moving_predicted == 'non_stationary' ) ) ]  )
          }
          
          move_for_displace_df_temp <- data.frame( id = track, move_start = moving_event_starts, move_end = moving_event_ends )
          
          plot( track_dat$local_timestamp, track_dat$speed )
          
          abline( v = move_for_displace_df_temp$move_start, col = 'green' )
          
          abline( v = move_for_displace_df_temp$move_end, col = 'red' )
          
          move_for_displace_df <- rbind( move_for_displace_df, move_for_displace_df_temp )
          
        }
        
        
        
        ## making displacement_event_df
        
        run_length_encoding <- rle( as.numeric( track_dat$spat.disc.dist == 0 ) )
        
        rle_values <- run_length_encoding$values
        
        rle_lengths <- run_length_encoding$lengths
        
        track_runs <- as.numeric( rep( rle_lengths > displacement_recoil, rle_lengths ) )
        
        confirmed_no_displacement <- as.numeric( track_runs == 1 & track_dat$spat.disc.dist == 0 )
        
        confirmed_no_displacement[ is.na( track_dat$spat.disc.dist ) ] <- NA
        
        displacement_transitions <- ifelse( c( NA, diff( confirmed_no_displacement ) ) == -1, 1, ifelse(  c( NA, diff( confirmed_no_displacement ) ) == 1, -1, 0 ) )
        
        up_displace_trans <- which( displacement_transitions == 1 )
        
        first_displace <- which( track_dat$spat.disc.dist != 0 )[ 1 ]
        
        if( !is.na( first_displace ) ){
          
          if( ! first_displace %in% up_displace_trans ){
            
            up_displace_trans <- c( first_displace, up_displace_trans )
            
          }
          
        }
        
        
        down_displace_trans <- which( displacement_transitions == -1 )
        
        
        if( length( up_displace_trans ) > 1 & length( down_displace_trans ) > 0 ){
          
          if( sum( order( c( up_displace_trans[ 1 ], down_displace_trans[ 1 ], up_displace_trans[ 2 ] ) ) != c( 1, 2 ,3 ) ) > 0 ) stop( dafs )
          
        }
        
        if( length( up_displace_trans ) > 0 ){
          
          displacement_event_starts <- track_dat$local_timestamp[ up_displace_trans ]
          
          displacement_event_ends <- track_dat$local_timestamp[ down_displace_trans ]
          
          if( length( displacement_event_starts ) > length( displacement_event_ends ) ){
            
            displacement_event_ends <- c( displacement_event_ends, track_dat$local_timestamp[ max( which( track_dat$spat.disc.dist != 0 ) ) ]  )
            
          }
          
          displacement_events_df_temp <- data.frame( id = track, displacement_start = displacement_event_starts, displacement_end = displacement_event_ends )
          
          plot( track_dat$local_timestamp, track_dat$spat.disc.dist )
          
          abline( v = displacement_events_df_temp$displacement_start, col = 'green' )
          
          abline( v = displacement_events_df_temp$displacement_end, col = 'red' )
          
          displacement_events_df <- rbind( displacement_events_df, displacement_events_df_temp )
        }
        
      }
    }
    
    
    displacement_events_df <- displacement_events_df[ order( displacement_events_df$displacement_start ), ]
    
    if( nrow( displacement_events_df ) == 0 ){
      
      return( list( NULL, NULL, NULL, NULL, NULL, NULL ) )
      
    }else{
      
      
      displacement_events_df$start_curr_move <- NA
      displacement_events_df$end_curr_move <- NA
      
      for( i in 1:nrow( displacement_events_df ) ){
        
        id_move <- move_for_displace_df[ move_for_displace_df$id == displacement_events_df$id[ i ], ]

        
        if( length( id_move$move_start[ id_move$move_start <= displacement_events_df$displacement_start[ i ] ] ) == 0 ){
          
          next
        }
        
        if( as.numeric( displacement_events_df$displacement_start[ i ] - max( id_move$move_start[ id_move$move_start <= displacement_events_df$displacement_start[ i ] ] ) > 60, units = 'secs'  ) ){
          
          next
          
        }
        
        
        start_curr_move <- max( id_move$move_start[ id_move$move_start <= displacement_events_df$displacement_start[ i ] ] )
        
        time_diffs <- abs( as.numeric( displacement_events_df$displacement_end[ i ] - id_move$move_end, units = 'secs' ) )
        
        if( min( time_diffs ) > 60 ){
          
          end_time <- displacement_events_df$displacement_end[ i ]
          
        }else{
          
          end_ind <- which.min( time_diffs )
          
          end_time <- id_move$move_end[ end_ind ]
          
          # if the displacement ends after the move
          if( as.numeric( displacement_events_df$displacement_end[ i ] - end_time, units = 'secs' ) > 0 ){
            
            #just end the displacement at the displacement end
            end_time <- displacement_events_df$displacement_end[ i ]
            
          }
        }
        
        displacement_events_df$start_curr_move[ i ] <- start_curr_move
        
        displacement_events_df$end_curr_move[ i ] <- end_time
        
      }
      
      
      displacement_events_df$start_curr_move <- as.POSIXct( displacement_events_df$start_curr_move, tz = 'UTC', origin = '1970-01-01 00:00:00' )
      displacement_events_df$end_curr_move <- as.POSIXct( displacement_events_df$end_curr_move, tz = 'UTC', origin = '1970-01-01 00:00:00' )
      
      displacement_events_df <- displacement_events_df[ !is.na( displacement_events_df$start_curr_move ), ]
      
      displacement_events_df <- displacement_events_df[ order( displacement_events_df$start_curr_move ), ]
      
      num_woken_at_start <- c()
      num_woken_along_the_way_vec <- c()
      num_woken_at_end <- c()
      
      prop_woken_at_start <- c()
      prop_woken_along_the_way <- c()
      prop_woken_at_end <- c()
      
      move_events_df_for_depletion <- move_events_df
      
      for( i in 1:nrow( displacement_events_df ) ){
        
        tracks_sub <- track_df[ track_df$id == displacement_events_df$id[ i ] & track_df$local_timestamp >= displacement_events_df$start_curr_move[ i ] & track_df$local_timestamp <= displacement_events_df$end_curr_move[ i ],  ]
        
        tracks_sub$row_to_process <- as.numeric( tracks_sub$spat.disc.dist != 0 )
        
        tracks_sub$row_to_process[ nrow( tracks_sub ) ] <- 1
        
        first_x <- tracks_sub$x_final[ 1 ]
        first_y <- tracks_sub$y_final[ 1 ]
        
        curr_time <- tracks_sub$local_timestamp[ 1 ]
        
        others_dat <- track_df[ track_df$local_timestamp == curr_time & track_df$id != displacement_events_df$id[ i ], ]
        
        distance_to_others <- sqrt( ( first_x - others_dat$x_final )**2 + ( first_y - others_dat$y_final ) **2 )
        
        ids_in_range <- others_dat$id[ which( distance_to_others < dist_to_others_thresh ) ]
        
        woken_with_move <- which( move_events_df_for_depletion$id %in% ids_in_range & move_events_df_for_depletion$move_start > curr_time & move_events_df_for_depletion$move_start < ( curr_time + future_window ) )
        
        num_woken <- length( woken_with_move )
        
        prop_woken <- ifelse( length( ids_in_range != 0 ) , num_woken / length( ids_in_range ), NA )
        
        
        num_woken_at_start <- c( num_woken_at_start, num_woken )
        
        prop_woken_at_start <- c( prop_woken_at_start, prop_woken )
        
        temper_list <- list( num_woken, prop_woken )
        

        ## remove those woken from the move_events_df_for_depletion
        if( num_woken != 0 ){
          
          move_events_df_for_depletion <- move_events_df_for_depletion[ - woken_with_move, ]
          
        }
        
        counter <- 2 
        
        num_woken_along_the_way <- 0
        
        for( j in which( tracks_sub$row_to_process == 1 ) ){
          
          curr_x <- tracks_sub$x_final[ j ]
          curr_y <- tracks_sub$y_final[ j ]
          
          curr_time <- tracks_sub$local_timestamp[ j ]
          
          others_dat <- track_df[ track_df$local_timestamp == curr_time & track_df$id != displacement_events_df$id[ i ], ]
          
          distance_to_others <- sqrt( ( curr_x - others_dat$x_final )**2 + ( curr_y - others_dat$y_final )**2 )
          
          ids_in_range <- others_dat$id[ which( distance_to_others < dist_to_others_thresh ) ]
          
          woken_with_move <- which( move_events_df_for_depletion$id %in% ids_in_range & move_events_df_for_depletion$move_start > curr_time & move_events_df_for_depletion$move_start < ( curr_time + future_window ) )
          
          num_woken <- length( woken_with_move )
          
          prop_woken <- ifelse( length( ids_in_range != 0 ) , num_woken / length( ids_in_range ), NA )
          
          
          if( length( num_woken ) != 0 ){
            
            save_me_some_times <- c( save_me_some_times, curr_time )
            
            #exit_sem_df <- rbind( exit_sem_df, displacement_events_df[ i, ] )
            
          }
          
          num_woken_along_the_way <- num_woken_along_the_way + num_woken
          
          prop_woken_along_the_way <- c( prop_woken_along_the_way, prop_woken )
          
          if( j == nrow( tracks_sub ) ){
            
            num_woken_at_end <- c( num_woken_at_end, num_woken )
            
            prop_woken_at_end <- c( prop_woken_at_end, prop_woken )
            
            
          }
          
          
          ## remove those woken from the move_events_df_for_depletion
          if( num_woken != 0 ){
            
            move_events_df_for_depletion <- move_events_df_for_depletion[ - woken_with_move, ]
            
          }
          
          
          temper_list <- list( num_woken, prop_woken )
          
          
          counter <- counter + 1 
        }
        
        
        num_woken_along_the_way_vec <- c( num_woken_along_the_way_vec, num_woken_along_the_way )  
        
      }
      
      
      return( list( num_woken_at_start, num_woken_along_the_way_vec, num_woken_at_end, prop_woken_at_start, prop_woken_along_the_way, prop_woken_at_end  ))
      
    }
    
    
  }
  
  
}


stopImplicitCluster( )


final_num_woken_at_start <- unlist( lapply( spatial_prop_result, function( x ) x[[ 1 ]] ) )

final_num_woken_along_the_way_vec <- unlist( lapply( spatial_prop_result, function( x ) x[[ 2 ]] ) )

final_num_woken_at_end <- unlist( lapply( spatial_prop_result, function( x ) x[[ 3 ]] ) )

trim_num_woken_at_start <- final_num_woken_at_start[ final_num_woken_at_start != final_num_woken_along_the_way_vec ]

trim_num_woken_along_the_way <- final_num_woken_along_the_way_vec[ final_num_woken_at_start != final_num_woken_along_the_way_vec ]



along_way_wilc <- wilcox.test( final_num_woken_at_start, final_num_woken_along_the_way_vec, paired = T, alternative = 'less' )
along_way_wilc
along_way_wilc <- wilcox.test( trim_num_woken_at_start, trim_num_woken_along_the_way, paired = T, alternative = 'less' )
along_way_wilc




boxplot( final_num_woken_along_the_way_vec, final_num_woken_at_start )






















