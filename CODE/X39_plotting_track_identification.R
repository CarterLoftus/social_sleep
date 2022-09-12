

library( data.table )
library( stringr )

tracklet_files <- list.files( 'DATA/thermal_tracks/identified_tracks/corrected' )

nights <- str_split_fixed( tracklet_files, '_', 3 )[ , 1 ]

## read in the GPS data
gps_dat <- fread( 'DATA/gps_acc_data/gps_dat.csv' )

gps_dat <- as.data.frame( gps_dat )

gps_dat$corr_local_timestamp <- as.POSIXct( gps_dat$corr_local_timestamp, tz = 'UTC' )

## read in the VeDBA data
full_ved <- fread( 'DATA/gps_acc_data/full_ved.csv' )

full_ved <- as.data.frame( full_ved )

full_ved$corr_local_timestamp <- as.POSIXct( full_ved$corr_local_timestamp, tz = 'UTC' )


for( night_of_int in nights ){
  
  ## read in the tracklet data
  ident_tracks <- readRDS( paste0( 'DATA/thermal_tracks/identified_tracks/corrected/', night_of_int, '_ident_tracks.rds' ) )
  
  ident_tracks$local_timestamp <- as.POSIXct( ident_tracks$local_timestamp, tz = 'UTC' )
  
  track_times <- unique( as.POSIXct( ident_tracks$local_timestamp ) )
  
  head( ident_tracks )
  
  ###### Some plots ###########

  gps_trim <- gps_dat[ gps_dat$corr_local_timestamp > min( ident_tracks$local_timestamp ) & gps_dat$corr_local_timestamp < max( ident_tracks$local_timestamp ),  ]
  
  head( gps_trim )    
  

  full_ved_trim <- full_ved[ full_ved$corr_local_timestamp > min( ident_tracks$local_timestamp ) & full_ved$corr_local_timestamp < max( ident_tracks$local_timestamp ),  ]
  
  head( full_ved_trim )    
  
  
  tag_names <- sort( as.character( unique( gps_dat$tag ) ) )
  
  
  ident_dat_gps_speed <-  read.csv( paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/ident_dat_gps_speed.csv' ) )
  ident_dat_gps_speed$ident_start_time <- as.POSIXct( ident_dat_gps_speed$ident_start_time, tz = 'UTC' )
  ident_dat_gps_speed$ident_end_time <- as.POSIXct( ident_dat_gps_speed$ident_end_time, tz = 'UTC' )
  
  ident_dat_cont_ved <-  read.csv( paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/ident_dat_cont_ved.csv' ) )
  ident_dat_cont_ved$ident_start_time <- as.POSIXct( ident_dat_cont_ved$ident_start_time, tz = 'UTC' )
  ident_dat_cont_ved$ident_end_time <- as.POSIXct( ident_dat_cont_ved$ident_end_time, tz = 'UTC' )
  
  ident_dat_burst_ved <-  read.csv( paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int, '/ident_dat_burst_ved.csv' ) )
  ident_dat_burst_ved$ident_start_time <- as.POSIXct( ident_dat_burst_ved$ident_start_time, tz = 'UTC' )
  ident_dat_burst_ved$ident_end_time <- as.POSIXct( ident_dat_burst_ved$ident_end_time, tz = 'UTC' )
  
  par( mfrow = c( 2, 2 ) )
  
  night_date <-  unique( as.Date( ident_tracks$local_timestamp - 12*60*60 ) )
  
  for( ident_dat in list( ident_dat_gps_speed, ident_dat_cont_ved, ident_dat_burst_ved ) ){
    
    plot( 1, ylab = 'Tag ID', xlab = 'Timestamp', type = 'n', xlim = range( ident_tracks$local_timestamp ), ylim = c( -0.5, ( length( tag_names ) + 0.5 ) ), xaxt = 'n', yaxt = 'n' )
    axis( 1, at = seq( min( ident_tracks$local_timestamp ), max( ident_tracks$local_timestamp ), by = '1 hour' ), labels = as.POSIXct( seq( min( ident_tracks$local_timestamp ), max( ident_tracks$local_timestamp ), by = '1 hour' ), tz = 'UTC' ) )
    axis( 2, at = 1:length( tag_names ), labels = tag_names, las = 1 )
    
    points( track_times, rep( 0, length( track_times ) ), cex = 0.5, col = 'green', pch = 16 )
    
    for( i in 1:length( tag_names ) ){
      
      gps_tag <- gps_trim[ gps_trim$tag == tag_names[ i ], ]
      
      acc_tag <- full_ved_trim[ full_ved_trim$tag == tag_names[ i ], ]
      
      tag_ident <- ident_dat[ ident_dat$tag == tag_names[ i ], ]
      
      #points( gps_tag$corr_local_timestamp, rep( i - 0.2 , length( gps_tag$corr_local_timestamp ) ), cex = 0.3, pch = 18, col = 'red' )
      
      #points( acc_tag$corr_local_timestamp, rep( i + 0.2 , length( acc_tag$corr_local_timestamp ) ), cex = 0.3, pch = 18, col = 'blue' )
      
      
      if( nrow( tag_ident ) != 0 ){
        
        for( j in 1:nrow( tag_ident ) ){
          
          x_times <- seq( tag_ident$ident_start_time[ j ], tag_ident$ident_end_time[ j ], by = '1 sec' )
          
          points( x_times, rep( i, length( x_times ) ), pch = 16, cex = 0.7 )
          
        }
      }
    }
    
    abline( h = seq( 0.5, length( tag_names ) + 0.5 , by = 1 ) )
    
    abline( v = as.POSIXct( paste( night_date, '20:00:00' ), tz = 'UTC' ), lty = 2 )
    
    abline( v = as.POSIXct( paste( night_date, '21:00:00' ), tz = 'UTC' ), lty = 2 )
    
  }
  
  
  ## combine these three plots into one to show where we have identified individuals using any of the possible data streams
  
  total_ident <- rbind( ident_dat_gps_speed, ident_dat_cont_ved, ident_dat_burst_ved )
  
  
  plot( 1, ylab = 'Tag ID', xlab = 'Timestamp', type = 'n', xlim = range( ident_tracks$local_timestamp ), ylim = c( 0.5, ( length( tag_names ) + 0.5 ) ), xaxt = 'n', yaxt = 'n' )
  axis( 1, at = seq( min( ident_tracks$local_timestamp ), max( ident_tracks$local_timestamp ), by = '1 hour' ), labels = as.POSIXct( seq( min( ident_tracks$local_timestamp ), max( ident_tracks$local_timestamp ), by = '1 hour' ), tz = 'UTC' ) )
  axis( 2, at = 1:length( tag_names ), labels = tag_names, las = 1 )
  
  points( track_times, rep( 0, length( track_times ) ), cex = 0.5, col = 'green', pch = 16 )
          
  for( i in 1:length( tag_names ) ){
    
    gps_tag <- gps_trim[ gps_trim$tag == tag_names[ i ], ]
    
    acc_tag <- full_ved_trim[ full_ved_trim$tag == tag_names[ i ], ]
    
    tag_ident <- total_ident[ total_ident$tag == tag_names[ i ], ]
    
    #points( gps_tag$corr_local_timestamp, rep( i - 0.2 , length( gps_tag$corr_local_timestamp ) ), cex = 0.5, pch = 18, col = 'red' )
    
    #points( acc_tag$corr_local_timestamp, rep( i + 0.2 , length( acc_tag$corr_local_timestamp ) ), cex = 0.5, pch = 18, col = 'blue' )
    
    
    if( nrow( tag_ident ) != 0 ){
      
      for( j in 1:nrow( tag_ident ) ){
        
        x_times <- seq( tag_ident$ident_start_time[ j ], tag_ident$ident_end_time[ j ], by = '1 sec' )
        
        points( x_times, rep( i, length( x_times ) ), pch = 16, cex = 0.7 )
        
      }
    }
  }
  
  abline( h = seq( 0.5, length( tag_names ) + 0.5 , by = 1 ) )
  
  abline( v = as.POSIXct( paste( night_date, '20:00:00' ), tz = 'UTC' ), lty = 2 )
  
  abline( v = as.POSIXct( paste( night_date, '21:00:00' ), tz = 'UTC' ), lty = 2 )
  
}



load( paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int,  '/tracklet_id_dat.RData' )  )


#### make a plotting tool to check track matches #####


##### plot according to user inputs #####

library( plotly )

# 
# tag <- '2428' ## which tag to plot
# 
# tracklet_duration_threshold <- 10*60 ## we won't visualize any tracklets that are shorter than this number of seconds
# 
# ##### prep the data for plotting #### 
# 
# ### combine the two acc dataframes (continuous vedbas and burst vedbas)
# 
# ## make a dataframe of all the times
# 
# earliest_time <- min( ident_tracks$local_timestamp )
# 
# latest_time <- max( ident_tracks$local_timestamp )
# 
# time_df <- data.frame( corr_local_timestamp = seq( earliest_time, latest_time, by = '1 sec' ) )
# 
# tag_cont_ved <- merge( x = time_df, y = trim_vedba_smooth_std[ , c( 'corr_local_timestamp', tag ) ], by = 'corr_local_timestamp', all.x = T, all.y = F, sort = F )
# 
# names( tag_cont_ved )[ 2 ] <- 'cont_ved'
# 
# tag_full_ved <- merge( x = tag_cont_ved, y = trim_burst_vedba_std[ , c( 'corr_local_timestamp', tag ) ], by = 'corr_local_timestamp', all.x = T, all.y = F, sort = F ) 
# 
# names( tag_full_ved )[ 3 ] <- 'burst_ved'
# 
# 
# tag_full_ved <- tag_full_ved[ order( tag_full_ved$corr_local_timestamp ), ]
# 
# 
# problem_inds <- which( !is.na( tag_full_ved$cont_ved ) & !is.na( tag_full_ved$burst_ved ) ) ## this should never occur, where a tag has both continuous and burst data
# 
# #hmmm, but it does occur once
# 
# tag_full_ved[ problem_inds, ] ## I have no explanation for this but it only happens once. Let's just ignore it and overwrite it
# 
# tag_full_ved$burst_ved[ !is.na( tag_full_ved$cont_ved ) ] <- NA
# 
# tag_full_ved$vedba <- apply( tag_full_ved[ , c( 'cont_ved', 'burst_ved' ) ], 1, na.sum )
# 
# 
# 
# tag_full_ved <- tag_full_ved[ !is.na( tag_full_ved$vedba ), ]
# 
# ## make a dataframe of the tracklet speeds standardized for the whole night (not split into burst and continous) (although it would be better to combine the ones that were split above for burst and continuous)
# 
# # standardize the speeds of the tracklets all together
# tracklet_speeds_std <- as.data.frame( stdize( as.matrix( tracklet_speeds[  , names( tracklet_speeds ) != 'local_timestamp' ] ) ) )
# 
# # add the timestamp back to the dataframe
# tracklet_speeds_std$local_timestamp <- tracklet_speeds$local_timestamp
# 
# 

####

track_ids <- names( trim_tracklet_speeds )[ names( trim_tracklet_speeds ) != 'local_timestamp' ]

plotly_rgb <- function( color ){

  rgb <- col2rgb( color )

  paste0( 'rgb( ', rgb[ 1 ], ', ', rgb[ 2 ], ', ', rgb[ 3 ], ' )' )
}


id_plot <- function( plot_acc = T, plot_gps = T, plot_all_tracklets = T, plot_all_tags = T, tracklet_to_plot = NULL, tag_to_plot = NULL, start_time = NULL, end_time = NULL ){
  
  library( plotly )

  if( is.null( start_time ) & is.null( end_time ) ){

    if( !is.null( tracklet_to_plot ) ){
      
      tracklet_data <- trim_tracklet_speeds[ , tracklet_to_plot ]
      
      start_ind <- min( which( !is.na( tracklet_data ) ) )
      
      start_time <- trim_tracklet_speeds$local_timestamp[ start_ind ]
      
      end_ind <- max( which( !is.na( tracklet_data ) ) )
      
      end_time <- trim_tracklet_speeds$local_timestamp[ end_ind ]
      
      full_ved_extra_trim <- full_ved_trim[ full_ved_trim$corr_local_timestamp >= start_time & full_ved_trim$corr_local_timestamp <= end_time, ]
      
      extra_trim_gps_speeds_smooth <- trim_gps_speeds_smooth[ trim_gps_speeds_smooth$corr_local_timestamp >= start_time & trim_gps_speeds_smooth$corr_local_timestamp <= end_time, ]
      
      extra_trim_tracklet_speeds <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= start_time & trim_tracklet_speeds$local_timestamp <= end_time, ]
      
    }else{
      
      full_ved_extra_trim <- full_ved_trim
      
      extra_trim_gps_speeds_smooth <- trim_gps_speeds_smooth
      
      extra_trim_tracklet_speeds <- trim_tracklet_speeds
      
    }
    
  }else{

    start_time <- as.POSIXct( start_time, tz = 'UTC' )
    end_time <- as.POSIXct( end_time, tz = 'UTC' )

    full_ved_extra_trim <- full_ved_trim[ full_ved_trim$corr_local_timestamp >= start_time & full_ved_trim$corr_local_timestamp <= end_time, ]

    extra_trim_gps_speeds_smooth <- trim_gps_speeds_smooth[ trim_gps_speeds_smooth$corr_local_timestamp >= start_time & trim_gps_speeds_smooth$corr_local_timestamp <= end_time, ]
    
    extra_trim_tracklet_speeds <- trim_tracklet_speeds[ trim_tracklet_speeds$local_timestamp >= start_time & trim_tracklet_speeds$local_timestamp <= end_time, ]
    
  }

  
  fig_tracklets <- plot_ly( )
  
  if( !is.null( tracklet_to_plot ) ){
    
    tracklet_speed <- extra_trim_tracklet_speeds[ , tracklet_to_plot ]
    
    fig_tracklets <- add_trace( fig_tracklets,  x = extra_trim_tracklet_speeds$local_timestamp, y = tracklet_speed,
                                type = 'scatter',
                                mode = 'lines',
                                marker = list( color = plotly_rgb( 'black' ), size = 1 ),
                                line = list( color = plotly_rgb( 'black' ) ),
                                name = paste( tracklet_to_plot, ' speed' ) )
    
  }
  
  
  if( plot_all_tracklets ){
    
    for( track in track_ids ){
      
      tracklet_speed <- extra_trim_tracklet_speeds[ , track ]
      
      if( sum( !is.na( tracklet_speed ) ) != 0  ){
        
        fig_tracklets <- add_trace( fig_tracklets,  x = extra_trim_tracklet_speeds$local_timestamp, y = tracklet_speed,
                                    type = 'scatter',
                                    mode = 'lines',
                                    marker = list( color = plotly_rgb( 'black' ), size = 1 ),
                                    line = list( color = plotly_rgb( 'black' ) ),
                                    name = paste( track, ' speed' ) )
        
      }
      
    }
    
  }
  
  
  if( plot_acc ){
    
    fig_acc <- plot_ly( )

    if( plot_all_tags ){

      for( tag in tag_names ){

        tag_full_ved_extra_trim <- full_ved_extra_trim[ full_ved_extra_trim$tag == tag, ]

        fig_acc <- add_trace( fig_acc,  x = tag_full_ved_extra_trim$corr_local_timestamp, y = tag_full_ved_extra_trim$log_vedba,
                          type = 'scatter',
                          mode = 'lines',
                          marker = list( color = plotly_rgb( 'blue' ), size = 1 ),
                          line = list( color = plotly_rgb( 'blue' ) ),
                          name = paste( tag, 'log VeDBA' ) )
        

      }

    }

    if( !is.null( tag_to_plot ) ){

      tag_full_ved_extra_trim <- full_ved_extra_trim[ full_ved_extra_trim$tag == tag_to_plot, ]

      fig_acc <- add_trace( fig_acc,  x = tag_full_ved_extra_trim$corr_local_timestamp, y = tag_full_ved_extra_trim$log_vedba,
                        type = 'scatter',
                        mode = 'lines',
                        marker = list( color = plotly_rgb( 'blue' ), size = 1 ),
                        line = list( color = plotly_rgb( 'blue' ) ),
                        name = paste( tag_to_plot, 'log VeDBA' ) )


    }
  }

  if( plot_gps ){

    fig_gps <- plot_ly( )
    
    if( plot_all_tags ){

      for( tag in tag_names ){

        tag_gps_speeds <- extra_trim_gps_speeds_smooth[ , tag ]

        fig_gps <- add_trace( fig_gps,  x = extra_trim_gps_speeds_smooth$corr_local_timestamp, y = tag_gps_speeds,
                          type = 'scatter',
                          mode = 'lines',
                          marker = list( color = plotly_rgb( 'red' ), size = 1 ),
                          line = list( color = plotly_rgb( 'red' ) ),
                          name = paste( tag, 'GPS speed' ) )
        
      }

    }

    if( !is.null( tag_to_plot ) ){

      tag_gps_speeds <- extra_trim_gps_speeds_smooth[ , tag_to_plot ]

      fig_gps <- add_trace( fig_gps,  x = extra_trim_gps_speeds_smooth$corr_local_timestamp, y = tag_gps_speeds,
                        type = 'scatter',
                        mode = 'lines',
                        marker = list( color = plotly_rgb( 'red' ), size = 1 ),
                        line = list( color = plotly_rgb( 'red' ) ),
                        name = paste( tag_to_plot, 'GPS speed' ) )

    }
  }

  if( plot_acc & plot_gps ){
    
    fig_final <- subplot( fig_acc, fig_tracklets, fig_gps, nrows = 3, shareX = T, margin = 0, heights = c( 0.2,0.2,0.2 ) ) %>% layout( )
    
  }else{
    
    if( plot_acc ){
      
      fig_final <- subplot( fig_acc, fig_tracklets, nrows = 2, shareX = T, margin = 0, heights = c( 0.2,0.2 ) ) %>% layout( )
      
    }else{
      
      if( plot_gps ){
        
        fig_final <- subplot( fig_gps, fig_tracklets, nrows = 2, shareX = T, margin = 0, heights = c( 0.2,0.2 ) ) %>% layout( )
        
      }else{
        
        fig_final <- fig_tracklets
      }
    }
  }
  
  fig_final
}


total_ident$tag <- as.character( total_ident$tag )

i = 1

id_plot( plot_all_tracklets = F, plot_all_tags = F, tracklet_to_plot = total_ident$tracklet[ i ], tag_to_plot = total_ident$tag[ i ] )


library( stringr )

#### for validation: baboon that is 64 in second video is 2428

input_vid_name <- "20190806_142005603000"

input_track_id <- '43'

input_tag <- '6934'



vid_num <- which( sort( unique( therm_tracks$vid_name ) ) == input_vid_name )

focal_tracklet <- track_ids[ which( str_split_fixed( track_ids, '_', 6 )[ , vid_num ] ==  str_pad( input_track_id, 3 , pad = "0") ) ]  ## 2428 is track NA_064_NA_NA_NA_NA


id_plot( plot_all_tracklets = F, plot_all_tags = F, tracklet_to_plot = focal_tracklet, tag_to_plot = input_tag, start_time = '2019-08-06 17:00', end_time = '2019-08-06 21:00' )


### input video

### input track number

### input tag number





# {
#   ids_to_keep <- dur_dat$id[ dur_dat$duration >= tracklet_duration_threshold ]
#   
#   if( heatmap_dist_data == 'gps' ){
#     
#     tag_dist_arr <- - sqrt( sec_gps_dist_arr[ dimnames( sec_gps_dist_arr)[[ 1 ]] %in% ids_to_keep, tag,  ] )
#     
#   }
#   
#   if( heatmap_dist_data == 'acc' ){
#     
#     tag_dist_arr <- - sqrt( sec_acc_dist_arr[ dimnames( sec_acc_dist_arr)[[ 1 ]] %in% ids_to_keep, tag,  ] )
#     
#   }
#   
#   fig1 <- plot_ly( x = as.POSIXct( as.numeric( dimnames( tag_dist_arr)[[ 2 ]] ), origin = '1970-01-01 00:00:00', tz = 'UTC' ), y = dimnames( tag_dist_arr)[[ 1 ]], z = tag_dist_arr, type = 'heatmap' )
#   
#   
#   ids_to_plot <- dimnames( tag_dist_arr)[[ 1 ]]  ## this is because if we are using the GPS or continuous ACC data that stops at 18:00, we need to remove the tracks that begin only after the tag data stops (because we never calculated distances to these tracklets, and they are therefore not in the distance array, which causes an error below if we try to call their column)
#   
#   
#   if( plot_acc == T ){
#     
#     fig2 <- plot_ly(  x = tag_full_ved$corr_local_timestamp, y = tag_full_ved$vedba, 
#                       type = 'scatter', 
#                       mode = 'markers', 
#                       marker = list( color = plotly_rgb( 'blue' ), size = 1 ), 
#                       line = list( color = plotly_rgb( 'blue' ) ), 
#                       name = 'log VeDBA' )
#     
#     if( plot_gps == T ){
#       
#       if( sep_acc_gps == F ){
#         
#         fig2 <- add_trace( fig2,  x = trim_gps_speeds_std$corr_local_timestamp, y = trim_gps_speeds_std[ , tag ], 
#                            mode = 'lines', 
#                            line = list( color = plotly_rgb( 'red' ) ), 
#                            name = 'GPS speed' )
#         
#       }else{
#         
#         fig3 <- plot_ly(  x = trim_gps_speeds_std$corr_local_timestamp, y = -trim_gps_speeds_std[ , tag ], 
#                           type = 'scatter',
#                           mode = 'lines', 
#                           line = list( color = plotly_rgb( 'red' ) ),
#                           name = 'GPS speed' )
#         
#         for( i in 1:length( ids_to_plot ) ){
#           
#           fig3 <- add_trace( fig3, x = trim_gps_speeds_std$corr_local_timestamp, y = -trim_tracklet_speeds_std[ , ids_to_plot[ i ] ], hoverinfo = 'text', text =  ids_to_plot[ i ], line = list( color = plotly_rgb( 'black' ) ), name =  ids_to_plot[ i ] )
#           
#         }
#       }
#     }
#   }else{
#     
#     if( plot_gps == T ){
#       
#       fig2 <- plot_ly(  x = trim_gps_speeds_std$corr_local_timestamp, y = trim_gps_speeds_std[ , tag ], 
#                         type = 'scatter', 
#                         mode = 'lines', 
#                         line = list( color = plotly_rgb( 'red' ) ), 
#                         name = 'GPS speed' )
#     }
#   }
#   
#   if( plot_bursts == F ){
#     
#     for( i in 1:length( ids_to_plot ) ){
#       
#       fig2 <- add_trace( fig2, x = trim_gps_speeds_std$corr_local_timestamp, y = trim_tracklet_speeds_std[ , ids_to_plot[ i ] ], hoverinfo = 'text', text =  ids_to_plot[ i ], line = list( color = plotly_rgb( 'black' ) ), name =  ids_to_plot[ i ] )
#       
#     }
#   }else{=
#     
#     for( i in 1:length( ids_to_keep ) ){
#       
#       x_s <- tracklet_speeds_std$local_timestamp[ !is.na( tracklet_speeds_std[ , ids_to_keep[ i ] ] ) ]
#       y_s <- tracklet_speeds_std[ , ids_to_keep[ i ] ][ !is.na( tracklet_speeds_std[ , ids_to_keep[ i ] ] ) ]
#       
#       fig2 <- add_trace( fig2, x = x_s, y = y_s, hoverinfo = 'text', text =  ids_to_keep[ i ], 
#                          line = list( color = plotly_rgb( 'black' ) ), 
#                          name =  ids_to_keep[ i ] )
#       
#     }
#     
#   }
#   
#   if( plot_window_means == T ){
#     
#     if( plot_bursts == F ){
#       
#       for( i in 1:length( ids_to_plot ) ){
#         
#         if( i == 1 ){
#           
#           dat_to_plot <- plotly_dists_arr[ ids_to_plot[ i ], tag, ]
#           
#           times <- as.POSIXct( as.numeric( dimnames( plotly_dists_arr )[[ 3 ]][ !is.na( dat_to_plot ) ] ), origin = '1970-01-01 00:00:00', tz = 'UTC' )
#           
#           dat_to_plot <- dat_to_plot[ !is.na( dat_to_plot ) ]
#           
#           fig3 <- plot_ly( x = times, y = dat_to_plot, 
#                            hoverinfo = 'text', 
#                            type = 'scatter',
#                            mode = 'lines',
#                            text =  ids_to_plot[ i ], 
#                            line = list( color = plotly_rgb( 'black' ) ), 
#                            name =  ids_to_plot[ i ] )
#           
#         }else{
#           
#           dat_to_plot <- plotly_dists_arr[ ids_to_plot[ i ], tag, ]
#           
#           times <- as.POSIXct( as.numeric( dimnames( plotly_dists_arr )[[ 3 ]][ !is.na( dat_to_plot ) ] ), origin = '1970-01-01 00:00:00', tz = 'UTC' )
#           
#           dat_to_plot <- dat_to_plot[ !is.na( dat_to_plot ) ]
#           
#           fig3 <- add_trace( fig3, x = times, y = dat_to_plot,
#                              hoverinfo = 'text', 
#                              type = 'scatter',
#                              mode = 'lines',
#                              text =  ids_to_plot[ i ], 
#                              line = list( color = plotly_rgb( 'black' ) ), 
#                              name =  ids_to_plot[ i ] )
#           
#         }
#         
#         
#       }
#     }else{
#       
#       for( i in 1:length( ids_to_keep ) ){
#         
#         if( i == 1 ){
#           
#           dat_to_plot <- plotly_dists_arr[ ids_to_keep[ i ], tag, ]
#           
#           times <- as.POSIXct( as.numeric( dimnames( plotly_dists_arr )[[ 3 ]][ !is.na( dat_to_plot ) ] ), origin = '1970-01-01 00:00:00', tz = 'UTC' )
#           
#           dat_to_plot <- dat_to_plot[ !is.na( dat_to_plot ) ]
#           
#           fig3 <- plot_ly( x = times, y = dat_to_plot, 
#                            hoverinfo = 'text', 
#                            type = 'scatter',
#                            mode = 'lines',
#                            text =  ids_to_keep[ i ], 
#                            line = list( color = plotly_rgb( 'black' ) ), 
#                            name =  ids_to_keep[ i ] )
#           
#         }else{
#           
#           dat_to_plot <- plotly_dists_arr[ ids_to_keep[ i ], tag, ]
#           
#           times <- as.POSIXct( as.numeric( dimnames( plotly_dists_arr )[[ 3 ]][ !is.na( dat_to_plot ) ] ), origin = '1970-01-01 00:00:00', tz = 'UTC' )
#           
#           dat_to_plot <- dat_to_plot[ !is.na( dat_to_plot ) ]
#           
#           fig3 <- add_trace( fig3, x = times, y = dat_to_plot, 
#                              hoverinfo = 'text', 
#                              type = 'scatter',
#                              mode = 'lines',
#                              text =  ids_to_keep[ i ], 
#                              line = list( color = plotly_rgb( 'black' ) ), 
#                              name =  ids_to_keep[ i ] )
#           
#         }
#       }
#     }
#     
#     fig_final <- subplot( fig2, fig1, fig3, nrows = 3, shareX = T, margin = 0, heights = c( 0.2,0.6,0.2 ) ) %>% layout( )
#     
#     fig_final
#     
#   }else{
#     
#     if( sep_acc_gps == F ){
#       
#       fig_final <- subplot( fig2, fig1, nrows = 2, shareX = T, margin = 0, heights = c( 0.3,0.7 ) ) %>% layout( )
#       
#       fig_final
#       
#     }else{
#       
#       fig_final <- subplot( fig2, fig1, fig3, nrows = 3, shareX = T, margin = 0, heights = c( 0.2,0.6,0.2 ) ) %>% layout( )
#       
#       fig_final
#     }
#   }
# }
# 
# 



tag_names <- gps_dat[]

min_time
max_time

for( tag in tag_names )
















