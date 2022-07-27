
#options( digits.secs = 6, digits = 15 ) #reset options, in order to see the milliseconds

library( tidygraph )
library( hms )
library( plyr )
library( lubridate )
library( raster )
library( igraph )
library( qgraph )
library( data.table )


na_min <- function( x ){
  
  if( sum( !is.na( x ) ) == 0 ){
    
    return( NA )
    
  }else{
    
    return( min( x, na.rm = T ) )
  }
  
}

### spatial networks from the tracklet data

input_data <- 'positions' # options are 'corrected' (tracklets), 'uncorrected' (tracklets), or 'positions'. To reproduce the results of the dissertation, this should only be run with positions

spat_net_time <- '22:00:00'

class_meth <- 'percentile_thresh'

sep_thresh <- T


if( input_data == 'positions' ){

  files <- list.files( path = paste0( "DATA/thermal_tracks/raw_positions_processed/" ), full.names = T )

}else{

  if( ! input_data %in% c( 'corrected', 'uncorrected' ) ) stop( 'input_data must be set to positions, corrected, or uncorrected' )

  files <- list.files( path = paste0( "DATA/thermal_tracks/identified_tracks/", input_data ), full.names = T )

}

tracks_or_pos_list <- lapply( files, FUN = readRDS )


## the next piece is just for visualizing and it takes a while to run which is why it is commented out

# times_to_plot <- c( '18:00:00', '20:00:00', '22:00:00', '00:00:00', '02:00:00', '04:00:00'
#                     #, '06:00:00'
#                      )
# 
# colors <- c( 'red', 'red', 'red', 'red', 'red', 'red', 'red')
# 
# for( s in 1:length( tracks_or_pos_list ) ){
# 
#   par( mfrow = c( 3, 2 ) )
#   par( oma = c( 0, 0, 0, 0 ) )
#   par( mar = rep( 0, 4 ) )
# 
#   tracks_or_pos <- tracks_or_pos_list[[ s ]]
# 
#   if( input_data == 'positions' ){
# 
#     tracks_to_plot <- tracks_or_pos[ seq( 1, nrow( tracks_or_pos ), by = 10 ), ]
#   }else{
# 
#     tracks_to_plot <- tracks_or_pos[ seq( 1, nrow( tracks_or_pos ), by = 10 ), ]
# 
#   }
#   ## plot where everyone is at the times to plot listed above
# 
#   for( t in 1:length( times_to_plot ) ){
# 
#     time <- times_to_plot[ t ]
# 
#     if( input_data == 'positions' ){
# 
#       if( time >= '12:00:00' ){
# 
#         timestamp_to_plot <- as.POSIXct( paste( as.character( unique( tracks_or_pos$night ) ), time ), tz = 'UTC' )
# 
#       }else{
# 
#         timestamp_to_plot <- as.POSIXct( paste( as.character( as.Date( unique( tracks_or_pos$night ) ) +1 ), time ), tz = 'UTC' )
# 
#       }
# 
#       time_diffs <- abs( as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) )
# 
#       rounded_time_diffs <- round( time_diffs, digits = 6 )
#       # increase the time differences on one side of the time to plot by an arbitrary small number, so we can distinguish them if there happens to be two timestamps the same distance away from the time to plot, on either side
# 
#       if( length( rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ] ) != 0 ){
# 
#         rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ]  <- rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ] + 1e-5
# 
#       }
# 
#       min_dist_from_time <- min( unique( rounded_time_diffs ) )
# 
#       if( min_dist_from_time < 60 ){
# 
#         time_dat <- tracks_or_pos[ which( rounded_time_diffs == min_dist_from_time ), ]
# 
#       }else{
# 
#         time_dat <- tracks_or_pos[ 0, ]
# 
#       }
# 
#     }else{
# 
#       time_dat <- tracks_or_pos[ tracks_or_pos$local_time == time, ]
# 
#     }
# 
#     if( nrow( time_dat ) > 0 ){
# 
#       plot( tracks_to_plot$x_final, tracks_to_plot$y_final, cex = 0.1, pch = 16, xaxt = 'n', yaxt = 'n' )
# 
#       points( time_dat$x_final, time_dat$y_final, col = colors[ t ], pch = 16, cex = 1.5 )
# 
#       mtext( unique( time_dat$local_timestamp ), line = -1, cex = 0.6 )
#     }
#   }
# }

# 
# plot_time <- "22:00:00"
# 
# par( mfrow = c( 3, 2 ) )
# par( oma = c( 0, 0, 0, 0 ) )
# par( mar = rep( 0, 4 ) )
# 
# 
# for( s in 1:length( tracks_or_pos_list ) ){
# 
#   tracks_or_pos <- tracks_or_pos_list[[ s ]]
# 
#   tracks_to_plot <- tracks_or_pos[ seq( 1, nrow( tracks_or_pos ), by = 10 ), ]
# 
#   ## plot where everyone is at the plot time listed above
# 
#   if( input_data == 'positions' ){
# 
#     if( plot_time >= '12:00:00' ){
# 
#       timestamp_to_plot <- as.POSIXct( paste( as.character( unique( tracks_or_pos$night ) ), plot_time ), tz = 'UTC' )
# 
#     }else{
# 
#       timestamp_to_plot <- as.POSIXct( paste( as.character( as.Date( unique( tracks_or_pos$night ) ) +1 ), plot_time ), tz = 'UTC' )
# 
#     }
# 
#     time_diffs <- abs( as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) )
# 
#     rounded_time_diffs <- round( time_diffs, digits = 6 )
#     # increase the time differences on one side of the time to plot by an arbitrary small number, so we can distinguish them if there happens to be two timestamps the same distance away from the time to plot, on either side
# 
#     if( length( rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ] ) != 0 ){
# 
#       rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ]  <- rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ] + 1e-5
# 
#     }
# 
#     min_dist_from_time <- min( unique( rounded_time_diffs ) )
# 
#     if( min_dist_from_time < 60 ){
# 
#       time_dat <- tracks_or_pos[ which( rounded_time_diffs == min_dist_from_time ), ]
# 
#     }else{
# 
#       time_dat <- tracks_or_pos[ 0, ]
# 
#     }
# 
#   }else{
# 
#     time_dat <- tracks_or_pos[ tracks_or_pos$local_time == plot_time, ]
# 
#   }
# 
#   if( nrow( time_dat ) > 0 ){
# 
#     plot( tracks_to_plot$x_final, tracks_to_plot$y_final, cex = 0.1, pch = 16, xaxt = 'n', yaxt = 'n' )
# 
#     points( time_dat$x_final, time_dat$y_final, col = 'green', pch = 16 )
# 
#     mtext( unique( time_dat$local_timestamp ), line = -1, cex = 0.6 )
#   }
# }





start_hms_time <- '15:00:00' # this is the time of when to start extracting spatial networks for each night of thermal data
start_num_time <- as.numeric( as_hms( start_hms_time ) )

end_hms_time <- '10:00:00' # this is the time of when to stop extracting spatial networks for each night of thermal data

if( end_hms_time > '12:00:00' ){

  end_num_time <- as.numeric( as_hms( end_hms_time ) )

}else{

  end_num_time <- as.numeric( as_hms( end_hms_time ) ) + 24*60*60

}



inter <- 30*60  #this is the interval between the spatial networks to extract in seconds

times_of_int_temp <- seq( start_num_time, end_num_time, by = inter )

times_of_int_temp[ times_of_int_temp >= 24*60*60 ] <- times_of_int_temp[ times_of_int_temp >= 24*60*60 ] - 24*60*60

times_of_int <- as.character( as_hms( times_of_int_temp ) )

spatial_net_measures <- as.data.frame( matrix( NA, nrow = 0, ncol = 15 ) )

nights <- sapply( tracks_or_pos_list, function( x ) as.character( unique( as.Date( x[ , 'local_timestamp' ] - 12*60*60 ) ) ) )

spatial_net_measures <- data.frame( night = rep( nights, each = length( times_of_int ) ), local_time = rep( times_of_int, times = length( nights ) ), edge_sums = NA, edge_sums_corr = NA, ave_weighted_degree = NA, density = NA, global_transitivity = NA, ave_local_transitivity = NA, modularity = NA, mean_path_length = NA, average_eccent = NA, radius = NA, diameter = NA, efficiency = NA, median_nn_dist = NA, median_nn_within_3m = NA, small_worldness = NA )

spatial_net_measures$local_timestamp <- ifelse( spatial_net_measures$local_time < '12:00:00', paste( ( as.Date( spatial_net_measures$night ) + 1 ), spatial_net_measures$local_time ),  paste( as.Date( spatial_net_measures$night ), spatial_net_measures$local_time ) )


for( s in 1:length( tracks_or_pos_list ) ){

  tracks_or_pos <- tracks_or_pos_list[[ s ]]

  for( ti in 1:length( times_of_int ) ){

    time <- times_of_int[ ti ]

    if( input_data == 'positions' ){

      if( time >= '12:00:00' ){

        timestamp_to_plot <- as.POSIXct( paste( as.character( unique( tracks_or_pos$night ) ), time ), tz = 'UTC' )

      }else{

        timestamp_to_plot <- as.POSIXct( paste( as.character( as.Date( unique( tracks_or_pos$night ) ) +1 ), time ), tz = 'UTC' )

      }

      time_diffs <- abs( as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) )

      rounded_time_diffs <- round( time_diffs, digits = 6 )
      # increase the time differences on one side of the time to plot by an arbitrary small number, so we can distinguish them if there happens to be two timestamps the same distance away from the time to plot, on either side

      if( length( rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ] ) != 0 ){

        rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ]  <- rounded_time_diffs[ as.numeric( tracks_or_pos$local_timestamp - timestamp_to_plot, units = 'secs' ) < 0 ] + 1e-5

      }

      min_dist_from_time <- min( unique( rounded_time_diffs ) )

      if( min_dist_from_time < 60 ){

        time_dat <- tracks_or_pos[ which( rounded_time_diffs == min_dist_from_time ), ]

      }else{

        time_dat <- tracks_or_pos[ 0, ]

      }

    }else{

      time_dat <- tracks_or_pos[ tracks_or_pos$local_time == time, ]

    }

    if( nrow( time_dat ) > 1 ){

      night <- as.character( unique( as.Date( tracks_or_pos$local_timestamp - 12*60*60 ) ) )

      time_dat <- time_dat[ !is.na( time_dat$x_final ), ]

      # adding a tenth to make it so that there a no 0's in the dyadic distances (otherwise it will become infinity with the inverse )
      sp_network <- 1 /  ( pointDistance( time_dat[ , c( 'x_final', 'y_final' ) ], lonlat = F ) + 0.01 )

      dyadic_distances <- pointDistance( time_dat[ , c( 'x_final', 'y_final' ) ], lonlat = F )

      diag( dyadic_distances ) <- NA

      nn_dists <- apply( dyadic_distances, 1, na_min )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'median_nn_dist' ] <- median( nn_dists )

      nn_num <- apply( dyadic_distances, 1, function( x ) sum( x <= 3, na.rm = T ) )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'median_nn_within_3m' ] <- median( nn_num )

      # spot check
      1 / sqrt( ( time_dat$x_final[ 1 ] - time_dat$x_final[ 2 ] )**2 + ( time_dat$y_final[ 1 ] - time_dat$y_final[ 2 ] )**2 )

      sp_network[ 1, 2 ]

      sp_network[ lower.tri( sp_network, diag = T ) ] <- NA

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'edge_sums' ] <- sum( sp_network, na.rm = T )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'edge_sums_corr' ] <- sum( sp_network, na.rm = T ) / ( nrow( time_dat ) * ( nrow( time_dat ) - 1 ) / 2 )

      sp_net_dup <-  1 / pointDistance( time_dat[ , c( 'x_final', 'y_final' ) ], lonlat = F )
      diag( sp_net_dup ) <- NA

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'ave_weighted_degree' ] <-  mean( apply( sp_net_dup, 1, FUN = mean, na.rm = T ) )

      ## now moving onto the measures that require binary (unweighted) edges
      dist_cutoff <- 3

      sp_network[ which( sp_network < ( 1 / dist_cutoff ) ) ] <- 0

      g_trimmed <- graph_from_adjacency_matrix( sp_network, mode = 'undirected', diag = F, weighted = T )

      plot( g_trimmed )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'density' ] <- graph.density( g_trimmed )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'global_transitivity' ]  <- transitivity( g_trimmed, type = 'global' )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'ave_local_transitivity' ] <- mean( transitivity( g_trimmed, type = 'local' ), na.rm = T )

      attempt <- try( cluster_walktrap( g_trimmed ) )

      if( 'try-error' %in% class( attempt ) ){

        spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'modularity' ] <- NA

        plot.new( )

      }else{

        wtc <- cluster_walktrap( g_trimmed )

        spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'modularity' ] <- modularity( g_trimmed, membership = membership( wtc ) )

        plot( wtc, g_trimmed )

      }

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'mean_path_length' ] <- mean_distance( g_trimmed )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'average_eccent' ] <- mean( eccentricity( g_trimmed ) )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'radius' ] <- radius( g_trimmed )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'diameter' ] <- diameter( g_trimmed )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'efficiency' ] <- brainGraph::efficiency( g_trimmed, type = 'global', weights = E( g_trimmed )$weight )

      small_world <- qgraph::smallworldness( g_trimmed )

      spatial_net_measures[ spatial_net_measures$night == night & spatial_net_measures$local_time == time, 'small_worldness' ] <- small_world[ 'smallworldness' ]

    }
  }
}

spatial_net_measures <- spatial_net_measures[ !is.na( spatial_net_measures$edge_sums ), ]

write.csv( spatial_net_measures, paste0( 'DATA/thermal_tracks/spatial_net_measures_', input_data, '.csv' ), row.names = F )




