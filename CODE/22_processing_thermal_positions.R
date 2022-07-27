


options( digits.secs = 6 ) #reset options, in order to see the milliseconds

library( hms )
library( plyr )
library( stringr )
library( doParallel )
library( foreach )

calib_method = 'height'


## this bit of code at the top out just pulls the positions data from the server onto the local computer

# # if we aren't working on the Davis computer and therefore have access to the department server, extract the relevant files from the department server onto the local machine. This also needs to be run even if we already have the data on our local machine, as it puts the files into a new folder that we will use going forward. If the data is already on the local machine and not on the server, just change the input_tracks_file_path accordingly

if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] != 'jcl273' ){

    dir.create( paste( getwd(), 'DATA/tracking_output/raw_positions_more_tracks_parameters', sep = '/' ) )

    input_vids_path <- 'Z:/baboon/archive/processed/video/thermal/2019_summer/cliff_data/mp4_files/viewpoint_1/T1020'

    input_tracks_path <- "Z:/baboon/archive/pubs/Loftus_dissertation/social_sleep/DATA/tracking_output/more_tracks_parameters"

    folders <- list.files( input_vids_path )

    for( folder in folders ){

      txt_files <- list.files( paste( input_vids_path, folder, sep = '/' ), pattern = '.txt' )

      dir.create( paste( getwd(), 'DATA/tracking_output/raw_positions_more_tracks_parameters', folder, sep = '/' ) )

      file.copy( from = paste( input_vids_path, folder, txt_files, sep = '/' ), to = paste( getwd(), 'DATA/tracking_output/raw_positions_more_tracks_parameters', folder, txt_files, sep = '/' ), overwrite = T )

      vid_names <- str_split_fixed( txt_files, '.txt', 2 )[ , 1 ]

      csv_names <- paste0( 'positions_', vid_names, '.csv' )

      file.copy( from = paste( input_tracks_path, folder, csv_names, sep = '/' ), to = paste( getwd(), 'DATA/tracking_output/raw_positions_more_tracks_parameters', folder, csv_names, sep = '/' ), overwrite = T )

    }
  }
}


stopImplicitCluster()

## add timestamps to the detections and merge them across hours within the same night. This only needs to be done once

dir.create( paste0( getwd(), "/DATA/thermal_tracks/raw_positions_processed" ) )


nights <- list.files( path = 'DATA/tracking_output/raw_positions_more_tracks_parameters', full.names = F )

if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  registerDoParallel( length( nights ) )
  
}else{
  
  registerDoParallel( 10 )
  
}

foreach( ni = 1:length( nights ), .packages = .packages() ) %dopar% {
  
  files_to_merge <- list.files( path = paste0( 'DATA/tracking_output/raw_positions_more_tracks_parameters/', nights[ ni ] ), pattern = '.csv', full.names = T )
  
  files_to_merge <-  sort( files_to_merge )
  
  dfs <- lapply( as.list( files_to_merge ), read.csv )
  
  print( files_to_merge )
  
  ## view the positions from the thermal
  lapply( dfs, head )
  
  txt_files <- list.files( path = paste0( 'DATA/tracking_output/raw_positions_more_tracks_parameters/', nights[ ni ] ), pattern = '.txt', full.names = T, recursive = T )
  
  txt_files <-  sort( txt_files )
  
  timestamp_txts <- lapply( as.list( txt_files ), read.table ) # read in each txt file with the frame information. These txt files give the timestamp of each frame
  
  ## add timestamp to the thermal tracking data frames
  for( i in 1:length( timestamp_txts ) ){ # for each hour of video recording (i.e. each separate video)...
    
    frame_info <- timestamp_txts[[ i ]] # save the dataframe with the timestamps from this video
    
    vid_name <- str_split_fixed( basename( txt_files[ i ] ), ".txt", 2 )[ , 1 ] # split the string to extract the name of the video
    
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
    
    if( sum( is.na( dfs[[ i ]]$timestamp ) ) > 0 ){ # check to make sure all locations in the tracklet dataframe received a timestamp
      
      stop( "some timestamps not matched up" )
    }
  }
  
  therm_positions <- ldply( dfs )
  
  # cut the night of 20190809 before the baboon pulls the camera down
  if( nights[ ni ] == '20190809' ){ 
    
    therm_positions <- therm_positions[ therm_positions$timestamp < as.POSIXct( '2019-08-09 15:40:40', tz = 'UTC' ), ]
    
  }
  
  
  ### now do the spatial correction of the detections
  
  therm_positions$timestamp <- as.POSIXct( therm_positions$timestamp, tz = 'UTC' )    

  calib_csv <- read.csv( "DATA/thermal_spatial_correction/spatial_correction.csv" )
  
  head( calib_csv )
  
  
  ## flip the y-axis in both the dataframes (because in a plot, a higher y value refers to higher up in the plot. But image pixels are indexed by row number, with the first row being at the top of the image, so a higher y value means the bottom of the image)
  therm_positions$y <- - therm_positions$y
  
  calib_csv[ , grepl( 'y', names( calib_csv ) ) ] <- -calib_csv[ , grepl( 'y', names( calib_csv ) ) ]
  
  # change the necessary columns to numeric
  for( i in 3:10 ){
    
    calib_csv[ , i ] <- as.numeric( calib_csv[ , i ] )
  }
  
  ### rotate everything now so that the 
  
  ledge_coords <- read.csv( "DATA/thermal_spatial_correction/coords_along_cliff.csv" )
  
  names( ledge_coords ) <- c( 'x', 'y' )
  
  head( ledge_coords )
  
  # flip the y-axis
  ledge_coords$y <- -ledge_coords$y
  
  ledge_mod <- lm( ledge_coords$y ~ ledge_coords$x )
  
  m <- coef( ledge_mod )[ 2 ]
  
  # the arctangent of the slope of the regression line gives the angle of the line with respect to the horizontal
  angle <- abs( atan( m ) )
  
  # rotate all coordinates by this angle
  therm_positions$x_rot <- cos( angle )*therm_positions$x - sin( angle )*therm_positions$y
  
  therm_positions$y_rot <- sin( angle )*therm_positions$x + cos( angle )*therm_positions$y
  
  # rotate the calibration coordinates as well
  
  calib_csv$bottom_left_x_rot <- cos( angle )*calib_csv$bottom_left_x - sin( angle )*calib_csv$bottom_left_y
  calib_csv$bottom_right_x_rot <- cos( angle )*calib_csv$bottom_right_x - sin( angle )*calib_csv$bottom_right_y
  calib_csv$top_right_x_rot <- cos( angle )*calib_csv$top_right_x - sin( angle )*calib_csv$top_right_y
  calib_csv$top_left_x_rot <- cos( angle )*calib_csv$top_left_x - sin( angle )*calib_csv$top_left_y
  
  calib_csv$bottom_left_y_rot <- sin( angle )*calib_csv$bottom_left_x + cos( angle )*calib_csv$bottom_left_y
  calib_csv$bottom_right_y_rot <- sin( angle )*calib_csv$bottom_right_x + cos( angle )*calib_csv$bottom_right_y
  calib_csv$top_right_y_rot <- sin( angle )*calib_csv$top_right_x + cos( angle )*calib_csv$top_right_y
  calib_csv$top_left_y_rot <- sin( angle )*calib_csv$top_left_x + cos( angle )*calib_csv$top_left_y
  
  
  calib_csv_ext <- calib_csv
  
  calib_csv_ext$width_midpoint_x <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'x', names( calib_csv ) ) & grepl( 'rot', names( calib_csv ) ) ] ) ) )
  calib_csv_ext$width_midpoint_y <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'y_rot', names( calib_csv ) ) ] ) ) )
  
  calib_csv_ext$height_left_midpoint_x <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'x', names( calib_csv ) ) & grepl( 'rot', names( calib_csv ) ) & grepl( 'left', names( calib_csv ) ) ] ) ) )
  calib_csv_ext$height_left_midpoint_y <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'y_rot', names( calib_csv ) ) & grepl( 'left', names( calib_csv ) ) ] ) ) )
  
  calib_csv_ext$height_right_midpoint_x <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'x', names( calib_csv ) ) & grepl( 'rot', names( calib_csv ) ) & grepl( 'right', names( calib_csv ) ) ] ) ) )
  calib_csv_ext$height_right_midpoint_y <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'y_rot', names( calib_csv ) ) & grepl( 'right', names( calib_csv ) ) ] ) ) )
  
  if( calib_method == 'width' ){
    
    calib_csv_ext$bottom_width <- sqrt( ( calib_csv_ext$bottom_left_x_rot - calib_csv_ext$bottom_right_x_rot )**2 + ( calib_csv_ext$bottom_left_y_rot - calib_csv_ext$bottom_right_y_rot )**2 )
    
    calib_csv_ext$top_width <- sqrt( ( calib_csv_ext$top_left_x_rot - calib_csv_ext$top_right_x_rot )**2 + ( calib_csv_ext$top_left_y_rot - calib_csv_ext$top_right_y_rot )**2 )
    
    ## make sure that the bottom and top widths are the same
    plot( calib_csv_ext$bottom_width, calib_csv_ext$top_width )
    
    calib_csv_ext$mean_width <- apply( matrix( c( calib_csv_ext$bottom_width, calib_csv_ext$top_width ), ncol = 2 ), 1, mean )  
    
    plot( calib_csv_ext$width_midpoint_x, calib_csv_ext$mean_width, xlab = 'Position of midpoint of object (in # of pixels) from original right side of frame', ylab = 'Width of object' )
    
    # now model it so that we can develop a scaling
    
    mod <- lm( calib_csv_ext$mean_width ~ calib_csv_ext$width_midpoint_x )
    summary( mod )
    
    coef( mod )
    
    b <- coef( mod )[ 1 ]
    m <- coef( mod )[ 2 ]
    
    abline( coef( mod ), col = 'red' )
    
    k <- 0.27305 ## for width of bottle. This is 10.75 inches
    
  }else{
    
    if( calib_method == 'height' ){
      
      calib_csv_ext$left_height <- sqrt( ( calib_csv_ext$bottom_left_x_rot - calib_csv_ext$top_left_x_rot )**2 + ( calib_csv_ext$bottom_left_y_rot - calib_csv_ext$top_left_y_rot )**2 )
      
      calib_csv_ext$right_height <- sqrt( ( calib_csv_ext$bottom_right_x_rot - calib_csv_ext$top_right_x_rot )**2 + ( calib_csv_ext$bottom_right_y_rot - calib_csv_ext$top_right_y_rot )**2 )
      
      heights <- c( calib_csv_ext$left_height, calib_csv_ext$right_height )
      height_midpoints <- c( calib_csv_ext$height_left_midpoint_x, calib_csv_ext$height_right_midpoint_x )
      
      ## are the right side and left side the same height?
      plot( calib_csv_ext$left_height, calib_csv_ext$right_height )
      
      plot( height_midpoints, heights, xlab = 'Position of height midpoint of object (in # of pixels) from right side of frame', ylab = 'height of object' )
      
      # now model it so that we can develop a scaling
      
      mod <- lm( heights ~ height_midpoints )
      summary( mod )
      
      coef( mod )
      
      b <- coef( mod )[ 1 ]
      m <- coef( mod )[ 2 ]
      
      abline( coef( mod ), col = 'red' )
      
      k <- 0.4064 ## for height of bottle. This is equal to 16 inches
      
    }
  }
  
  
  ### now make the actual corrections to the x and y coordinates of the tracks (and calibration bottle)
  
  x_coord_corr <- function( old_x_coord ){
    
    if( is.na( old_x_coord ) ){
      return( NA )
    }
    
    return( ( k*log( abs( b + m*old_x_coord ) ) / m ) - ( k*log( abs( b ) ) / m ) ) 
    
  }
  
  
  therm_positions$x_corr <- sapply( therm_positions$x_rot, x_coord_corr )
  max( therm_positions$x_corr, na.rm = T ) # 23.367
  
  calib_csv_ext$bottom_left_x_corr <- sapply( calib_csv_ext$bottom_left_x_rot, x_coord_corr )
  calib_csv_ext$bottom_right_x_corr <- sapply( calib_csv_ext$bottom_right_x_rot, x_coord_corr )
  calib_csv_ext$top_right_x_corr <- sapply( calib_csv_ext$top_right_x_rot, x_coord_corr )
  calib_csv_ext$top_left_x_corr <- sapply( calib_csv_ext$top_left_x_rot, x_coord_corr )
  
  
  ### just plot the width in terms of the x values as the y values haven't been corrected yet
  
  calib_csv_ext$bottom_width_corr <- sqrt( ( calib_csv_ext$bottom_left_x_corr - calib_csv_ext$bottom_right_x_corr )**2 )
  
  calib_csv_ext$top_width_corr <- sqrt( ( calib_csv_ext$top_left_x_corr - calib_csv_ext$top_right_x_corr )**2  )
  
  
  calib_csv_ext$mean_width_corr <- apply( matrix( c( calib_csv_ext$bottom_width_corr, calib_csv_ext$top_width_corr ), ncol = 2 ), 1, mean )  
  
  
  plot( calib_csv_ext$width_midpoint_x, calib_csv_ext$mean_width_corr, xlab = 'Position of width_midpoint of object (in # of pixels) from left side of frame', ylab = 'Width of object' )
  
  ######## correct the y values now 
  
  ### size of pixels in meters is equal to k / ( m*x + b )
  
  mean_y <- mean( therm_positions$y_rot, na.rm = T )
  
  therm_positions$y_cent <- therm_positions$y_rot - mean_y
  
  calib_csv_ext$bottom_left_y_cent <- calib_csv_ext$bottom_left_y_rot - mean_y
  calib_csv_ext$bottom_right_y_cent <- calib_csv_ext$bottom_right_y_rot - mean_y
  calib_csv_ext$top_right_y_cent <- calib_csv_ext$top_right_y_rot - mean_y
  calib_csv_ext$top_left_y_cent <- calib_csv_ext$top_left_y_rot - mean_y
  
  
  ### ok, y axis is successfully centered. Now apply the correction
  
  therm_positions$y_corr <- therm_positions$y_cent*( k / ( ( m*therm_positions$x_rot ) + b ) )
  
  calib_csv_ext$bottom_left_y_corr <- calib_csv_ext$bottom_left_y_cent*( k / ( ( m*calib_csv_ext$bottom_left_x_rot ) + b ) )
  calib_csv_ext$bottom_right_y_corr <- calib_csv_ext$bottom_right_y_cent*( k / ( ( m*calib_csv_ext$bottom_right_x_rot ) + b ) )
  calib_csv_ext$top_right_y_corr <- calib_csv_ext$top_right_y_cent*( k / ( ( m*calib_csv_ext$top_right_x_rot ) + b ) )
  calib_csv_ext$top_left_y_corr <- calib_csv_ext$top_left_y_cent*( k / ( ( m*calib_csv_ext$top_left_x_rot ) + b ) )
  
  
  calib_csv_ext$bottom_width_corr <- sqrt( ( calib_csv_ext$bottom_left_x_corr - calib_csv_ext$bottom_right_x_corr )**2 + ( calib_csv_ext$bottom_left_y_corr - calib_csv_ext$bottom_right_y_corr )**2 )
  
  calib_csv_ext$top_width_corr <- sqrt( ( calib_csv_ext$top_left_x_corr - calib_csv_ext$top_right_x_corr )**2 + ( calib_csv_ext$top_left_y_corr - calib_csv_ext$top_right_y_corr )**2 )
  
  calib_csv_ext$mean_width_corr <- apply( matrix( c( calib_csv_ext$bottom_width_corr, calib_csv_ext$top_width_corr ), ncol = 2 ), 1, mean )  
  
  plot( calib_csv_ext$width_midpoint_x, calib_csv_ext$mean_width_corr, xlab = 'x coord of midpoint of object (in # of pixels) from original right side of frame', ylab = 'Width of object' )
  
  
  
  calib_csv_ext$left_height_corr <- sqrt( ( calib_csv_ext$bottom_left_x_corr - calib_csv_ext$top_left_x_corr )**2 + ( calib_csv_ext$bottom_left_y_corr - calib_csv_ext$top_left_y_corr )**2 )
  
  calib_csv_ext$right_height_corr <- sqrt( ( calib_csv_ext$bottom_right_x_corr - calib_csv_ext$top_right_x_corr )**2 + ( calib_csv_ext$bottom_right_y_corr - calib_csv_ext$top_right_y_corr )**2 )
  
  heights <- c( calib_csv_ext$left_height_corr, calib_csv_ext$right_height_corr )
  height_midpoints <- c( calib_csv_ext$height_left_midpoint_x, calib_csv_ext$height_right_midpoint_x )
  
  ## are the right side and left side the same height?
  plot( calib_csv_ext$left_height_corr, calib_csv_ext$right_height_corr )
  
  plot( height_midpoints, heights, xlab = 'x coord of height midpoint (in # of pixels) from original right side of frame', ylab = 'height of object' )
  
  
  ## now make the y coordinates positive
  
  therm_positions$x_final <- therm_positions$x_corr
  
  calib_csv_ext$bottom_left_x_final <- calib_csv_ext$bottom_left_x_corr
  calib_csv_ext$bottom_right_x_final <- calib_csv_ext$bottom_right_x_corr
  calib_csv_ext$top_right_x_final <- calib_csv_ext$top_right_x_corr
  calib_csv_ext$top_left_x_final <- calib_csv_ext$top_left_x_corr
  
  
  min_y <- min( therm_positions$y_corr, na.rm = T )
  
  therm_positions$y_final <- therm_positions$y_corr + abs( min_y )
  
  calib_csv_ext$bottom_left_y_final <- calib_csv_ext$bottom_left_y_corr + abs( min_y )
  calib_csv_ext$bottom_right_y_final <- calib_csv_ext$bottom_right_y_corr + abs( min_y )
  calib_csv_ext$top_right_y_final <- calib_csv_ext$top_right_y_corr + abs( min_y )
  calib_csv_ext$top_left_y_final <- calib_csv_ext$top_left_y_corr + abs( min_y )
  
  
  therm_positions$local_timestamp <- therm_positions$timestamp + 3*60*60
  
  therm_positions_trim <- therm_positions[ , c( 'local_timestamp', 'x_final', 'y_final' ) ]
  
  therm_positions_trim$night <- as.character( as.Date( therm_positions_trim$local_timestamp - 12*60*60 ), tz = 'UTC' )
  
  therm_positions_trim$local_time <- str_split_fixed( therm_positions_trim$local_timestamp, ' ', 2 )[ , 2 ]
  
  saveRDS(  therm_positions_trim,  paste0( "DATA/thermal_tracks/raw_positions_processed/", nights[ ni ], "_positions.rds" ) )
  
}





stopImplicitCluster()



