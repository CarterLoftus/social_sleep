

library( stringr )
library( data.table )
library( doParallel )
library( foreach )


input_data <- 'uncorrected' ## can be 'corrected' or 'uncorrected'

files <- list.files( path = paste0( "DATA/thermal_tracks/auto_stitch_across_vids/", input_data ) )

nights <- str_split_fixed( files, "_", 2 )[, 1 ]

should_plot <- F

transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}


calib_method <- 'height'

dir.create( paste( getwd(), "DATA/thermal_tracks/spatially_corrected_tracks", sep = '/' ) )
dir.create( paste( getwd(), "DATA/thermal_tracks/spatially_corrected_tracks", input_data, sep = '/' ) )

if( str_split( getwd( ), "/", simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  registerDoParallel( length( nights ) )
  
}else{
  
  registerDoParallel( 4 )
  
}


foreach( ni = 1:length( nights ), .packages = .packages() ) %dopar% {
  
  print( nights[ ni ] ) 
  
  therm_tracks <- readRDS( paste0( "DATA/thermal_tracks/auto_stitch_across_vids/", input_data, '/', nights[ ni ], "_tracks.rds" ) )
  
  therm_tracks <- as.data.frame( therm_tracks )
  
  if( should_plot ){
    
    therm_tracks <- therm_tracks[ seq( 1, nrow( therm_tracks ), by = 10 ), ]
    
  }
  
  therm_tracks$timestamp <- as.POSIXct( therm_tracks$timestamp, tz = 'UTC' )    
  

  calib_csv <- read.csv( "DATA/thermal_spatial_correction/spatial_correction.csv" )
  
  head( calib_csv )
  
  
  ## flip the y-axis in both the dataframes (because in a plot, a higher y value refers to higher up in the plot. But image pixels are indexed by row number, with the first row being at the top of the image, so a higher y value means the bottom of the image)
  therm_tracks$y <- - therm_tracks$y
  
  calib_csv[ , grepl( 'y', names( calib_csv ) ) ] <- -calib_csv[ , grepl( 'y', names( calib_csv ) ) ]
  
  
  if( should_plot ){
    
    plot( therm_tracks$x, therm_tracks$y, pch = 16, col = transp( 'black', 0.5 ) )
    
    points( calib_csv$bottom_left_x , calib_csv$bottom_left_y , col = 'red', pch = 16, cex = .7 )
    points( calib_csv$bottom_right_x , calib_csv$bottom_right_y , col = 'red', pch = 16, cex = .7 )
    points( calib_csv$top_right_x , calib_csv$top_right_y , col = 'red', pch = 16, cex = .7 )
    points( calib_csv$top_left_x , calib_csv$top_left_y , col = 'red', pch = 16, cex = .7 )
    
  }
  
  
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
  
  if( should_plot ){
    
    abline( coef( ledge_mod ), col = 'red' )
    
  }
  
  # the arctangent of the slope of the regression line gives the angle of the line with respect to the horizontal
  angle <- abs( atan( m ) )
  
  # rotate all coordinates by this angle
  therm_tracks$x_rot <- cos( angle )*therm_tracks$x - sin( angle )*therm_tracks$y
  
  therm_tracks$y_rot <- sin( angle )*therm_tracks$x + cos( angle )*therm_tracks$y
  
  # rotate the calibration coordinates as well
  
  calib_csv$bottom_left_x_rot <- cos( angle )*calib_csv$bottom_left_x - sin( angle )*calib_csv$bottom_left_y
  calib_csv$bottom_right_x_rot <- cos( angle )*calib_csv$bottom_right_x - sin( angle )*calib_csv$bottom_right_y
  calib_csv$top_right_x_rot <- cos( angle )*calib_csv$top_right_x - sin( angle )*calib_csv$top_right_y
  calib_csv$top_left_x_rot <- cos( angle )*calib_csv$top_left_x - sin( angle )*calib_csv$top_left_y
  
  calib_csv$bottom_left_y_rot <- sin( angle )*calib_csv$bottom_left_x + cos( angle )*calib_csv$bottom_left_y
  calib_csv$bottom_right_y_rot <- sin( angle )*calib_csv$bottom_right_x + cos( angle )*calib_csv$bottom_right_y
  calib_csv$top_right_y_rot <- sin( angle )*calib_csv$top_right_x + cos( angle )*calib_csv$top_right_y
  calib_csv$top_left_y_rot <- sin( angle )*calib_csv$top_left_x + cos( angle )*calib_csv$top_left_y
  
  if( should_plot ){
    
    plot( therm_tracks$x_rot, therm_tracks$y_rot, pch = 16, col = transp( 'black', 0.5 ) )
    
    points( calib_csv$bottom_left_x_rot, calib_csv$bottom_left_y_rot, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv$bottom_right_x_rot, calib_csv$bottom_right_y_rot, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv$top_right_x_rot, calib_csv$top_right_y_rot, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv$top_left_x_rot, calib_csv$top_left_y_rot, col = 'red', pch = 16, cex = 0.7 )
    
  }
  
  calib_csv_ext <- calib_csv
  
  calib_csv_ext$width_midpoint_x <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'x', names( calib_csv ) ) & grepl( 'rot', names( calib_csv ) ) ] ) ) )
  calib_csv_ext$width_midpoint_y <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'y_rot', names( calib_csv ) ) ] ) ) )
  
  calib_csv_ext$height_left_midpoint_x <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'x', names( calib_csv ) ) & grepl( 'rot', names( calib_csv ) ) & grepl( 'left', names( calib_csv ) ) ] ) ) )
  calib_csv_ext$height_left_midpoint_y <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'y_rot', names( calib_csv ) ) & grepl( 'left', names( calib_csv ) ) ] ) ) )
  
  calib_csv_ext$height_right_midpoint_x <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'x', names( calib_csv ) ) & grepl( 'rot', names( calib_csv ) ) & grepl( 'right', names( calib_csv ) ) ] ) ) )
  calib_csv_ext$height_right_midpoint_y <- apply( calib_csv, 1, function( x ) mean( as.numeric( x[ grepl( 'y_rot', names( calib_csv ) ) & grepl( 'right', names( calib_csv ) ) ] ) ) )
  
  if( should_plot ){
    
    points( calib_csv_ext$width_midpoint_x, calib_csv_ext$width_midpoint_y, cex = 2, col = 'red', pch = 16 )
    
    points( calib_csv_ext$height_right_midpoint_x, calib_csv_ext$height_right_midpoint_y, cex = 0.5, col = 'green', pch = 16 )
    points( calib_csv_ext$height_left_midpoint_x, calib_csv_ext$height_left_midpoint_y, cex = 0.5, col = 'green', pch = 16 )
    
  }
  
  
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
  
  
  therm_tracks$x_corr <- sapply( therm_tracks$x_rot, x_coord_corr )
  max( therm_tracks$x_corr, na.rm = T ) # 23.367
  
  calib_csv_ext$bottom_left_x_corr <- sapply( calib_csv_ext$bottom_left_x_rot, x_coord_corr )
  calib_csv_ext$bottom_right_x_corr <- sapply( calib_csv_ext$bottom_right_x_rot, x_coord_corr )
  calib_csv_ext$top_right_x_corr <- sapply( calib_csv_ext$top_right_x_rot, x_coord_corr )
  calib_csv_ext$top_left_x_corr <- sapply( calib_csv_ext$top_left_x_rot, x_coord_corr )
  
  if( should_plot ){
    
    plot( therm_tracks$x_corr, therm_tracks$y_rot, pch = 16, col = transp( 'black', 0.5 ) )
    
    points( calib_csv_ext$bottom_left_x_corr, calib_csv_ext$bottom_left_y_rot, col = 'red', pch = 16, cex = 0.5 )
    points( calib_csv_ext$bottom_right_x_corr, calib_csv_ext$bottom_right_y_rot, col = 'red', pch = 16, cex = 0.5 )
    points( calib_csv_ext$top_right_x_corr, calib_csv_ext$top_right_y_rot, col = 'red', pch = 16, cex = 0.5 )
    points( calib_csv_ext$top_left_x_corr, calib_csv_ext$top_left_y_rot, col = 'red', pch = 16, cex = 0.5 )
    
  }
  
  
  ### just plot the width in terms of the x values as the y values haven't been corrected yet
  
  calib_csv_ext$bottom_width_corr <- sqrt( ( calib_csv_ext$bottom_left_x_corr - calib_csv_ext$bottom_right_x_corr )**2 )
  
  calib_csv_ext$top_width_corr <- sqrt( ( calib_csv_ext$top_left_x_corr - calib_csv_ext$top_right_x_corr )**2  )
  
  
  calib_csv_ext$mean_width_corr <- apply( matrix( c( calib_csv_ext$bottom_width_corr, calib_csv_ext$top_width_corr ), ncol = 2 ), 1, mean )  
  
  
  plot( calib_csv_ext$width_midpoint_x, calib_csv_ext$mean_width_corr, xlab = 'Position of width_midpoint of object (in # of pixels) from left side of frame', ylab = 'Width of object' )
  
  ######## correct the y values now 
  
  ### size of pixels in meters is equal to k / ( m*x + b )
  
  mean_y <- mean( therm_tracks$y_rot, na.rm = T )
  
  therm_tracks$y_cent <- therm_tracks$y_rot - mean_y
  
  calib_csv_ext$bottom_left_y_cent <- calib_csv_ext$bottom_left_y_rot - mean_y
  calib_csv_ext$bottom_right_y_cent <- calib_csv_ext$bottom_right_y_rot - mean_y
  calib_csv_ext$top_right_y_cent <- calib_csv_ext$top_right_y_rot - mean_y
  calib_csv_ext$top_left_y_cent <- calib_csv_ext$top_left_y_rot - mean_y
  
  if( should_plot ){
    
    plot( therm_tracks$x_rot, therm_tracks$y_cent )
    
    points( calib_csv_ext$bottom_left_x_rot, calib_csv_ext$bottom_left_y_cent, col = 'red', pch = 16, cex = 0.5 )
    points( calib_csv_ext$bottom_right_x_rot, calib_csv_ext$bottom_right_y_cent, col = 'red', pch = 16, cex = 0.5 )
    points( calib_csv_ext$top_right_x_rot, calib_csv_ext$top_right_y_cent, col = 'red', pch = 16, cex = 0.5 )
    points( calib_csv_ext$top_left_x_rot, calib_csv_ext$top_left_y_cent, col = 'red', pch = 16, cex = 0.5 )
    
  }
  
  ### ok, y axis is successfully centered. Now apply the correction
  
  therm_tracks$y_corr <- therm_tracks$y_cent*( k / ( ( m*therm_tracks$x_rot ) + b ) )
  
  calib_csv_ext$bottom_left_y_corr <- calib_csv_ext$bottom_left_y_cent*( k / ( ( m*calib_csv_ext$bottom_left_x_rot ) + b ) )
  calib_csv_ext$bottom_right_y_corr <- calib_csv_ext$bottom_right_y_cent*( k / ( ( m*calib_csv_ext$bottom_right_x_rot ) + b ) )
  calib_csv_ext$top_right_y_corr <- calib_csv_ext$top_right_y_cent*( k / ( ( m*calib_csv_ext$top_right_x_rot ) + b ) )
  calib_csv_ext$top_left_y_corr <- calib_csv_ext$top_left_y_cent*( k / ( ( m*calib_csv_ext$top_left_x_rot ) + b ) )
  
  if( should_plot ){
    
    plot( therm_tracks$x_corr, therm_tracks$y_corr, pch = 16, col = transp( 'black', 0.5 ) )
    
    points( calib_csv_ext$bottom_left_x_corr, calib_csv_ext$bottom_left_y_corr, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv_ext$bottom_right_x_corr, calib_csv_ext$bottom_right_y_corr, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv_ext$top_right_x_corr, calib_csv_ext$top_right_y_corr, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv_ext$top_left_x_corr, calib_csv_ext$top_left_y_corr, col = 'red', pch = 16, cex = 0.7 )
    
  }
  
  
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
  
  therm_tracks$x_final <- therm_tracks$x_corr
  
  calib_csv_ext$bottom_left_x_final <- calib_csv_ext$bottom_left_x_corr
  calib_csv_ext$bottom_right_x_final <- calib_csv_ext$bottom_right_x_corr
  calib_csv_ext$top_right_x_final <- calib_csv_ext$top_right_x_corr
  calib_csv_ext$top_left_x_final <- calib_csv_ext$top_left_x_corr
  
  
  min_y <- min( therm_tracks$y_corr, na.rm = T )
  
  therm_tracks$y_final <- therm_tracks$y_corr + abs( min_y )
  
  calib_csv_ext$bottom_left_y_final <- calib_csv_ext$bottom_left_y_corr + abs( min_y )
  calib_csv_ext$bottom_right_y_final <- calib_csv_ext$bottom_right_y_corr + abs( min_y )
  calib_csv_ext$top_right_y_final <- calib_csv_ext$top_right_y_corr + abs( min_y )
  calib_csv_ext$top_left_y_final <- calib_csv_ext$top_left_y_corr + abs( min_y )
  
  if( should_plot ){
    
    plot( therm_tracks$x_final, therm_tracks$y_final, pch = 16, col = transp( 'black', 0.5 ) )
    
    points( calib_csv_ext$bottom_left_x_final, calib_csv_ext$bottom_left_y_final, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv_ext$bottom_right_x_final, calib_csv_ext$bottom_right_y_final, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv_ext$top_right_x_final, calib_csv_ext$top_right_y_final, col = 'red', pch = 16, cex = 0.7 )
    points( calib_csv_ext$top_left_x_final, calib_csv_ext$top_left_y_final, col = 'red', pch = 16, cex = 0.7 )
    
    
  }
  
  if( !should_plot ){
    
    therm_tracks$local_timestamp <- therm_tracks$timestamp + 3*60*60
    
    therm_tracks_trim <- therm_tracks[ , c( 'unique_ID', 'local_timestamp', 'x_final', 'y_final', 'vid_name' ) ]
    
    
    
    saveRDS(  therm_tracks_trim,  paste0( "DATA/thermal_tracks/spatially_corrected_tracks/", input_data, '/', nights[ ni ], "_tracks.rds" ) )
    
  }
  
  
}


stopImplicitCluster( )




