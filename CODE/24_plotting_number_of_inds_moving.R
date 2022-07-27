




library(foreach)
library(doParallel)
library( plyr )
library( stringr )
library( hms )
library( data.table )
library( zoo )
library( lubridate )

na_sum <- function( x ){
  
  if( sum( !is.na( x ) ) == 0 ){
    
    return( NA )
    
  }else{
    
    return( sum( x, na.rm = T ) )
  }
}


doing_same <- function( x ){
  
  if( sum( !is.na( x ) ) == 0 ){
    
    return( NA )
    
  }else{
    
    s1 <- sum( x == 0, na.rm = T ) / sum( !is.na( x ) )
    
    s2 <- max( s1, 1 - s1 )
    
    
    return( s2 )
  }
}


input_data <- 'uncorrected'

files_to_merge <- list.files( paste( "DATA/thermal_tracks/smooth_tracks", input_data, sep = '/' ), full.names = T )

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}



tracks_list <- lapply( files_to_merge, readRDS )

tracks_df <- ldply( tracks_list )

tracks_df$time_num <- as.numeric( as_hms( tracks_df$local_time ) )

tracks_df$time_num[ tracks_df$local_time < '12:00:00' ] <- tracks_df$time_num[ tracks_df$local_time < '12:00:00' ] + 24*60*60


plot( tracks_df$time_num, rep( 1, length( tracks_df$time_num ) ), type = 'n', ylim = c( 0, 0.8 ), xaxt = 'n', xlab = 'Local time', ylab = 'Proportion of group members moving in infrared imagery' )
axis( 1, at = seq( 0, 36*60*60, by = 60*60 ), labels = c( as_hms( seq( 0, 23*60*60, by = 60*60 ) ), as_hms( seq( 0, 12*60*60, by = 60*60 ) ) ) )

head( tracks_df )

nights <- as.character( unique( tracks_df$night ) )

for( night in nights ){
  
  night_dat <- tracks_df[ tracks_df$night == night, ]
  
  agg_dat <- aggregate( night_dat$moving_predicted, by = list( night_dat$time_num ), FUN = function( x ) mean( x == 'non_stationary', na.rm = T ) )
  
  names( agg_dat ) <- c( 'time_num', 'prop_moving' )
  
  agg_dat$prop_moving_smooth <- rollmean( agg_dat$prop_moving, k = 61, na.pad = T )
   
  
  
  #plot( agg_dat$time_num, agg_dat$prop_moving, type = 'l' )
  points( agg_dat$time_num, agg_dat$prop_moving_smooth, type = 'l', col = transp( 'grey', 0.7 ) )
  
  
  
}

night <- '2019-08-01'

night_dat <- tracks_df[ tracks_df$night == night, ]

agg_dat <- aggregate( night_dat$moving_predicted, by = list( night_dat$time_num ), FUN = function( x ) mean( x == 'non_stationary', na.rm = T ) )

names( agg_dat ) <- c( 'time_num', 'prop_moving' )

agg_dat$prop_moving_smooth <- rollmean( agg_dat$prop_moving, k = 61, na.pad = T )

#plot( agg_dat$time_num, agg_dat$prop_moving, type = 'l' )
points( agg_dat$time_num, agg_dat$prop_moving_smooth, type = 'l', col = transp( 'blue', 1 ) )



