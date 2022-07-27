


library(foreach)
library(doParallel)
library( plyr )
library( stringr )
library( hms )
library( data.table )
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


input_data <- 'uncorrected' # this should be run with uncorrected to reproduce dissertation results 

files_to_merge <- list.files( paste( "DATA/thermal_tracks/smooth_tracks", input_data, sep = '/' ), full.names = T )

night_start <- '21:00'

night_end <- '05:00'

num_rands <- 1000


num_tracks_moving <- data.frame( matrix( NA, nrow = 0, ncol = 3 + num_rands ) )

names( num_tracks_moving ) <- c( 'local_timestamp', 'night', 'emp', paste( 'rand', 1:num_rands, sep = '_' ) )

num_tracks_present <- data.frame( matrix( NA, nrow = 0, ncol = 3 + num_rands ) )

names( num_tracks_present ) <- c( 'local_timestamp', 'night', 'emp', paste( 'rand', 1:num_rands, sep = '_' ) )


### This below is just commented out because it has already been run

if( str_split( getwd(), '/', simplify = T )[ 1, 3 ] == 'jcl273' ){

  registerDoParallel( length( files_to_merge ) )

}else{

  registerDoParallel( 10 )

}



rand_results <- foreach( d = 1:length( files_to_merge ), .packages = .packages()  ) %dopar% {

#for( d in 1:length( files_to_merge ) ){

  smooth_tracks <- readRDS( files_to_merge[[ d ]] )

  smooth_tracks$moving <- ifelse( smooth_tracks$moving_predicted == 'stationary', 0, 1 )

  min_time <- max( min( smooth_tracks$local_time ), night_start )

  if( length( smooth_tracks$local_time[ smooth_tracks$local_time < '12:00:00' ] ) > 0 ){

    max_track_time <- max( smooth_tracks$local_time[ smooth_tracks$local_time < '12:00:00' ] )

    max_time <- min( max_track_time, night_end )

    night_dat <- smooth_tracks[ smooth_tracks$local_time >= min_time | smooth_tracks$local_time <= max_time, ]

  }else{

    max_time <- max( smooth_tracks$local_time )

    night_dat <- smooth_tracks[ smooth_tracks$local_time >= min_time & smooth_tracks$local_time <= max_time, ]

  }

  if( nrow( night_dat ) > 0 ){

    wide_night <- reshape2::dcast( night_dat, local_timestamp ~ id, value.var = 'moving'  )

    uniq_tracklets <- as.character( unique( night_dat$id ) )

    print( sum( ! uniq_tracklets %in% names( wide_night ) ) )

    print( sum( ! names( wide_night ) %in% uniq_tracklets ) )

    emp <- apply( wide_night[ , 2 : ( length( uniq_tracklets ) + 1 ) ], 1, FUN = na_sum )

    num_tracks_moving_temp <- data.frame( matrix( NA, nrow = length( emp ), ncol = 3 + num_rands ) )

    names( num_tracks_moving_temp ) <- c( 'local_timestamp', 'night', 'emp', paste( 'rand', 1:num_rands, sep = '_' ) )

    num_tracks_moving_temp$emp <- emp

    num_tracks_moving_temp$local_timestamp <- wide_night$local_timestamp

    night_date <- unique( as.Date( smooth_tracks$local_timestamp - 12*60*60 ) )

    num_tracks_moving_temp$night <- night_date


    num_tracks_present_temp <- data.frame( matrix( NA, nrow = length( emp ), ncol = 3 + num_rands ) )

    names( num_tracks_present_temp ) <- c( 'local_timestamp', 'night', 'emp', paste( 'rand', 1:num_rands, sep = '_' ) )

    num_tracks_present_temp$emp <- apply( wide_night[ , 2 : ( length( uniq_tracklets ) + 1 ) ], 1, FUN = function( x ) sum( !is.na( x ) ) )

    num_tracks_present_temp$local_timestamp <- wide_night$local_timestamp

    num_tracks_present_temp$night <- night_date

    for( i in 1:num_rands ){

      wide_night_rand <- wide_night

      if( i %% 100 == 0 ) print( i )

      for( tracklet in uniq_tracklets ){

        shift <- sample( 1: nrow( wide_night_rand ), 1 )

        new_inds <- 1:nrow( wide_night_rand ) + shift
        new_inds <- new_inds %% nrow( wide_night_rand ) + 1

        wide_night_rand[ , tracklet ] <- wide_night_rand[ , tracklet ][ new_inds ]

      }

      num_tracks_moving_temp[ , paste( 'rand', i, sep = '_' ) ] <- apply( wide_night_rand[ , 2 : ( length( uniq_tracklets ) + 1 ) ], 1, FUN = na_sum )

      num_tracks_present_temp[ , paste( 'rand', i, sep = '_' ) ] <- apply( wide_night_rand[ , 2 : ( length( uniq_tracklets ) + 1 ) ], 1, FUN = function( x ) sum( !is.na( x ) ) )

    }

    return( list( num_tracks_moving_temp,  num_tracks_present_temp ) )
  }

}

stopImplicitCluster()

num_tracks_moving_list <- lapply( rand_results, FUN = function( x ) x[[ 1 ]] )

num_tracks_moving <- ldply( num_tracks_moving_list )

nrow( num_tracks_moving )

sum( num_tracks_moving$emp )

sum( num_tracks_moving$rand_1 )


num_tracks_present_list <- lapply( rand_results, FUN = function( x ) x[[ 2 ]] )

num_tracks_present <- ldply( num_tracks_present_list )


dir.create( paste( getwd(), 'RESULTS/', sep = '/' ) )

dir.create( paste( getwd(), 'RESULTS/track_rand_results/', sep = '/' ) )

saveRDS( num_tracks_moving, file = 'RESULTS/track_rand_results/num_tracks_moving_updated.rds' )
saveRDS( num_tracks_present, file = 'RESULTS/track_rand_results/num_tracks_present_updated.rds' )



num_tracks_moving <- readRDS( file = 'RESULTS/track_rand_results/num_tracks_moving_updated.rds' )
num_tracks_present <- readRDS(  file = 'RESULTS/track_rand_results/num_tracks_present_updated.rds' )


ave_moving_func <- function( x ){
  
  return( mean( x[ which( x > 0 ) ] ) )
  
}




someone_moving <- apply( num_tracks_moving[ , -( 1:2 ) ], 2, FUN = function( x ) sum( x > 0 ) / sum( !is.na( x ) ) )

real_someone_moving <- someone_moving[ 1 ]

rands_someone_moving <- someone_moving[ -1 ]

rands_dens <- density( rands_someone_moving )

plot( 1, type = 'n', xlim = range( c( real_someone_moving, rands_dens$x ) ), ylim = range( rands_dens$y ), xlab = 'Proportion of seconds during all nights (21:00 - 05:00) when at least one individual is moving', ylab = 'Probability density', bty = 'l' )

points( rands_dens$x, rands_dens$y, type = 'l' )

abline( v = real_someone_moving, col = 'red', lty = 2 )

# p-value
mean( rands_someone_moving <= real_someone_moving )



## black for presentation
plot( 1, type = 'n', xlim = range( c( real_someone_moving, rands_dens$x ) ), ylim = range( rands_dens$y ), xlab = 'Proportion of seconds during all nights (21:00 - 05:00) when at least one individual is moving', ylab = 'Probability density', bty = 'l', col.axis = 'white', col.lab = 'white', col.main = 'white', col = 'white' )
axis(1, col = 'white', labels = F )
axis(2, col = 'white', labels = F )

points( rands_dens$x, rands_dens$y, type = 'l', col = 'white' )


rel_quants <- quantile( rands_someone_moving, c( 0.025, 0.975 ) )

abline( v = real_someone_moving, col = 'red', lty = 1 )

abline( v = rel_quants, col = 'white', lty = 2 )





ave_moving <- apply( num_tracks_moving[ , -( 1:2 ) ], 2, ave_moving_func )

real_ave_moving <- ave_moving[ 1 ]

rands_ave_moving <- ave_moving[ -1 ]

rands_dens <- density( rands_ave_moving )

plot( 1, type = 'n', xlim = range( c( real_ave_moving, rands_dens$x ) ), ylim = range( rands_dens$y ), xlab = 'Average number of baboons moving, when there is movement in the group', ylab = 'Probability density', bty = 'l' )

points( rands_dens$x, rands_dens$y, type = 'l' )

abline( v = real_ave_moving, col = 'red', lty = 2 )

# p-value
mean( rands_ave_moving >= real_ave_moving )



### black for presentation
plot( 1, type = 'n', xlim = range( c( real_ave_moving, rands_dens$x ) ), ylim = range( rands_dens$y ), xlab = 'Average number of baboons moving, when there is movement in the group', ylab = 'Probability density', bty = 'l', col.axis = 'white', col.lab = 'white', col.main = 'white', col = 'white' )
axis(1, col = 'white', labels = F )
axis(2, col = 'white', labels = F )

points( rands_dens$x, rands_dens$y, type = 'l', col = 'white' )


rel_quants <- quantile( rands_ave_moving, c( 0.025, 0.975 ) )

abline( v = real_ave_moving, col = 'red', lty = 1 )

abline( v = rel_quants, col = 'white', lty = 2 )


# p-value
mean( rands_ave_moving >= real_ave_moving )




### are these results just a consequence of the tracks being more synchronous in their existance, and thus inherently also more synchronous in their behavior, in the empirical data than the randomized data?

var_tracks_present <- apply( num_tracks_present[ , -( 1:2 ) ], 2, FUN = sd )

real_var_tracks_present <- var_tracks_present[ 1 ]

rands_var_tracks_present <- var_tracks_present[ -1 ]

rands_dens <- density( rands_var_tracks_present )

plot( 1, type = 'n', xlim = range( c( real_var_tracks_present, rands_dens$x ) ), ylim = range( rands_dens$y ), xlab = 'Standard deviation in the number of tracks present at a given time', ylab = 'Probability density', bty = 'l' )

points( rands_dens$x, rands_dens$y, type = 'l' )

abline( v = real_var_tracks_present, col = 'red', lty = 2 )

# p-value
mean( rands_var_tracks_present <= real_var_tracks_present )

### answer: No. The tracks are actually LESS synchronous in their existence in the empirical data than the randomized data, making the previous results even more impressive

