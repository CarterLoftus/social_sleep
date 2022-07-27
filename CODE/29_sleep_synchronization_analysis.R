
library( zoo )
library( data.table )
library( stringr )
library( brms )
library( sjPlot )
library( ggplot2 )
library( lubridate )
library( plyr )

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}

## function for normalizing a vector
normalize_func <- function( x ) return( (x - mean( x, na.rm = T ) )/ sd( x, na.rm = T ) )

na.max <- function( x ){
  
  if( sum( !is.na( x ) ) == 0 ){
    
    return( NA )
  }else{
    
    return( max( x, na.rm = T ) )
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

################## Read in the d1 (accelerometer burst) data ###################


total_dat <- readRDS( 'DATA/sleep_analysis_data/total_dat.rds' )


night_start <- '21:00:00'

night_end <- '05:00:00'

tag_names <- names( total_dat )[ startsWith( names( total_dat ), '2' ) | startsWith( names( total_dat ), '6' ) ] 


### this loop is going to remove partial nights of data at the end of an individual's data so that we are only randomizing full nights of data
for( tag in tag_names ){
  
  max_ind <- max( which( !is.na( total_dat[ , tag ] ) ) )
  
  max_time <- total_dat[ max_ind, 'local_time' ]
  
  max_night <- total_dat[ max_ind, 'night' ]
  
  if( max_time < night_end | max_time > night_start ){
    
    
    total_dat[ total_dat$night == max_night, tag ] <- NA
    
  }
  
}

sent_dat <- total_dat[ total_dat$local_time > night_start | total_dat$local_time < night_end, ]

all_nights <- sort( as.character( unique( sent_dat$night ) ) )

## remove the first night because the data starts at 3 AM local time, which doesn't give much time to randomize
sent_dat <- sent_dat[ sent_dat$night != all_nights[ 1 ], ]

## calculating individual and collective vigilance ##

coll_vig_mean <- c()

coll_vig_raw <- c()

ind_vig <- c()

for( nigh in 1:length( unique( sent_dat$night ) ) ){
  
  nigh_dat <- sent_dat[ sent_dat$night == unique( sent_dat$night )[ nigh ], ]
  
  # this is a vector of the total number of minutes during the night that someone is awake for 
  coll_vig_raw <- c( coll_vig_raw, sum( apply( nigh_dat[ , 2 : ( length( tag_names ) + 1 ) ], 1, FUN = na.max ), na.rm = T ) )
  
  # this is a vector of the proportion of minutes during the night that someone is awake for
  coll_vig_mean <- c( coll_vig_mean, mean( apply( nigh_dat[ , 2 : ( length( tag_names ) + 1 ) ], 1, FUN = na.max ), na.rm = T ) )
  
  for( tag in tag_names ){
    
    t_vec <- nigh_dat[ , tag ]
    
    if( sum( !is.na( t_vec ) ) != 0 ){
      
      # this is a vector of the number of minutes that an individual is awake for
      ind_vig <- c( ind_vig, sum( t_vec, na.rm =  T ) )
      
    }
  }
}

mean( coll_vig_raw )

sd( coll_vig_raw ) / sqrt( length( coll_vig_raw ) )

mean( coll_vig_mean )

mean( ind_vig )

sd( ind_vig ) / sqrt( length( ind_vig ) )


##### Testing for group-level synchronization in sleep-wake state ###

## first comparing empirical data to randomizations produced by shifting each individual's sleep-wake data by a random amount within each given night

sync_dat <- total_dat[ total_dat$local_time > night_start | total_dat$local_time < night_end, ]

## remove the first night because there is only one individual on this night that actually has data
sync_dat <- sync_dat[ sync_dat$night != 1, ]

num_rands <- 1000

shift_results_sync <- data.frame( matrix( NA, nrow = nrow( sync_dat ), ncol = 3 + num_rands ) ) 

names( shift_results_sync ) <- c( 'local_timestamp', 'night', 'emp', paste( 'rand', 1:num_rands, sep = '_' ) )

shift_results_sync$local_timestamp <- sync_dat$local_timestamp

shift_results_sync$night <- sync_dat$night
shift_results_sync$emp <- apply( sync_dat[ , 2 : ( length( tag_names ) + 1 ) ], 1, FUN = doing_same )

emp_sync_prop <- mean( apply( sync_dat[ , 2 : ( length( tag_names ) + 1 ) ], 1, FUN = doing_same ), na.rm = T )


## now randomize

rand_sync_vec <- c()

nights <- unique( sync_dat$night )

for( n in 1:num_rands ){

  print( n )

  rand_dat <- sync_dat

  for( night in nights ){

    night_dat <- rand_dat[ rand_dat$night == night, ]

    for( tag in tag_names ){

      shift <- sample( 1: nrow( night_dat ), 1 )

      new_inds <- 1:nrow( night_dat ) + shift
      new_inds <- new_inds %% nrow( night_dat ) + 1


      night_dat[ , tag ] <- night_dat[ , tag ][ new_inds ]

    }

    rand_dat[ rand_dat$night == night, ] <- night_dat

  }

  shift_results_sync[ , paste( 'rand', n, sep = '_' ) ] <- apply( rand_dat[ , 2 : ( length( tag_names ) + 1 ) ], 1, FUN = doing_same )

  sync_prop <- mean( apply( rand_dat[ , 2 : ( length( tag_names ) + 1 ) ], 1, FUN = doing_same ), na.rm = T )

  rand_sync_vec <- c( rand_sync_vec, sync_prop )

}


dir.create( paste( getwd(), 'RESULTS', sep = '/' ) )
dir.create( paste( getwd(), 'RESULTS/sleep_sync_rand_results', sep = '/' ) )

saveRDS( shift_results_sync, file = 'RESULTS/sleep_sync_rand_results/shift_results_sync.rds' )


shift_results_sync <- readRDS( 'RESULTS/sleep_sync_rand_results/shift_results_sync.rds' )


## find the mean of each column (each column represents one randomization and the first column is the empirical)
means <- apply( shift_results_sync[ , -( 1:2 ) ], 2, mean, na.rm = T )

## pull out the mean of the empirical
emp <- means[ 1 ]

## save a vector with the mean of each randomization
rand_vec <- means[ -1 ]

## the proportion of random that are as extreme or more extreme than the empirical (aka the p-value)
mean( emp >= rand_vec )

dens <- density( rand_vec )

dens_x <- dens$x
dens_y <- dens$y

par( bg = "white" )

plot( dens_x, dens_y, ylim = range( dens_y ), xlim = range( c( dens_x, emp ) ), type = 'l', lwd = 1, xlab = 'Mean proportion of group synchronized', ylab = 'Probability density', bty = 'l' )

#polygon( x = dens_x, y = dens_y, col = transp( 'black', 0.3) )

segments( x0 = emp, x1 = emp, y0 = 0, y1 = max( dens_y), lwd = 1, col = 'red', lty = 2)



par( bg = 'black' )


plot( dens_x, dens_y, ylim = range( dens_y ), xlim = range( c( dens_x, emp ) ), type = 'l', lwd = 1, xlab = 'Mean proportion of group synchronized', ylab = 'Probability density', bty = 'l', col.axis = 'white', col.lab = 'white', col.main = 'white', col = 'white' )
axis(1, col = 'white', labels = F )
axis(2, col = 'white', labels = F )


#polygon( x = dens_x, y = dens_y, col = transp( 'white', 0.3) )

rel_quants <- quantile( rand_vec, c( 0.025, 0.975 ) )

abline( v = rel_quants, col = 'white', lty = 2 )
abline( v = emp, lwd = 1, col = 'red', lty = 1)


