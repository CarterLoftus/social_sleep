
library( data.table )
library( RTransferEntropy )
library( NlinTS )
library( doParallel )
library( foreach )
library( stringr )
library( lubridate )



#### testing for influence in the waking network #####

night_start <- '21:00:00'

night_end <- '05:00:00'

total_dat <- readRDS( 'DATA/sleep_analysis_data/total_dat.rds' )

head( total_dat )

history_length <- 3

night_dat <- total_dat[ ( total_dat$local_time > night_start | total_dat$local_time < night_end ), ]

tag_names <- names( night_dat )[ ! names( night_dat ) %in% c( 'local_timestamp', 'local_time', 'night' ) ]



#### look at the autocorrelation to decide on a history length ####


par( mfrow = c( 4, 4 ) )


for( tag in tag_names ){
  
  par( mar = c( 2,2,2,2) )

  pacf( ( night_dat[ , tag ][ !is.na( night_dat[ , tag ] ) ] ), main = "" )
  
  title( main = tag, line = 1 )
  
}



### transfer entropy for the whole study period during the night

num_rands <- 1000

te_arr_emp <- array( NA, dim = c( length( tag_names ), length( tag_names ) ), dimnames = list( tag_names, tag_names ) )

te_arr_rand <- array( NA, dim = c( length( tag_names ), length( tag_names ), num_rands ), dimnames = list( tag_names, tag_names, paste0( 'rand_', 1:num_rands ) ) )


for( n in 1:num_rands ){

  print( n )

  for( i in 1:length( tag_names ) ){

    for( j in 1:length( tag_names ) ){

      if( i != j ){

        vec_i <- night_dat[ , tag_names[ i ] ]

        vec_j <- night_dat[ , tag_names[ j ] ]

        min_ind_i <- min( which( !is.na( vec_i ) ) )

        min_ind_j <- min( which( !is.na( vec_j ) ) )

        min_ind <- max( min_ind_i, min_ind_j )


        max_ind_i <- max( which( !is.na( vec_i ) ) )

        max_ind_j <- max( which( !is.na( vec_j ) ) )

        max_ind <- min( max_ind_i, max_ind_j )


        vec_i <- vec_i[ min_ind:max_ind ]

        vec_j <- vec_j[ min_ind:max_ind ]


        if( length( vec_i ) > 1000 & length( vec_j ) > 1000 ){

          if( n == 1 ){

            te_arr_emp[ i, j ] <- calc_te( vec_i, vec_j, lx = history_length, ly = history_length )

          }

          shift <- sample( 1:length( vec_i ), 1 )

          new_inds <- 1:length( vec_i ) + shift
          new_inds <- new_inds %% length( vec_i ) + 1

          te_arr_rand[ i, j, n ] <- calc_te( vec_i, vec_j[ new_inds ], lx = history_length, ly = history_length )

        }
      }
    }
  }
}

dir.create( paste0( getwd(), '/RESULTS' ) )
dir.create( paste0( getwd(), '/RESULTS/transfer_entropy_results' ) )

saveRDS( te_arr_emp, 'RESULTS/transfer_entropy_results/te_arr_emp_night_his_length_3_both.rds' ) ### te_arr_emp is the empirical transfer entropies from the whole study period aggregated

saveRDS( te_arr_rand, 'RESULTS/transfer_entropy_results/te_arr_rand_night_his_length_3_both.rds' ) ### te_arr_emp is the randomized transfer entropies from the whole study period aggregated (one matrix slice per randomization)


te_arr_emp <- readRDS( 'RESULTS/transfer_entropy_results/te_arr_emp_night_his_length_3_both.rds' )

te_arr_rand <- readRDS( 'RESULTS/transfer_entropy_results/te_arr_rand_night_his_length_3_both.rds' )


### sum the whole matrix 
emp_total_te <- sum( te_arr_emp, na.rm = T )

rand_total_te <- apply( te_arr_rand, 3, sum, na.rm = T )

rand_dens <- density( rand_total_te, na.rm = T )

par( bg = 'black' )

plot( 1, type = 'n', bty = 'l', xlim = range( c( emp_total_te, rand_dens$x ) ),  ylim = range( c( emp_total_te, rand_dens$y ) ), ylab = 'Probability density', xlab = 'Total sleep state transfer entropy within group', col = 'white', col.axis = 'white', col.lab = 'white', col.main = 'white' ) 

axis( 1, labels = F, col = 'white' )
axis( 2, labels = F, col = 'white' )


rel_quants <- quantile( rand_total_te, c( 0.025, 0.975 ) )

abline( v = emp_total_te, col = 'red', lty = 1 )

abline( v = rel_quants, col = 'white', lty = 2 )

points( rand_dens$x, rand_dens$y, type = 'l', col = 'white', lty = 1 )
#legend( 'top', legend = c( 'empirical', 'time-shifted' ), lty = c( 2, 1 ), col = c( 'red', 'black' ), bty = 'n' )


###################### save networks that I will use to test for correlation between transfer entropy network and social network ##############

night_start <- '21:00:00'
night_end <- '05:00:00'

history_length <- 1

night_dat <- total_dat[ ( total_dat$local_time > night_start | total_dat$local_time < night_end ), ]

tag_names <- names( night_dat )[ ! names( night_dat ) %in% c( 'local_timestamp', 'local_time', 'night' ) ]


agg_eff_te_net <- array( NA, dim = c( length( tag_names ), length( tag_names ) ), dimnames = list( tag_names, tag_names ) )


for( i in 1:length( tag_names ) ){
  
  for( j in 1:length( tag_names ) ){
    
    if( i != j ){
      
      vec_i <- night_dat[ , tag_names[ i ] ]
      
      vec_j <- night_dat[ , tag_names[ j ] ]
      
      min_ind_i <- min( which( !is.na( vec_i ) ) )
      
      min_ind_j <- min( which( !is.na( vec_j ) ) )
      
      min_ind <- max( min_ind_i, min_ind_j )
      
      
      max_ind_i <- max( which( !is.na( vec_i ) ) )
      
      max_ind_j <- max( which( !is.na( vec_j ) ) )
      
      max_ind <- min( max_ind_i, max_ind_j )
      
      
      vec_i <- vec_i[ min_ind:max_ind ]
      
      vec_j <- vec_j[ min_ind:max_ind ]
      
      
      agg_eff_te_net[ i, j ] <- calc_te( vec_i, vec_j, lx = history_length, ly = history_length )
      
    }
  }
}



dir.create( paste0( getwd(), '/DATA' ) )
dir.create( paste0( getwd(), '/DATA/social_data' ) )

saveRDS( agg_eff_te_net, 'DATA/social_data/agg_te_net.rds' ) ## agg_te_net is the empirical effective transfer entropies from the whole study period aggregated only during the night

