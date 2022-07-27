


library(brms)
library( stats )
library( sjPlot )
library( data.table )
library( lubridate )
library( ggplot2 )

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}

na_sum <- function( x ){
  
  if( sum( is.na( x ) ) == length( x ) ){
    
    return( NA )
    
  }else{
    
    return( sum( x, na.rm = T ) )
  }
}

night_start <- "21:00:00"

night_end <- "05:00:00"


class_meth <- 'percentile_thresh'

sep_thresh <- T

sleep_column <- 'sleep_bouts'

if( class_meth != 'percentile_thresh' ){

  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '.csv' ) )

}else{

  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ) )

}

full_dat <- as.data.frame( full_dat )

head( full_dat )


#### add columns for awake and waking

full_dat$curr_awake <- 1 - full_dat$sleep_bouts

full_dat$curr_awake_conservative <- NA

full_dat$just_awoke <- NA

full_dat$just_awoke_conservative <- NA

wake_block <- 3 ## number of minutes that individuals need to remain awake for to be considered awake in the curr_awake_conservative


tag_names <- as.character( unique( full_dat$tag ) )

for( tag in tag_names ){

  tag_dat <- full_dat[ full_dat$tag == tag, ]

  tag_nights <- as.character( unique( tag_dat$night ) )

  for( night in tag_nights ){

    night_dat <- tag_dat[ tag_dat$night == night, ]

    wake_rle <- rle( night_dat$curr_awake )

    wake_runs <- as.numeric( rep( wake_rle$lengths > wake_block, times = wake_rle$lengths ) )

    night_dat$curr_awake_conservative <- as.numeric( night_dat$curr_awake == 1 & wake_runs == 1 )

    night_dat$just_awoke <- as.numeric( c( NA, diff( night_dat$curr_awake ) ) == 1 )

    night_dat$just_awoke_conservative <- as.numeric( c( NA, diff( night_dat$curr_awake_conservative ) ) == 1 )

    tag_dat[ tag_dat$night == night, ] <- night_dat

  }

  full_dat[ full_dat$tag == tag, ] <- tag_dat

}




## trimming the data to nights when we have most of the group collared

nights <- sort( unique( full_dat$night ) )

num_active_collars <- c()

for( night in nights ){

  night_dat <- full_dat[ full_dat$night == night, ]

  num_active_collars <- c( num_active_collars, length( unique( night_dat$tag ) ) )
}


plot( nights, num_active_collars )

abline( v = 23, lty = 2 )

### after day 23 we drop down to 18 tags, from the original 10, so let's keep it to the first 18 days

full_dat_trim <- full_dat[ !is.na( full_dat$sleep_bouts ) & full_dat$night > 1 & full_dat$night < 24 & ( full_dat$local_time > night_start | full_dat$local_time < night_end ), ]

full_dat_trim <- full_dat_trim[ order( full_dat_trim$local_timestamp ), ]
full_dat_trim <- full_dat_trim[ order( full_dat_trim$tag ), ]

full_dat_trim$others_awake_prev_min <- NA
full_dat_trim$others_wakening_prev_min <- NA
full_dat_trim$others_wakening_curr_min <- NA
full_dat_trim$others_awake_prev_min_conservative <- NA
full_dat_trim$others_wakening_prev_min_conservative <- NA
full_dat_trim$others_wakening_curr_min_conservative <- NA
full_dat_trim$self_awake_prev_min <- NA
full_dat_trim$self_awake_prev_min_conservative <- NA


identical( full_dat_trim$local_timestamp, round_date( full_dat_trim$local_timestamp, unit = 'min' ) ) # just to confirm that all bursts are on the minute (important for the subtraction of 60 seconds below)


for( i in 1:nrow( full_dat_trim ) ){

  if( i %% 1000 == 0) print( i / nrow( full_dat_trim ) )

  others_prev_min_dat <- full_dat_trim[ full_dat_trim$tag != full_dat_trim$tag[ i ] & full_dat_trim$local_timestamp == ( full_dat_trim$local_timestamp[ i ] - 60 ), ]

  others_curr_min_dat <- full_dat_trim[ full_dat_trim$tag != full_dat_trim$tag[ i ] & full_dat_trim$local_timestamp == full_dat_trim$local_timestamp[ i ] , ]

  full_dat_trim$others_awake_prev_min[ i ] <- na_sum( others_prev_min_dat$curr_awake )

  if( sum( is.na( others_prev_min_dat$curr_awake ) ) != 0 ) stop( 'woahhhh' )

  full_dat_trim$others_wakening_prev_min[ i ] <- na_sum( others_prev_min_dat$just_awoke )

  full_dat_trim$others_wakening_curr_min[ i ] <- na_sum( others_curr_min_dat$just_awoke )

  ## now the conservative versions as well
  full_dat_trim$others_awake_prev_min_conservative[ i ] <- na_sum( others_prev_min_dat$curr_awake_conservative )

  if( sum( is.na( others_prev_min_dat$curr_awake_conservative ) ) != 0 ) stop( 'woahhhh' )

  full_dat_trim$others_wakening_prev_min_conservative[ i ] <- na_sum( others_prev_min_dat$just_awoke_conservative )

  full_dat_trim$others_wakening_curr_min_conservative[ i ] <- na_sum( others_curr_min_dat$just_awoke_conservative )

  self_prev_min_dat <- full_dat_trim[ full_dat_trim$tag == full_dat_trim$tag[ i ] & full_dat_trim$local_timestamp == ( full_dat_trim$local_timestamp[ i ] - 60 ), ]

  if( nrow( self_prev_min_dat ) > 0 ){

    full_dat_trim$self_awake_prev_min[ i ] <- self_prev_min_dat$curr_awake

    full_dat_trim$self_awake_prev_min_conservative[ i ] <- self_prev_min_dat$curr_awake_conservative

  }

}


saveRDS( object = full_dat_trim, file = "DATA/sleep_analysis_data/full_dat_trim_for_focal_waking_propagation.rds" )

full_dat_trim <- readRDS( file = "DATA/sleep_analysis_data/full_dat_trim_for_focal_waking_propagation.rds" )

full_dat_trim$night_start <- as.POSIXct( paste( full_dat_trim$night_date, night_start ), tz = 'UTC' )

full_dat_trim$time_since_night_start <- as.numeric( full_dat_trim$local_timestamp - full_dat_trim$night_start, unit = 'mins' )




## show the autocorrelation in the data
was_asleep_dat <- full_dat_trim[ which( full_dat_trim$self_awake_prev_min == 0 ), ] 

pacf( was_asleep_dat$curr_awake, main = '' )

abline( v = 15.2, lty = 2 ) 



nrow( was_asleep_dat )

options( mc.cores = parallel::detectCores() )

hist( was_asleep_dat$curr_awake )

was_asleep_dat <- was_asleep_dat[ seq( 1, nrow( was_asleep_dat ), by = 15 ), ]


table( was_asleep_dat$curr_awake, was_asleep_dat$others_wakening_prev_min )

priors <- c( prior(normal(0,2), class = "b", coef = others_wakening_prev_min ),
             prior(normal(0,2), class = "b", coef = stime_since_night_start_1 ) )

priors <- get_prior( curr_awake ~ others_wakening_prev_min + s( time_since_night_start ) + ( 1 | night ) + ( 1 | tag ), data = was_asleep_dat, family= "bernoulli" )

sleep_prop_mod <- brm( curr_awake ~ others_wakening_prev_min + s( time_since_night_start ) + ( 1 | night ) + ( 1 | tag ), data = was_asleep_dat, family= "bernoulli", iter = 5000, prior = priors, control = list(max_treedepth = 15, adapt_delta = .999999999999 ) )

dir.create( paste( getwd(), "RESULTS/wake_propagation_models", sep = '/' ) )

saveRDS( object = sleep_prop_mod, file = "RESULTS/wake_propagation_models/sleep_prop_mod_downsample_15.rds" )




sleep_prop_mod <- readRDS( "RESULTS/wake_propagation_models/sleep_prop_mod_downsample_15.rds" )

summary( sleep_prop_mod )

plot( conditional_effects( sleep_prop_mod, 'others_wakening_prev_min' ), theme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) )[[ 1 ]] + labs( y = 'Probability of waking', x = 'Number of other individuals that awoke in previous minute') # + ylim( range( TST_model_dat$TST ) )



mod_dat_to_plot <- conditional_effects( sleep_prop_mod, 'others_wakening_prev_min' )[[1]]

plot( mod_dat_to_plot$others_wakening_prev_min, mod_dat_to_plot$estimate__, type = 'l', xlim = range( mod_dat_to_plot$others_wakening_prev_min ), ylim = range( c( mod_dat_to_plot$lower__, mod_dat_to_plot$upper__  ) ), bty = 'l', xlab = 'Number of group-mates that awoke in previous minute', ylab = 'Probability of waking' ) 

polygon( x = c( mod_dat_to_plot$others_wakening_prev_min, rev( mod_dat_to_plot$others_wakening_prev_min ) ), y = c( mod_dat_to_plot$lower__, rev( mod_dat_to_plot$upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot$others_wakening_prev_min, mod_dat_to_plot$estimate__, type = 'l', lwd = 2, col = 'blue' )


## black for presentation
mod_dat_to_plot <- conditional_effects( sleep_prop_mod, 'others_wakening_prev_min' )[[1]]

plot( mod_dat_to_plot$others_wakening_prev_min, mod_dat_to_plot$estimate__, type = 'l', xlim = range( mod_dat_to_plot$others_wakening_prev_min ), ylim = range( c( mod_dat_to_plot$lower__, mod_dat_to_plot$upper__  ) ), bty = 'l', xlab = 'Number of group-mates that awoke in previous minute', ylab = 'Probability of waking', col = 'white', col.axis = 'white', col.lab = 'white', col.main = 'white' ) 

axis( 1, labels = F, col = 'white' )
axis( 2, labels = F, col = 'white' )


polygon( x = c( mod_dat_to_plot$others_wakening_prev_min, rev( mod_dat_to_plot$others_wakening_prev_min ) ), y = c( mod_dat_to_plot$lower__, rev( mod_dat_to_plot$upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot$others_wakening_prev_min, mod_dat_to_plot$estimate__, type = 'l', lwd = 2, col = 'white' )




pp_check( sleep_prop_mod )

tab_model( sleep_prop_mod )

