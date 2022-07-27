

library(sjPlot)
library(insight)
library(httr)
library(brms)
library( zoo )
library( hms )
library( data.table )
library( stringr )
library( lubridate )
library( lmerTest )
library( plotrix )
library( suncalc )
library( LaplacesDemon )
library( dplyr )
library( purrr )
library( HDInterval )
library(multcomp)
library( nlme )
library(tidyr) 
library(lmerTest)
library( sp )
library( rgdal )
library( stats )
library(rgeos)
library( entropy )
library( reshape2 )
library( plyr )
library(rstan)
library( brms )
library(fitdistrplus)
library( gamm4 )
library(glmmTMB)
library( mgcv )
library( rstudioapi )

################# Functions #########################

## function for normalizing a vector
normalize_func <- function( x ) return( (x - mean( x, na.rm = T ) )/ sd( x, na.rm = T ) )

## function for turning tag_names into colors
babcolor<-function(IDvec){
  
  IDvec <- as.character( IDvec )
  
  outputVec <- ifelse( IDvec %in% c( '6917', '6933', '6915', '6934', '6921', '6911' ), 'skyblue', ifelse( IDvec %in% c( '6927', '6932', '6910', '2454', '2451','2428' ), 'blue' , ifelse( IDvec %in% c( '6891', '6894', '2455' ), 'grey', ifelse( IDvec %in% c( '6898', '2448', '2436' ), 'yellow', ifelse( IDvec %in% c( '6900', '6892', '6890', '6903', '2447', '6914', '6924', '6908', '2441', '2450' ), 'pink', ifelse( IDvec %in% c( '6897', '2434', '2433' ), 'red', 'black' ) ) ) ) ) )
  
  return(outputVec)
}

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}

## function for plotting times from noon to noon. It will make correct 12:00 - 24:00 to be 00:00 - 12:00 and 00:00 - 12:00 to be 12:00 to 24:00

ts_func <- function( time_vec ){
  
  num_time <- as.numeric( as_hms( time_vec ) )
  
  corr_time <- ifelse( num_time < 12*60*60, num_time + 12*60*60, num_time - 12*60*60 )
  
  return( corr_time )
  
}



class_meth <- 'percentile_thresh' # I will generalize to allowing other classification methods. For now, it only works with percentile thresh

sep_thresh <- T  # this determines whether the log VeDBA threshold that makes the determination between inactive and active should be recalculated as the declared percentile of the raw log VeDBA, and not percentile of the rolling median of the log VeDBA, when determining whether each epoch is sleep or wake (the percentile of the rolling median of the log VeDBA is used in the determination of the sleep period). This parameter is only relevant if class_meth == 'percentile_thresh'


################# Determining sleep periods with modification of Van Hees et al. 2018 method ###################

################## Read in the d1 (accelerometer burst) data ###################

## d1 is a dataframe with a row for each minute for each baboon. Each row contains the raw (or interpolated) GPS acc burst, and several different measures calculated from bursts (like VeDBA)
d1 <- fread("DATA/sleep_analysis_data/processed_ACC_emom.csv")

## turn the data table into a dataframe
d1 <- as.data.frame( d1 )

## turn timestamp into POSIX element and time into character
d1$timestamp <- as.POSIXct( d1$timestamp, tz = 'UTC' )

## some of the timestamps are not exactly on the minute because the burst occurred late. Round the timestamps to the nearest minute
d1$timestamp <- round_date( d1$timestamp, unit = 'min' )

## make a column for the time of the burst
d1$time <- str_split_fixed(d1$timestamp, " ", 2)[,2]

## change times to local time here
d1$local_timestamp <- d1$timestamp + 3*60*60

## make a column for local time
d1$local_time <- str_split_fixed(d1$local_timestamp, " ", 2)[,2]

## view the dataframe and it's summary
head(d1)

summary(d1)


dup_inds <- sort( c( which(  duplicated( d1[ , c( 'tag', 'timestamp' ) ] ) ) , which( duplicated( d1[ , c( 'tag', 'timestamp' ) ], fromLast = T ) ) ) )

d1[ dup_inds, ]

d1 <- d1[ !duplicated( d1[ , c( 'tag', 'local_timestamp' ) ] ), ]

## assign each minute of data to a given night. A night lasts from noon to noon. First, apply a time shift so that each night is a unit, and not each day
time_shift <- d1$local_timestamp - 12*60*60

## save the date of the first night of the study (the date of the night is always the date of the evening at the beginning of that night; so the first night of the study is 2012-07-31, although the data starts on 2012-08-01, because the data on that first morning is still technically part of the data for the previous night, as a night is noon to noon)
start_date <- as.Date(min(d1$local_timestamp)- 12*60*60)

## assign night as number of nights from the start of the study, with all data before the first noon representing night 1
d1$night <- as.numeric( as.Date(time_shift) - start_date + 1 )

d1$night_date <- as.Date( d1$local_timestamp - 12*60*60 )

## show how many baboon-nights there are
nrow( unique( d1[ , c( 'tag', 'night' ) ] ) )

## check where the night changes from one night to the next to see if it is at noon
d1[(diff(c( d1$night_date )) == 1),]

## save a variable denoting the total number of minutes in the day
mins_in_day <- 60*12.5 # there are 12.5 hours between 18:00:00 and 06:30:00 

missing_mins <- 45 ## this is the maximum total number of minutes of data that can be missing from a day and still have that day included in the analysis (for sleep period time and sleep based analyses; i.e. not ave_vedba)

time_gap <- 20*60 ## this is the maximum allowable time gap between two accelerometer bursts (in seconds) that can exist in a noon-to-noon period without removing this noon-to-noon period from the data

mov_window <- 9 ## this is the size of the moving window (in minutes) used in calculating the rolling median of the average VeDBA

block_size <- 30 ## duration in minutes of the blocks of continuous inactivity that will be considered sleep

gap_size <- 45 ## maximum duration between sleep blocks that will be merged

percentile_for_no_mult <- 0.90 # this is the percentile threshold of the log VeDBA within the 18:00 to 06:30 period used to classify activity vs. inactivity (without multiplying by a multiplier)

waso_block <- 3 ## this is the number of consecutive minutes of inactivity needed to classify a period as sleep. A waso_block of 1 means that anytime the value is below the threshold, the baboon in considered sleeping and anytime the value is above the threshold the baboon is considered awake

frag_block <- 2 ## this is the number of minutes of waking that need to be consecutive to be considered a wake bout during the night (other epochs of wake that do not meet this criterion will still be considered wake for WASO and wake_bouts, but not frag_wake_bouts)

dark_start <- '19:55:00' # the time at which evening astronomical twilight ends (or whatever time you want to use to consider the start of 'night')

dark_end <- '05:23:00' # the time at which morning astronomical twilight starts (or whatever time you want to use to consider the end of 'night')

## shows the time (as well as one previous time and one later time) where a minute is skipped. This shows that throughout the data, a burst at every minute is represented
sort( unique( d1$time ) ) [ which( diff( as_hms( sort( unique( d1$time ) ) ) ) != as_hms( '00:01:00' ) ) + -1:1 ] 

## again confirms that every minute is represented in the data except for one (can tell this by comparing this number to the minutes_in_day variable above)
length( unique(d1$time) )

## create a vector containing the names of each baboon
tag_names <- unique( d1$tag )

## make a copy of d1. We will fill in this new dataframe with information about if the baboon was asleep in each epoch
full_dat <- d1[ d1$local_time > "18:00:00" | d1$local_time < "06:30:00", ]

full_dat$sleep_per <- NA ## binary indicating whether a row belongs to the sleep period window
full_dat$pot_sleep <- NA ## binary indicating whether the row is below the VeDBA threshold, making it a potential sleep bout. Three or more of these in a row get labeled as sleep bouts
full_dat$sleep_bouts <- NA ## binary indicating whether the row is considered sleep, based on the waso or nap requirements
full_dat$n_bursts <- NA ## the number of bursts collected in a given noon-to-noon period (to be compared to the total number of minutes in the day). This column will indicate whether the data for a given night is insufficient to calculate the sleep period (and thus: onset, waking, SPT, sleep_eff, TST, sleep_bouts -- because this depends on a percentile of bursts' log vedba, WASO, wake_bouts, summed_VeDBA, night_VeDBA_corr, dark_TST, prev_naps, prev_day_sleep)
full_dat$max_time_diff <- NA ## the maximum difference between consecutive fixes in a given noon-to-noon period. With the previous column, this column will indicate whether the data is insufficient to calculate the sleep period (and thus: onset, waking, SPT, sleep_eff, TST, WASO, wake_bouts, summed_VeDBA, night_VeDBA_corr, prev_naps )


## create a vector containing the names of each baboon
tag_names <- ( unique( full_dat$tag ) )


# par( mfrow = c( 4, 4 ) )

## for each individual...
for( tag in tag_names ){
  
  ## subset the data to just this individual's data
  id_dat <- full_dat[ full_dat$tag == tag, ]
  
  ## create a vector the nights for which this individual has data
  nights <- unique( id_dat$night_date )
  
  #if( length( nights ) > 15 ){ # unhash this if you only want to run sleep classification for individuals that have many nights of data, and thus reliable thresholds (as their thresholds are based on a percentile of their entire 18:00 - 06:30 log VeDBA data, if class_meth is set to 'percentile_thresh' )
  
  # we are going to classify sleep based on a percentile of the individual's rolling log VeDBA for the study period, let's first find the individual's rolling log VeDBA for the full study period, and save the relevant percentile as the threshold
  
  # create an empty vector to fill with the rolling log VeDBAs from each night
  full_roll <- c()
  
  # for each night on which this individual has data
  for( night in nights ){
    
    # subset the individual's data to this night
    night_dat <- id_dat[ id_dat$night_date == night, ]
    
    # take the rolling median of the log VeDBA
    roll_log_vedba <- rollmedian( night_dat$log_vedba, mov_window, fill = NA, align = 'center' )
    
    # add the rolling medians to the vector of the individuals rolling medians for the whole study period
    full_roll <- c( full_roll, roll_log_vedba )
    
  }
  
  ## determine the threshold activity vs. inactivity threshold based on the percentile, multiplier, and the rolling median just produced
  thresh <- quantile( full_roll, percentile_for_no_mult, na.rm = T )
  
  
  if( sep_thresh ){ # if we should recalculate the log VeDBA threshold from the unsmoothed log VeDBA data before using it for the epoch by epoch sleep classification...
    
    # recalculate the threshold
    unsmooth_thresh <- quantile( id_dat$log_vedba, percentile_for_no_mult, na.rm = T )
    
  }
  
  for( night in nights ){
    
    ## subset this individual's data to just that night
    night_dat <- id_dat[ id_dat$night_date == night, ]
    
    ## create empty columns for the sleep period, potential sleep bouts, and sleep bout binary variables
    night_dat$sleep_per <- NA
    night_dat$pot_sleep <- NA
    night_dat$sleep_bouts <- NA
    
    
    ## save a column of the total number of bursts for that day. This will also make it easier to remove these days from the dataframe later
    night_dat$n_bursts <- nrow( night_dat )
    
    ## sort the timestamps, and book end them with the beginning and end of the night
    sorted_times <- c( as.POSIXct( paste( as.Date( night, origin = "1970-01-01", tz = 'UTC' ), '18:00:00' ), tz = 'UTC' ), sort( night_dat$local_timestamp ), as.POSIXct( paste( as.Date( ( night + 1 ), origin = "1970-01-01", tz = 'UTC' ), '06:30:00' ), tz = 'UTC' ) )
    
    ## find the time difference in seconds between each burst
    time_diffs <- as.numeric( diff( sorted_times, units = 'secs' ) )
    
    if( length( time_diffs ) > 0 ){ ### There is one night for one baboon with only one single burst, which is why this if statement is needed
      
      ## save a column of the maximum time difference between burst for that day (this will make it easier to pull out days with insufficient data later)
      night_dat$max_time_diff <- max( time_diffs )
      
    }else{
      
      night_dat$max_time_diff <- NA
      
    }
    
    ### find blocks of continuous inactivity
    
    ## take the rolling median of the log VeDBA and save it as a column
    roll_log_vedba <- rollmedian( night_dat$log_vedba, mov_window, fill = NA, align = 'center' )
    
    ## find the run length encoding of periods above and below the threshold
    temp <- rle( as.numeric( roll_log_vedba < thresh ) ) 
    
    ## mark the rows that are part of runs (i.e. part of chunks that are greater than the block_size of either continuous activity or continuous inactivity )
    sleep_per_runs <- as.numeric( rep( temp$lengths > block_size, times = temp$lengths ) )
    
    ## mark the rows corresponding to sleep bouts. These sleep bouts are runs of inactivity
    sleep_per_sleep_bouts <- as.numeric( roll_log_vedba < thresh & sleep_per_runs == 1 )
    
    ## find when sleep bouts start and end
    diffs <- diff( c(0, sleep_per_sleep_bouts ) )
    starts <- which( diffs == 1 ) [ -1 ]
    ends <- which( diffs == -1 )
    
    ## if there are any sleep bouts...
    if( length( which( diffs == 1 ) ) != 0 ){
      
      ## find the duration of the gaps between each sleep bout (the end of one sleep bout and the start of the next)
      gaps <- as.numeric( night_dat$local_timestamp [ starts ] - night_dat$local_timestamp [ ends[ 1: length( starts ) ] ], units = 'mins' )
      
      ## sleep bouts separated by gaps that are shorter than that specified by gap_size will be merged. Note which of these gaps are shorter than the gap_size
      inds_to_remove <- which( gaps < gap_size ) 
      
      ## if there are NO gaps between sleep bouts that are to be removed...
      if( length( inds_to_remove ) == 0 ){
        
        ## set sleep onset index to be the start of sleep bouts
        onset <- which( diffs == 1 ) 
        
        ## set waking index to be the end of sleep bouts
        wake <- ends
        
      }else{ ## if there ARE gaps between sleep bouts that are to be removed...
        
        ## set sleep onset index to be the start of sleep bouts that do not correspond to the gaps to be removed (because these will be within sleep periods, not a start of a new bout)
        onset <- which( diffs == 1 ) [ - (inds_to_remove + 1) ]
        
        ## set waking index to be the end of sleep bouts that do not correspond to the gaps to be removed
        wake <- ends [ - inds_to_remove ]
        
      }
      
      ## determine which sleep period is the longest
      per_ind <- which.max( as.numeric( night_dat$local_timestamp[ wake ] - night_dat$local_timestamp[ onset ], units = 'secs' ) )
      
      ## fill in the sleep period data frame with the sleep onset and waking time associated with the longest sleep period in the day (noon to noon)
      
      night_dat$sleep_per <- as.numeric( night_dat$local_timestamp >= night_dat$local_timestamp[ onset[ per_ind ] ] & night_dat$local_timestamp <= night_dat$local_timestamp[ wake[ per_ind ] ] )
      
    }else{ ## if there aren't any sleep bouts, record all rows as a 0 in the sleep_per column
      
      night_dat$sleep_per <- 0
      
    }
    
    # I was including the possibility of taking another rolling median here of the log VeDBA to determine the epoch-by-epoch sleep-wake classification. But instead I am going to do this with the raw log VeDBA (I was doing that before anyway, as waso_window as set to 1, so I wasn't actually taking a rolling median)
    # ## take the rolling median of the log VeDBA
    # night_dat$roll_log_vedba <- rollmedian( night_dat$log_vedba, waso_window, fill = NA, align = 'center' )
    # 
    
    if( sep_thresh ){
      
      night_dat$pot_sleep <- as.numeric( night_dat$log_vedba < unsmooth_thresh )
      
      ## find the run length encoding of periods above and below the threshold
      temp <- rle( as.numeric( night_dat$log_vedba < unsmooth_thresh ) ) 
      
      ## mark the rows that are part of runs (i.e. part of chunks that are greater than the block_size of either continuous activity or continuous inactivity )
      runs <- as.numeric( rep( temp$lengths >= waso_block, times = temp$lengths ) )
      
      ## mark the rows corresponding to sleep bouts. These sleep bouts are runs of inactivity
      sleep_bouts <- as.numeric( night_dat$log_vedba < unsmooth_thresh & runs == 1 )
      
    }else{
      
      night_dat$pot_sleep <- as.numeric( night_dat$log_vedba < thresh )
      
      ## find the run length encoding of periods above and below the threshold
      temp <- rle( as.numeric( night_dat$log_vedba < thresh ) ) 
      
      ## mark the rows that are part of runs (i.e. part of chunks that are greater than the block_size of either continuous activity or continuous inactivity )
      runs <- as.numeric( rep( temp$lengths >= waso_block, times = temp$lengths ) )
      
      ## mark the rows corresponding to sleep bouts. These sleep bouts are runs of inactivity
      sleep_bouts <- as.numeric( night_dat$log_vedba < thresh & runs == 1 )
      
    }
    
    ## make which rows are part of runs of inactivity. These are the periods of sleep within and outside of the sleep period
    night_dat$sleep_bouts <- sleep_bouts
    
    ### put the night data back into full_dat
    full_dat[ full_dat$tag == tag & full_dat$night_date == night, ] <- night_dat
  }
  
}


pre_clean_full <- full_dat

study_nights <- min( d1$night ):max( d1$night )

study_night_dates <- as.Date( min( d1$night_date ):max( d1$night_date ), origin = '1970-01-01' )


sleep_per <- data.frame( tag = rep( unique( d1$tag ), each = length( study_nights ) ), night = rep( study_nights, times = length( tag ) ), night_date = rep( study_night_dates, times = length( tag ) ), total_pot_sleep = NA, total_sleep_bouts = NA, onset = NA, waking = NA, SPT = NA, WASO = NA, TST = NA, sleep_eff = NA, wake_bouts = NA, frag_wake_bouts = NA, summed_VeDBA = NA, night_VeDBA_corr = NA, ave_vedba = NA, dark_pot_sleep = NA, dark_ave_vedba = NA, max_time_diff = NA, n_bursts= NA )


## create empty vectors for the durations of sleep and wake bouts. We will fill these in to see if the distributions of the durations of these bouts later
sleep_durs <- c()
wake_durs <- c() 


## for each individual...
for( tag in tag_names ){
  
  ## subset the data to just this individual's data
  id_dat <- full_dat[ full_dat$tag == tag, ]
  
  ## create a vector the nights for which this individual has data
  nights <- unique( id_dat$night_date )
  
  ## for each night on which this individual has data
  for( night in nights ){
    
    ## subset this individual's data to just that night
    night_dat <- id_dat[ id_dat$night_date == night, ]
    
    ## should already be in order, but just in case
    night_dat <- night_dat[ order( night_dat$local_timestamp ), ]
    
    sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$n_bursts <- unique( night_dat$n_bursts )
    
    sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$max_time_diff <- unique( night_dat$max_time_diff )
    
    sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$total_pot_sleep <- sum( night_dat$pot_sleep )
    
    sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$total_sleep_bouts <- sum( night_dat$sleep_bouts )
    
    sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$ave_vedba <- mean( night_dat$log_vedba )
    
    sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$dark_pot_sleep <- sum( night_dat$pot_sleep[ night_dat$local_time > dark_start | night_dat$local_time < dark_end ] )
    
    sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$dark_ave_vedba <- mean( night_dat$log_vedba[ night_dat$local_time > dark_start | night_dat$local_time < dark_end ] )
    
    
    
    SPT_dat <- night_dat[ night_dat$sleep_per == 1, ]
    
    if( nrow( SPT_dat ) > 0 ){
      
      onset <- min( SPT_dat$local_timestamp )
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$onset <- onset
      
      waking <- max( SPT_dat$local_timestamp ) 
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$waking <- waking
      
      SPT <- as.numeric( waking - onset, units = 'mins' ) + 1
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$SPT <- SPT
      
      WASO <- sum( SPT_dat$sleep_bouts == 0 )
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$WASO <- WASO
      
      TST <- sum( SPT_dat$sleep_bouts == 1 )
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$TST <- TST
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$sleep_eff <- TST/ nrow( SPT_dat )
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$summed_VeDBA <- sum( SPT_dat$log_vedba )
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$night_VeDBA_corr <- sum( SPT_dat$log_vedba ) / SPT
      
      temp <- rle( SPT_dat$sleep_bouts )
      
      runs <- as.numeric( rep( temp$lengths >= frag_block, times = temp$lengths ) )
      
      frag_wake_bouts <- as.numeric( SPT_dat$sleep_bouts == 0 & runs == 1 )
      
      diffs <- diff( c( 1, frag_wake_bouts ) )
      
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$frag_wake_bouts <- sum( diffs == 1 )
      
      ## find the distinct sleep bouts (i.e. epochs of sleep separated by waking)
      diffs <- diff( c( 0, SPT_dat$sleep_bouts ) )
      
      ## save the number of distinct wake bouts
      sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$wake_bouts <- sum( diffs == -1 )
      
      ## find durations of sleep and wake bouts
      temp <- rle( SPT_dat$sleep_bouts )
      
      ## add the duration of sleep bouts to the sleep bout duration vector
      sleep_durs <- c( sleep_durs, temp$lengths[ temp$values == 1 ] )
      ## add the duration of wake bouts to the wake bout duration vector
      wake_durs <- c( wake_durs, temp$lengths[ temp$values == 0 ] )
      
      
    }
  }
}


sum( !is.na( sleep_per$SPT ) )

### check number of nights for which sleep period was calculated and inspect those for which no sleep period was calculated ###

sleep_per_nona <- sleep_per[ !is.na( sleep_per$SPT ), ]

nrow( sleep_per_nona )

left_out <- unique( d1[ , c( 'tag', 'night' ) ] )[ !paste( unique( d1[ , c( 'tag', 'night' ) ] )$tag, unique( d1[ , c( 'tag', 'night' ) ] )$night ) %in% paste( sleep_per_nona$tag, sleep_per_nona$night ), ]


for( i in 1:nrow( left_out ) ){
  
  tag_night_dat <- d1[ d1$tag == left_out$tag[ i ] & d1$night == left_out$night[ i ], ]
  
  plot( tag_night_dat$local_timestamp, tag_night_dat$log_vedba )
  
} 

nrow( left_out )

tag_night_dat <- d1[ d1$tag == 2428 & d1$night == 5, ]

plot( tag_night_dat$local_timestamp, tag_night_dat$log_vedba )


############# Cleaning the dataframes of data on nights with insufficient data ################


# how many baboon-nights did we have ACC data for

nrow( unique( d1[ , c( 'tag','night' ) ] ) ) # 649

nrow( unique( full_dat[ , c( 'tag','night' ) ] ) ) # 646

sum( !is.na( sleep_per$dark_ave_vedba ) ) # 646



## remove all these variable from the night, and from the days on the early side of the noon-to-noon period if the noon-to-noon period is missing a lot of data (because then we might not be able to reliably calculate the sleep VeDBA threshold, and a lot of epochs might be missing, which would skew TST and such)
nrow( unique( full_dat[ full_dat$n_bursts < ( mins_in_day - missing_mins ), c( 'tag', 'night') ] ) ) # 74 baboon-nights removed from this cleaning step
sum( sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ) ) # confirmed 74 removed from this cleaning step


sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ), c( 'onset', 'waking', 'SPT', 'sleep_eff', 'TST', 'WASO', 'wake_bouts', 'summed_VeDBA', 'night_VeDBA_corr', 'ave_vedba', 'total_pot_sleep', 'total_sleep_bouts', 'dark_ave_vedba', 'dark_pot_sleep' ) ] <- NA

sleep_per_nona <- sleep_per[ !is.na( sleep_per$SPT ), ]

nrow( sleep_per_nona )

#### this next cleaning step below can be deleted because it doesn't actually remove anything 

## remove all these variable from the night, and from the days on the early side of the noon-to-noon period (only for those depending on SPT) if the noon-to-noon period has large gaps of missing data (because then we can't reliably calculate the SPT)
nrow( unique( full_dat[ full_dat$max_time_diff > time_gap, c( 'tag', 'night') ] ) ) # 0 baboon-nights removed from this cleaning step
sum( sleep_per$max_time_diff > time_gap & !is.na( sleep_per$max_time_diff ) ) # 0 baboon-nights removed from this cleaning step


sleep_per[ sleep_per$max_time_diff > time_gap & !is.na( sleep_per$max_time_diff ), c( 'onset', 'waking', 'SPT', 'sleep_eff', 'TST', 'WASO', 'wake_bouts', 'summed_VeDBA', 'ave_vedba', 'total_pot_sleep', 'total_sleep_bouts', 'dark_ave_vedba', 'dark_pot_sleep' ) ] <- NA

sleep_per_nona <- sleep_per[ !is.na( sleep_per$SPT ), ]

nrow( sleep_per_nona )

## remove data for sleep period and sleep bouts on days when there is a lot of missing data, because we cannot reliably calculate the sleep VeDBA threshold and there may be a lot of missing epochs
full_dat[ full_dat$n_bursts < ( mins_in_day - missing_mins ), c( 'sleep_per' ) ] <- NA

## remove data for sleep period on days when there are large gaps of missing data, because we can't reliably calculate the SPT with gaps in the data
full_dat[ full_dat$max_time_diff > time_gap, 'sleep_per'  ] <- NA



## how many baboon-nights do we have ACC data for after cleaning

sum( !is.na( sleep_per$total_sleep_bouts ) ) # 572 baboon-nights




## reformat sleep timestamp
sleep_per$onset <- as.POSIXct( sleep_per$onset, origin = "1970-01-01 00:00:00", tz = "UTC" )

## reformat waking timestamp
sleep_per$waking <- as.POSIXct( sleep_per$waking, origin = "1970-01-01 00:00:00", tz = "UTC" )

## make columns for just the time part of the sleep onset and waking timestamps
sleep_per$onset_time <- as_hms( sleep_per$onset )
sleep_per$waking_time <- as_hms( sleep_per$waking )

  


write.csv( full_dat, paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ), row.names = F )
write.csv( sleep_per, paste0( 'DATA/sleep_analysis_data/sleep_per_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ), row.names = F )

full_dat <- read.csv( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ) )
sleep_per <- read.csv( paste0( 'DATA/sleep_analysis_data/sleep_per_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ) )

full_dat$local_timestamp <- as.POSIXct( full_dat$local_timestamp, tz = 'UTC' )

###### accuracy of sleep classification #######

sec_focal_dat <- as.data.frame( fread( 'DATA/thermal_focal_follows/sec_focal_dat.csv' ) )

full_dat$corr_local_timestamp <- full_dat$local_timestamp - 16

labeled_full_dat <- merge( x = full_dat, y = sec_focal_dat, by.x = c( 'tag', 'corr_local_timestamp' ), by.y = c( 'tag', 'local_timestamp' ), all.x = T, all.y = F, sort = F )

labeled_full_dat$classified_sleep <- ifelse( labeled_full_dat$sleep_behavior == 'Unalert', 1, 0 )


valid_dat <- labeled_full_dat[ !is.na( labeled_full_dat$sleep_bouts ) & !is.na( labeled_full_dat$sleep_behavior ), ]

unique( valid_dat$tag )

length( unique( valid_dat$tag ) )
nrow( valid_dat )

confusion_matrix <- table( valid_dat$sleep_bouts, valid_dat$sleep_behavior )

accur <- ( confusion_matrix[ '0', 'Active' ] + confusion_matrix[ '0', 'Alert' ] + confusion_matrix[ '1', 'Unalert' ] ) / sum( confusion_matrix )

print( accur )

print( confusion_matrix )



################### Visualizing the sleep period and sleep/wake classification ########################## I think this has to be run after the stuff above

sleep_per_func <- function( tag, night, m_m = missing_mins, t_g = time_gap, m_w = mov_window, p_f_n_m = percentile_for_no_mult, b_s = block_size, g_s = gap_size, title = T, x_axis = T, plot_waso = T, w_b = waso_block, las = 1, ... ){
  
  ## save a variable denoting the total number of minutes in the day
  mins_in_day <- mins_in_day
  
  ## subset the data to the given tag on the given night
  night_dat <- full_dat[ full_dat$tag == tag & full_dat$night == night, ]
  
  ## sort the timestamps (they are probably already sorted)
  night_times <- as.numeric( as_hms( c( night_dat$local_time, '18:00:00', '06:30:00' ) ) )
  
  night_times[ night_times < 12*60*60 ] <- night_times[ night_times < 12*60*60 ] + 24*60*60
  
  sorted_times <- sort( night_times )
  
  ## find the time difference in seconds between each burst
  time_diffs <- as.numeric( diff(  sorted_times ) )
  
  ## if there is more than a single burst...
  if(length(time_diffs) != 0){
    
    ## if the number of bursts exceed the minimum required number of bursts in a night (determined by missing mins) and if the gaps in the data are within the allowable gap size (determined by time_gap)...
    if( nrow( night_dat ) > ( mins_in_day - missing_mins ) & max( time_diffs ) < time_gap ){
      
      ## take the rolling median of the log VeDBA and save it as a column
      night_dat$roll_log_vedba <- rollmedian( night_dat$log_vedba, mov_window, fill = NA, align = 'center' )
      
      ## determine the threshold activity vs. inactivity threshold based on the percentile and the rolling median just produced
      thresh <- quantile( night_dat$roll_log_vedba, percentile_for_no_mult, na.rm = T )
      
      ## put the rows of the dataframe in order from noon to noon (they should already be in this order, so this should be redundant)
      night_dat <- night_dat[ order( ts_func( night_dat$local_time ) ), ]
      
      ## turn the times into numerical elements for plotting
      ts_time <- ts_func( night_dat$local_time )
      
      if( title == F ){
        ## plot the log VeDBA
        #plot( ts_time, night_dat$log_vedba, type = 'l', xlab = 'Time', ylab = '', xaxt = 'n', las = las )
        
        plot( ts_time, night_dat$log_vedba, type = 'l', xlab = 'Time', ylab = '', xaxt = 'n', las = las, ylim = c( -2, 10 ), ... )
        
      }else{
        ## plot the log VeDBA
        plot( ts_time, night_dat$log_vedba, type = 'l', xlab = 'Time', ylab = '', main = paste( tag, night ), xaxt = 'n', las = las, ylim = c( -2, 10 ), ... )
        
        
      }
      
      if( x_axis == T ){
        
        axis( 1, at = seq( 0, 60*24*60, 60*60), labels = c( as_hms( seq( 12*60*60, 60*23*60, 60*60) ), as_hms( seq( 0, 60*12*60, 60*60) ) ) ) 
        
      }
      
      title( ylab = 'log VeDBA', line = 3.9 )
      ## plot the rolling median of the log VeDBA
      lines( ts_time, night_dat$roll_log_vedba, col = 'red')
      
      ## plot the threshold of the log VeDBA
      abline( h = thresh, col = 'blue' )
      
      ### find blocks of continuous inactivity
      
      ## find the run length encoding of periods above and below the threshold
      temp <- rle(as.numeric( night_dat$roll_log_vedba < thresh ) ) 
      
      ## mark the rows that are part of runs (i.e. part of chunks that are greater than the block_size of either continuous activity or continuous inactivity )
      night_dat$runs <- as.numeric( rep( temp$lengths > block_size, times = temp$lengths ) )
      
      ## mark the rows corresponding to sleep bouts. These sleep bouts are runs of inactivity
      night_dat$sleep_bouts <- as.numeric( night_dat$roll_log_vedba < thresh & night_dat$runs == 1 )
      
      ## find when sleep bouts start and end
      diffs <- diff( c(0, night_dat$sleep_bouts ) )
      starts <- which( diffs == 1 ) [ -1 ]
      ends <- which( diffs == -1 )
      
      ## if there are any sleep bouts...
      if( length( which( diffs == 1 ) ) != 0){
        
        ## find the duration of the gaps between each sleep bout (the end of one sleep bout and the start of the next)
        gaps <- as.numeric( night_dat$local_timestamp [ starts ] - night_dat$local_timestamp [ ends[ 1: length( starts ) ] ], units = 'mins' )
        
        ## sleep bouts separated by gaps that are shorter than that specified by gap_size will be merged. Note which of these gaps are shorter than the gap_size
        inds_to_remove <- which( gaps < gap_size ) 
        
        ## if there are NO gaps between sleep bouts that are to be removed...
        if( length( inds_to_remove ) == 0 ){
          
          ## set sleep onset index to be the start of sleep bouts
          onset <- which( diffs == 1 ) 
          
          ## set waking index to be the end of sleep bouts
          wake <- ends
          
        }else{ ## if there ARE gaps between sleep bouts that are to be removed...
          
          ## set sleep onset index to be the start of sleep bouts that do not correspond to the gaps to be removed (because these will be within sleep periods, not a start of a new bout)
          onset <- which( diffs == 1 ) [ - (inds_to_remove + 1) ]
          
          ## set waking index to be the end of sleep bouts that do not correspond to the gaps to be removed
          wake <- ends [ - inds_to_remove ]
          
        }
        
        ## determine which sleep period is the longest
        per_ind <- which.max( as.numeric( night_dat$local_timestamp[ wake ] - night_dat$local_timestamp[ onset ], units = 'secs' ) )
        
        ## plot the sleep onset time and waking time on the log VeDBA plot
        abline( v = c( ts_time[ onset[ per_ind ] ], ts_time[ wake[ per_ind ] ] ), col = 'orange', lty = 3, lwd = 4 )
        
        ## if you also want to plot WASO (this says WASO but it actually should plot all wake epochs)
        if( plot_waso == T ){

          ## calculate the threshold for sleeping and waking within the sleep period
          SPT_thresh <- quantile( night_dat$log_vedba, percentile_for_no_mult, na.rm = T)
          
          ## plot the threshold
          abline( h = SPT_thresh, col = 'blue', lty = 2, lwd = 2 )
          
          ## find blocks of continuous inactivity
          
          ## calcuate the run length encoding
          temp <- rle(as.numeric( night_dat$log_vedba < SPT_thresh ) ) 
          
          ## mark the runs of activity or inactivity
          night_dat$night_runs <- as.numeric( rep( temp$lengths >= waso_block, times = temp$lengths ) )
          
          ## mark the runs of inactivity as sleep bouts
          night_dat$night_sleep_bouts <- as.numeric( night_dat$log_vedba < SPT_thresh & night_dat$night_runs == 1 )
          
          ## find the starts and ends of waking bouts
          diffs <- diff( c(1, night_dat$night_sleep_bouts ) )
          
          starts <- which( diffs == -1 )
          
          ## add back in a "- 1" at the end of this line if you wish for the start and end times to be accurate. Now I just want to make it so the polygons show up even without a border
          ends <- which( diffs == 1 )
          
          ## if there are waking bouts
          if( length( which( diffs == -1 ) ) != 0){
            
            ## if the last waking bout never ends...
            if( length( starts ) != length( ends ) ){
              
              ## make it end at the end of the sleep period
              ends <- c(ends, length( diffs) )
              
            }
            
            ## save the start times and end times of waking bouts
            starts <- ts_func( night_dat$local_time[ starts ] )
            ends <- ts_func( night_dat$local_time[ ends ] )
            
            ## plot a polygon for each distinct waking bout
            for( n in 1:length( starts ) ){
              
              polygon( x = c(starts[ n ], ends[ n ], ends[ n ], starts[ n ], starts[ n ] ), y = c( 0, 0, 10, 10, 0), col = transp('blue', .25), border = NA )
              
            }
          }
        }
        
        lines( ts_time, night_dat$log_vedba, col = 'black')
        
        
        ## fill in the sleep period data frame with the sleep onset and waking time associated with the longest sleep period in the day (noon to noon)
        return( c( night_dat$local_timestamp[ onset[ per_ind ] ], night_dat$local_timestamp[ wake[ per_ind ] ] ) )
      } 
    }
  } 
}


for( i in 1:nrow( sleep_per ) ){
  
  sleep_per_func( tag = sleep_per$tag[ i ], night = sleep_per$night[ i ] )
  
  
}
