


library( data.table )
library( stringr )

################## Read in the full_dat (accelerometer burst) data ###################

class_meth <- 'percentile_thresh'

sep_thresh <- T

sleep_column <- 'sleep_bouts' 

if( class_meth != 'percentile_thresh' ){
  
  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '.csv' ) )
  
}else{
  
  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ) )
  
}

full_dat <- as.data.frame( full_dat )

full_dat$local_timestamp <- as.POSIXct( as.character( full_dat$local_timestamp ), tz = 'UTC' )

tag_names <- sort( as.character( unique( full_dat$tag ) ) )

timestamps <- seq( min( full_dat$local_timestamp) , max( full_dat$local_timestamp), by = '1 min' )


total_dat <- as.data.frame( matrix( NA, nrow = length( timestamps ), ncol = 1 + length( tag_names ) ) )

names( total_dat ) <- c( 'local_timestamp', tag_names )

total_dat$local_timestamp <- timestamps

total_dat$local_time <- str_split_fixed( total_dat$local_timestamp, ' ', 2 )[ , 2 ]

total_dat$night <- as.Date( total_dat$local_timestamp - 12*60*60 )

for( tag in tag_names ){
  
  id_dat <- full_dat[ full_dat$tag == tag, ]
  
  nights <- as.character( unique( id_dat$night_date ) )
  
  for( night in nights ){
    
    night_dat <- id_dat[ id_dat$night_date == night, ]
    
    night_dat$awake <- as.numeric( !( night_dat[ , sleep_column ] == 1 ) )
    
    total_dat[ match( night_dat$local_timestamp, total_dat$local_timestamp ), tag ] <- night_dat$awake
    
  }
}

total_dat <- total_dat[ total_dat$local_time > "18:00:00" | total_dat$local_time < "06:30:00", ]

saveRDS( total_dat, 'DATA/sleep_analysis_data/total_dat.rds' )


##### calculating the mean and SE of number of times of waking each night ########

class_meth <- 'percentile_thresh'

sep_thresh <- T

sleep_column <- 'sleep_bouts'

if( class_meth != 'percentile_thresh' ){
  
  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '.csv' ) )
  
}else{
  
  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ) )
  
}

full_dat <- as.data.frame( full_dat )

full_dat$local_timestamp <- as.POSIXct( as.character( full_dat$local_timestamp ), tz = 'UTC' )

full_dat$sleep_bouts[ is.na( full_dat$sleep_per ) ] <- NA

tag_names <- sort( as.character( unique( full_dat$tag ) ) )

nights <- sort( as.character( unique( full_dat$night_date ) ) )

night_start <- '21:00:00'
night_end <- '05:00:00'

wake_vec <- c()

for( tag in tag_names ){
  
  tag_dat <- full_dat[ full_dat$tag == tag, ]
  
  for( night in nights ){
    
    if( night != '2019-07-18' ){
      
      night_dat <- tag_dat[ tag_dat$night_date == night, ]
      
      if( sum( !is.na( night_dat$sleep_bouts ) != 0 ) ){
        
        max_ind <- max( which( !is.na( night_dat$sleep_bouts ) ) )
        
        max_time <- night_dat[ max_ind, 'local_time' ]
        
        
        if( max_time < night_end | max_time > night_start ){
          
          next
          
          
        }else{
          
          night_dat$waking <- diff( c( 1, night_dat$sleep_bouts ) ) 
          
          night_dat_trim <- night_dat[ night_dat$local_time > night_start | night_dat$local_time < night_end, ]
          
          if( nrow( night_dat_trim ) == 0 ){
            
            next 
            
          }
          
          wake_vec <- c( wake_vec, sum( night_dat_trim$waking == 1 ) )
          
        }
        
      }
      
    }
    
  }
  
}

wake_vec

mean( wake_vec )    

sd( wake_vec ) / sqrt( length( wake_vec ) )

