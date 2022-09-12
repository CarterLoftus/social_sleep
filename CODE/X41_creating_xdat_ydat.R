


library( data.table )
library( stringr )


#### this part needs to happen after running tracklet identification and merging of tracklet locations into full_dat

full_dat <- fread( 'DATA/sleep_analysis_data/full_spat_dat.csv' )

full_dat <- as.data.frame( full_dat )

full_dat$local_timestamp <- as.POSIXct( as.character( full_dat$local_timestamp ), tz = 'UTC' )

tag_names <- sort( as.character( unique( full_dat$tag ) ) )

timestamps <- seq( min( full_dat$local_timestamp) , max( full_dat$local_timestamp), by = '1 min' )


x_dat <- as.data.frame( matrix( NA, nrow = length( timestamps ), ncol = 1 + length( tag_names ) ) )

names( x_dat ) <- c( 'local_timestamp', tag_names )

x_dat$local_timestamp <- timestamps

x_dat$local_time <- str_split_fixed( x_dat$local_timestamp, ' ', 2 )[ , 2 ]

x_dat$night <- as.Date( x_dat$local_timestamp - 12*60*60 )

y_dat <- x_dat 

for( tag in tag_names ){
  
  id_dat <- full_dat[ full_dat$tag == tag, ]
  
  nights <- as.character( unique( id_dat$night_date ) )
  
  for( night in nights ){
    
    night_dat <- id_dat[ id_dat$night_date == night, ]
    
    x_dat[ match( night_dat$local_timestamp, x_dat$local_timestamp ), tag ] <- night_dat$x_final
    
    y_dat[ match( night_dat$local_timestamp, y_dat$local_timestamp ), tag ] <- night_dat$y_final
    
  }
}

x_dat <- x_dat[ x_dat$local_time > "18:00:00" | x_dat$local_time < "06:30:00", ]

y_dat <- y_dat[ y_dat$local_time > "18:00:00" | y_dat$local_time < "06:30:00", ]

saveRDS( x_dat, 'DATA/sleep_analysis_data/x_dat.rds' )

saveRDS( y_dat, 'DATA/sleep_analysis_data/y_dat.rds' )


x_dat[ x_dat$night == '2019-08-06' &  x_dat$local_time > "20:00:00" , ]

x_dat[ which( x_dat$`6934` != 0) ,]


