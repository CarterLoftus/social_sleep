

library( data.table )
library( lubridate )
library( plyr )
library( foreach )
library( doParallel)


input_data <- 'corrected' # I think this really only makes sense with 'corrected'

class_meth <- 'percentile_thresh'

sep_thresh <- T


# first read in tracklet data

identified_tracks_files <- list.files( path = paste0( "DATA/thermal_tracks/identified_tracks/", input_data ), full.names = T )

identified_tracks_list <- lapply( identified_tracks_files, FUN = readRDS )


# read in sleep data

if( class_meth != 'percentile_thresh' ){
  
  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '.csv' ) )
  
}else{
  
  full_dat <- fread( paste0( 'DATA/sleep_analysis_data/full_dat_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ) )
  
}

full_dat <- as.data.frame( full_dat )

full_dat$local_timestamp <- as.POSIXct( full_dat$local_timestamp, tz = 'UTC' )

# 
# meta_df <- read.csv( 'DATA/Collaring_data.csv' )
# 
# sleep_full <- merge( x = full_dat, y = meta_df[ , c( 'collar_id', 'sex', 'age_class_vet', 'age_class_akiko' ) ], by.x = 'tag', by.y = 'collar_id', all.x = T, all.y = F, sort = F )
# 


#### first merge sleep data into tracklet ####


sleep_tracks_list <- vector( 'list', length = length( identified_tracks_list ) )


sleep_tracks_list[[ i ]] <- foreach( i = 1:length( identified_tracks_list ) ){
  
#for( i in 1:length( identified_tracks_list ) ){
  
  identified_tracks <- identified_tracks_list[[ i ]]
  
  identified_tracks$final_tag <- ifelse( !is.na( identified_tracks$confirmed_tag ), identified_tracks$confirmed_tag, identified_tracks$tag )
  
  ## there may be a case where a confirmed tag is in conflict with automatically identified tag. In these cases, override the automatically identified tags with the confirmed track
  dup_inds <- sort( c( which( !is.na( identified_tracks$final_tag ) & duplicated( identified_tracks[ , c( 'local_timestamp', 'final_tag' ) ] ) ), which(  !is.na( identified_tracks$final_tag ) & duplicated( identified_tracks[ , c( 'local_timestamp', 'final_tag' ) ], fromLast = T ) ) ) )

  if( length( dup_inds ) != 0 ){
    
    identified_tracks[ dup_inds, 'final_tag' ][ is.na( identified_tracks[ dup_inds, 'confirmed_tag' ] ) ] <- NA
  } 
  
  if( sum(  !is.na( identified_tracks$final_tag ) & duplicated( identified_tracks[ , c( 'local_timestamp', 'final_tag' ) ] ) ) != 0 ) stop( 'yolo' )
  
  identified_tracks$local_timestamp <- as.POSIXct( identified_tracks$local_timestamp, tz = 'UTC' )
  
  identified_tracks$minute <- round_date( identified_tracks$local_timestamp, 'min' )
  
  sleep_tracks <- merge( x = identified_tracks, y = full_dat[ , c( 'tag', 'local_timestamp', 'log_vedba', 'sleep_bouts', 'pot_sleep' ) ], by.x = c( 'final_tag', 'minute' ), by.y = c( 'tag', 'local_timestamp' ), all.x = T, all.y = F, sort = F )
  
  sleep_tracks <- sleep_tracks[ order( sleep_tracks$local_timestamp ), ]
  sleep_tracks <- sleep_tracks[ order( sleep_tracks$id ), ]
  
  return( sleep_tracks )

}

saveRDS( sleep_tracks_list, 'DATA/thermal_tracks/final_tracks_list.rds' )

sleep_tracks_full <- ldply( sleep_tracks_list )

head( sleep_tracks_full )

head( sleep_tracks_full[ !is.na( sleep_tracks_full$sleep_bouts ), ] )

## check for duplicates
length( which( !is.na( sleep_tracks_full$final_tag) & duplicated( sleep_tracks_full[ , c( 'final_tag', 'local_timestamp' ) ] ) ) )

sum( duplicated( sleep_tracks_full[ , c( 'id', 'local_timestamp' ) ] ) ) 

#### then merge tracklet location data into the sleep ####

# another option than doing this would be just to remove duplicate tags/minutes in sleep_tracks_full
# I'll keep speed as well, although I don't know that it will be useful

full_spat_dat <- merge( x = full_dat, y = sleep_tracks_full[ , c( 'tag', 'local_timestamp', 'x_final', 'y_final', 'speed', 'id' ) ], by = c( 'tag', 'local_timestamp' ), all.x = T, all.y = F, sort = F  )

head( full_spat_dat )

write.csv( full_spat_dat, 'DATA/sleep_analysis_data/full_spat_dat.csv', row.names = F )

sum( duplicated( full_dat[ , c( 'tag', 'local_timestamp' ) ] ) )


sum( !is.na( full_spat_dat$x_final ) )/nrow( full_spat_dat )

sum( !is.na( full_spat_dat$x_final ) )

sum( full_spat_dat$local_timestamp %in% sleep_tracks_full$local_timestamp )


known_loc <- full_spat_dat[ !is.na( full_spat_dat$x_final ), ]

library( plotly )

p <- plot_ly( x = known_loc$local_timestamp,  y = as.numeric( as.factor( known_loc$tag ) ), type = 'scatter' )
p
