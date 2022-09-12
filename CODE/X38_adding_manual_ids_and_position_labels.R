
library(tidyverse)
library(caret)
library(xgboost)
library( data.table )
library( stringr )
library( plyr )
library( raster )

### manual track ID edits

transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}

input_data <- 'corrected' ## should be 'corrected'. This script only really makes sense to run with the corrected tracks. It will be pointless with the uncorrected tracks


if( str_split( getwd(), '/', simplify = T )[ 1, 3 ] == 'jcl273' ){
  
  manual_ident <- fread( "DATA\\id_and_time_sync_in_thermal\\final_id_and_focal.csv" )
  
}else{
  
  manual_ident <- fread( "Z:\\baboon\\working\\video\\thermal\\thermal_baboon\\DATA\\id_and_time_sync_in_thermal\\final_id_and_focal.csv" )
  
}

manual_ident <- as.data.frame( manual_ident )

## NA in focal column means that they could be focaled and haven't been yet. 0 means there is no point in focaling because they do not have ACC

manual_ident$local_start_time <- as.POSIXct( paste( manual_ident$date, manual_ident$start_time ), tz = 'UTC' ) + 3*60*60

manual_ident$local_end_time <- as.POSIXct( ifelse( manual_ident$end_time > "12:00:00", as.POSIXct( paste( manual_ident$date, manual_ident$end_time ), tz = 'UTC' ) + 3*60*60, as.POSIXct( paste( as.character( as.Date( manual_ident$date + 1 ) ), manual_ident$end_time ), tz = 'UTC' ) + 3*60*60 ), origin = '1970-01-01 00:00:00', tz = 'UTC' )

head( manual_ident )

files <- list.files( path = paste0( "DATA/thermal_tracks/identified_tracks/", input_data ) )

nights <- str_split_fixed( files, "_", 2 )[, 1 ]

final_tracks <- vector( 'list', length = length( nights ) )

for( ni in 1:length( nights ) ){
  
  night <- nights[ ni ]
  
  pre_id_tracks <- readRDS( paste0( "DATA/thermal_tracks/identified_tracks/", input_data, '/', night, "_ident_tracks.rds" ) )
  
  pre_id_tracks$local_timestamp <- as.POSIXct( pre_id_tracks$local_timestamp, tz = 'UTC' )
  
  pre_id_tracks$confirmed_tag <- NA
  
  # subset the identification data to just this night
  id_sub <- manual_ident[ gsub( "-", "", manual_ident$date ) == nights[ ni ], ]

  if( sum( !is.na( id_sub$track ) ) > 0 & sum( id_sub$track != "" ) > 0 ){
    
    night_date <- as.Date( night, format = '%Y%m%d' )
    
    id_sub$full_id <- paste( strftime( night_date, format = '%m%d' ), id_sub$track, sep = '_' )
    
    
    # go through row by row and add the manually confirmed identities into the dataframe
    for( i in 1:nrow( id_sub ) ){
      
      pre_id_tracks[ pre_id_tracks$id == id_sub$full_id[ i ] & pre_id_tracks$local_timestamp >= id_sub$local_start_time[ i ] & pre_id_tracks$local_timestamp < id_sub$local_end_time[ i ], 'confirmed_tag' ] <- id_sub$tag[ i ] 
      
    }
  }
  
  final_tracks[[ ni ]] <- pre_id_tracks
  
}


full_tracks <- ldply( final_tracks )

mean( full_tracks$confirmed_tag == full_tracks$tag, na.rm = T )

nrow( full_tracks[ which( full_tracks$confirmed_tag == full_tracks$tag ) , ] )

sum( !is.na( full_tracks$confirmed_tag ) & !is.na( full_tracks$tag ) )

unique( full_tracks$tag )

unique( full_tracks$confirmed_tag )


#View( full_tracks[ !is.na( full_tracks$tag ) & !is.na( full_tracks$confirmed_tag ), ] )


unique( full_tracks[ , c( 'tag', 'confirmed_tag' ) ] )

### this comes from CODE/00_prepping_therm_focals.R script
sec_focal_dat <- read.csv( "DATA/thermal_focal_follows/sec_focal_dat.csv" )

sec_focal_dat$local_timestamp <- as.POSIXct( sec_focal_dat$local_timestamp, tz = 'UTC' )

sec_focal_dat[ which( sec_focal_dat$tag == 2454 & sec_focal_dat$position == 'Lie_down' ), ]


full_tracks_labeled <- merge( x = full_tracks, y = sec_focal_dat, by.x = c( 'confirmed_tag', 'local_timestamp' ), by.y = c( 'tag', 'local_timestamp' ), all.x = T, all.y = F, sort = F )


full_tracks_labeled[ which( full_tracks_labeled$confirmed_tag == 2454 ), ]



sec_focal_dat[ which( sec_focal_dat$tag == 2454 ), ]

unique( sec_focal_dat$position )

full_tracks_labeled$stat_nonstat <- as.factor( ifelse( full_tracks_labeled$position %in% c( 'Sit', 'Stand', 'Lie_Down', 'Bipedal_stand', 'Crouch' ), 'stationary', ifelse( full_tracks_labeled$position %in% c( 'Walk', 'Climb', 'Run' ), 'non_stationary', NA )  ) )

# how will did our simple speed threshold do?
mean( ifelse( full_tracks_labeled$stat_nonstat == 'stationary', 0, 1 ) == as.numeric( full_tracks_labeled$speed > 0.25 ), na.rm = T ) # wow! Quite well!


full_tracks_for_ml_full <- full_tracks_labeled[ !is.na( full_tracks_labeled$stat_nonstat ) & !is.na( full_tracks_labeled$speed ), ]

full_tracks_for_ml <- full_tracks_for_ml_full[ , c( 'speed', 'stat_nonstat' ) ]

plot( full_tracks_for_ml_full[ , c( 'x_final', 'y_final' ) ] )


nrow( full_tracks_for_ml_full )

unique( full_tracks_for_ml_full$id )

unique( full_tracks_for_ml_full$confirmed_tag )


## trim the training data to only things that are on the cliff

data_to_plot <- full_tracks[ full_tracks$night == '2019-08-06', ]
data_to_plot <- data_to_plot[ seq( 1, nrow( data_to_plot ), by = 10 ), ]
plot( data_to_plot$x_final, data_to_plot$y_final )

#locator()

#the following limits of what is considered 'on the cliff' were found with the locator() function
training_y_limit_low <- 5
training_y_limit_high <- 20

training.samples <- full_tracks_for_ml$stat_nonstat %>%
  createDataPartition(p = 0.8, list = FALSE)
train_dat  <- full_tracks_for_ml_full[training.samples, ]
test_dat <- full_tracks_for_ml[-training.samples, ]

train_dat_lim <- train_dat[ train_dat$y_final > training_y_limit_low & train_dat$y_final < training_y_limit_high, c( 'speed', 'stat_nonstat' )]


#Fit the model on the training set
set.seed(123)
nonstat_model <- train(
  stat_nonstat ~., data = train_dat_lim, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
nonstat_model$bestTune

predicted.classes <- nonstat_model %>% predict( test_dat )
head(predicted.classes)

mean( predicted.classes == test_dat$stat_nonstat )

confusionMatrix( predicted.classes, test_dat$stat_nonstat )

confusionMatrix( as.factor( ifelse( full_tracks_labeled$speed > 0.25, 'non_stationary', 'stationary' ) ), full_tracks_labeled$stat_nonstat )

saveRDS( nonstat_model, 'DATA/thermal_tracks/stat_nonstat_ml_model.rds' )

nonstat_model <- readRDS( 'DATA/thermal_tracks/stat_nonstat_ml_model.rds' )

#### now use the model to add stationary vs. non-stationary information to the identified tracks (and also add the confirmed identities to the identified tracks)


## make sure you have run the uncommented script above if running for corrected tracks

nonstat_model <- readRDS( 'DATA/thermal_tracks/stat_nonstat_ml_model.rds' )


input_data <- 'corrected' ## 'corrected' or 'uncorrected'


if( input_data == 'corrected' ){
  
  final_tracks_list <- final_tracks
  
}else{

  if( input_data != 'uncorrected' ) stop( 'input_data must either be set to corrected or uncorrected' )
  
  files_to_read <- list.files( path = paste0( "DATA/thermal_tracks/identified_tracks/", input_data ), full.names = T )
  
  final_tracks_list <- lapply( files_to_read, readRDS )
}
  

for( final_track in final_tracks_list ){
  
  night <- as.character( unique( final_track$night ) )
  
  final_track$moving_predicted[ !is.na( final_track$speed ) ] <- as.character( nonstat_model %>% predict( final_track ) )
  
  saveRDS( final_track, paste0( "DATA/thermal_tracks/identified_tracks/", input_data, '/', gsub( '-', '', night ) , "_ident_tracks.rds" ) )

}


plot( final_track$speed, col = as.numeric( as.factor( final_track$moving_predicted ) ) )

abline( h = 0.25 )


