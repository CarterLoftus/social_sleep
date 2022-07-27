

library( lubridate ) 
library( data.table )
library( stringr )


#### Functions
## function for turning tag_names into colors
babcolor<-function(IDvec){
  
  IDvec <- as.character( IDvec )
  
  outputVec <- ifelse( IDvec %in% c( '6917', '6933', '6915', '6934', '6921', '6911' ), 'skyblue', ifelse( IDvec %in% c( '6927', '6932', '6910', '2454', '2451','2428' ), 'blue' , ifelse( IDvec %in% c( '6891', '6894', '2455' ), 'grey', ifelse( IDvec %in% c( '6898', '2448', '2436' ), 'yellow', ifelse( IDvec %in% c( '6900', '6892', '6890', '6903', '2447', '6914', '6924', '6908', '2441', '2450' ), 'pink', ifelse( IDvec %in% c( '6897', '2434', '2433' ), 'red', 'black' ) ) ) ) ) )
  
  return(outputVec)
}



dy_acc <- function(vect, win_size = 5){
  
  pad_size <- win_size/2 - 0.5
  
  padded <- unlist( c(rep(NA, pad_size), vect, rep(NA, pad_size)) )
  acc_vec <- rep(NA, length = length( vect ) )
  
  ## sliding window
  for(i in 1:length(vect)){
    win <- padded[i:(i+(2*pad_size))] ## subset the window
    m_ave <- mean( win, na.rm = T ) ## take the average over the window
    acc_comp <- vect[ i ] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i] <- acc_comp 
  }
  
  return( unlist( acc_vec) )
}


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




### this code calculates the log VeDBA for the bursts that we created and uniformized in the code "CODE/sleep_analysis/00a_trimming_acc_data_2019.ipynb". We want to calculate the log VeDBA for both the bursts that are closest to the minute (to uniformize the burst sampling schedule to an every minute on the minute schedule for the whole day and night, as this is the schedule of the most limiting data (the 2000s night-time bursts))

d1 <- fread("DATA/sleep_analysis_data/all_burst_acc.csv")

head( d1 )

d1 <- as.data.frame(d1)

## remove tags that are on individuals that are not in the cliff group
meta_dat <- read.csv( "DATA/Collaring_data.csv" )

head( meta_dat )

unique( d1$tag[ ! d1$tag %in% meta_dat$collar_id[ meta_dat$Group == 'Cliffs' ] ] )


d1 <- d1[ d1$tag %in% meta_dat$collar_id[ meta_dat$Group == 'Cliffs' ], ]

d1$timestamp <- as.POSIXct( d1$timestamp, tz = 'UTC' )

## first determine the inds that we want to keep because they are the closest to the minute
d1$min_timestamp <- round_date( d1$timestamp, unit = 'min' )

d1$time_from_min <- as.numeric( abs( d1$timestamp - d1$min_timestamp ), units = 'secs' )

agg <- aggregate( d1$time_from_min, by = list( d1$tag, d1$min_timestamp ), FUN = min )

names( agg ) <- c( 'tag', 'min_timestamp', 'min_time_from_min' )

set_to_keep <- paste( agg$tag, agg$min_timestamp, agg$min_time_from_min, sep = '_' )

full_set <- paste( d1$tag, d1$min_timestamp, d1$time_from_min, sep = '_' )

d1_trim <- d1[ full_set %in% set_to_keep, ]


# only keep the necessary columns 
d1_trim <- d1_trim[ , c( 'tag', 'timestamp', 'acc_interp' ) ]

#rename the columns
names(d1_trim) <- c('tag','timestamp','eobs_accelerations_raw' )

# Plot the the times when we have data for each collar 

plot(as.numeric(as.factor(d1_trim$tag))~d1_trim$timestamp,cex=0.3,pch=16, main="Acceleromter bursts", xlab="", xaxt='n',yaxt='n',ylab="ID",col=babcolor(d1_trim$tag))
axis(2,at=1:length(unique(d1_trim$tag)),labels=sort(unique(d1_trim$tag)),las=1,cex=0.3)
axis.POSIXct(1,at=seq(min(d1_trim$timestamp),max(d1_trim$timestamp),by="1 day"),las=2)

d1_trim$night <- as.Date( d1_trim$timestamp - 12*60*60) 

tag_names <- as.character( unique( d1_trim$tag ) )

for( tag in tag_names ){
  sub_d1_trim <- d1_trim[ d1_trim$tag == tag & d1_trim$night == as.Date( '2019-07-19'), ]
  print( nrow(sub_d1_trim) )
}



d1_trim$time <- str_split_fixed(d1_trim$timestamp, " ", 2)[,2]

d2 <- as.data.frame(str_split(d1_trim$eobs_accelerations_raw, " ", simplify = T))

for(i in 1:ncol(d2)){
  d2[,i] <- as.numeric(as.character((d2[,i])))
}

names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

d2$timestamp <- d1_trim$timestamp
d2$tag <- d1_trim$tag

d2[!complete.cases(d2),]

inds <- complete.cases(d2)
d1_trim <- d1_trim[ inds ,]
d2 <- d2[ inds ,]

names(d2) <- c( paste( rep(c("x","y","z"), ncol(d2)/3), rep(1:(ncol(d2)/3), each = 3), sep = ''), 'timestamp')

x_d <- d2[,grepl('x',names(d2))]
head(x_d)

y_d <- d2[,grepl('y',names(d2))]
head(y_d)

z_d <- d2[,grepl('z',names(d2))]
head(z_d)

d1_trim$ave_vedba <- apply( sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2) , 2, FUN = mean )

d1_trim$log_vedba <- log( d1_trim$ave_vedba )

write.csv(d1_trim, "DATA/sleep_analysis_data/processed_ACC_emom.csv", row.names = F)







