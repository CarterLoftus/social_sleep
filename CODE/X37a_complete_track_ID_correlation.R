



options(digits.secs = 6)
library(data.table)
library(lubridate)
library(stringr)
library(hms)
library(ascii)
library(plyr)
library(tidyr)
library(DescTools)
library(foreach)
library(doParallel)
library(ggplot2)
library(irr)
library(zoo)
library(dplyr)



######### functions #########
## this function standardizes a vector
stdize <- function(x) {
  return((x - mean(x, na.rm = T)) / sd(x, na.rm = T))
  
}

## this function takes the sum of a vector ignoring NAs, but returns an NA if the vector only contains NAs
na.sum <- function(x) {
  if (sum(!is.na(x)) == 0) {
    return(NA)
    
  } else{
    return(sum(x, na.rm = T))
  }
}

cor_function <- function(x, y) {
  if (sum(!is.na(x)) > 2 &
      sum(!is.na(y)) > 2 & sum(complete.cases(x, y)) > 3) {
    return(cor.test(x, y)$estimate)
    
  } else{
    return(NA)
    
  }
}



dy_acc <- function(vect, win_size = 5) {
  pad_size <- win_size / 2 - 0.5
  
  padded <- unlist(c(rep(NA, pad_size), vect, rep(NA, pad_size)))
  acc_vec <- rep(NA, length = length(vect))
  
  ## sliding window
  for (i in 1:length(vect)) {
    win <- padded[i:(i + (2 * pad_size))] ## subset the window
    m_ave <- mean(win) ## take the average over the window
    acc_comp <-
      vect[i] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i] <- acc_comp
  }
  
  return(unlist(acc_vec))
}

dy_acc_cont <- function(vect, win_size = win_size_cont) {
  pad_size <- win_size / 2 - 0.5
  
  acc_vec <- rep(NA, (length(vect) - 2 * pad_size))
  
  ## sliding window
  for (i in (pad_size + 1):(length(vect) -  pad_size)) {
    win <-
      vect[(i - pad_size):(i + pad_size)] ## subset the window
    m_ave <- mean(win) ## take the average over the window
    acc_comp <-
      vect[i] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i - pad_size] <- acc_comp
  }
  
  return(unlist(acc_vec))
}


## this function takes the rolling median but pads the ends with NAs so that the output vector is the same length as the input vector
pad_roll_med <- function(x, k) {
  library(zoo)
  
  return(c(rep(NA, floor(k / 2)), rollmedian(x, k, na.rm = T), rep(NA, floor(k /
                                                                               2))))
  
}

## this function standardizes a vector
stdize <- function(x) {
  return((x - mean(x, na.rm = T)) / sd(x, na.rm = T))
  
}
##############


input_data <- 'corrected' ## can be 'corrected' or 'uncorrected'
should_smooth_tracklets <- F
acc_informed <- F



#### read in the smoothed speeds from the GPS ####
files <-
  list.files(path = paste0("DATA/thermal_tracks/smooth_tracks/", input_data))
nights <- str_split_fixed(files, "_", 2)[, 1]
speeds_smoothed <- fread("DATA/gps_acc_data/speeds_smoothed.csv")
speeds_smoothed <- as.data.frame(speeds_smoothed)
speeds_smoothed$corr_local_timestamp <-
  as.POSIXct(speeds_smoothed$corr_local_timestamp, tz = 'UTC')

#### read in the log VeDBA data for the continuous ACC ####
cont_vedba_smoothed <-
  fread("DATA/gps_acc_data/cont_vedba_smoothed.csv")
cont_vedba_smoothed <- as.data.frame(cont_vedba_smoothed)
cont_vedba_smoothed$corr_local_timestamp <-
  as.POSIXct(cont_vedba_smoothed$corr_local_timestamp, tz = 'UTC')

#### read in the log VeDBA data from the ACC bursts ####
burst_vedba <- fread("DATA/gps_acc_data/burst_vedba.csv")
burst_vedba <- as.data.frame(burst_vedba)
burst_vedba$corr_local_timestamp <-
  as.POSIXct(burst_vedba$corr_local_timestamp, tz = 'UTC')

#### read in the log VeDBA data from the ACC bursts that has the 2nd of each burst for the 6000s collars removed, to make them more comparable to the 2000s collars ####
burst_vedba_one_sec <-
  fread("DATA/gps_acc_data/burst_vedba_one_sec.csv")
burst_vedba_one_sec <- as.data.frame(burst_vedba_one_sec)
burst_vedba_one_sec$corr_local_timestamp <-
  as.POSIXct(burst_vedba_one_sec$corr_local_timestamp, tz = 'UTC')

#### load trackelts ####
options(warn = 0)

dir.create(paste(getwd(), "DATA/thermal_tracks/identified_tracks", sep = '/'))
dir.create(paste(
  getwd(),
  "DATA/thermal_tracks/identified_tracks",
  input_data,
  sep = '/'
))

detectCores()

stopImplicitCluster()


if (str_split(getwd(), "/", simplify = T)[1, 3] == 'meerkat') {
  registerDoParallel(length(nights))
  
} else{
  registerDoParallel(2)
  
}

#### main loop ####

#identified_tracks <- foreach( ni = 1:length( nights ) ) %dopar% {
ni <- 5
for (ni in 1:length(nights)) {
  night_of_int <- nights[ni]
  
  print(night_of_int)

  smooth_tracks <-
    readRDS(
      paste0(
        "DATA/thermal_tracks/smooth_tracks/",
        input_data,
        '/',
        night_of_int,
        "_smooth_tracks.rds"
      )
    )
  
  min_time <- min(smooth_tracks$local_timestamp)
  max_time <- max(smooth_tracks$local_timestamp)
  
  all_times <- seq(min_time, max_time, by = '1 sec')
  
  ####### trim the GPS and ACC data ###########
  
  #### trim the GPS speed data to only the time when the thermal tracklets are present ####
  trim_gps_speeds_smooth <-
    merge(
      x = data.frame(corr_local_timestamp = all_times),
      y = speeds_smoothed,
      by = 'corr_local_timestamp',
      all.x = T,
      all.y = F,
      sort = F
    )
  trim_gps_speeds_smooth <-
    trim_gps_speeds_smooth[order(trim_gps_speeds_smooth$corr_local_timestamp),]
  
  #### trim the continuous log VeDBA data to only the time when the thermal tracklets are present ####
  trim_vedba_smooth <-
    merge(
      x = data.frame(corr_local_timestamp = all_times),
      y = cont_vedba_smoothed,
      by = 'corr_local_timestamp',
      all.x = T,
      all.y = F,
      sort = F
    )
  trim_vedba_smooth <-
    trim_vedba_smooth[order(trim_vedba_smooth$corr_local_timestamp),]
  
  identical(
    trim_gps_speeds_smooth$corr_local_timestamp,
    trim_vedba_smooth$corr_local_timestamp
  )
  
  
  ## now make the GPS acc-informed
  def_not_moving_thresh_2000s <- 3.3
  
  if (acc_informed) {
    gps_not_in_ved <-
      trim_gps_speeds_smooth[,!names(trim_gps_speeds_smooth) %in% names(trim_vedba_smooth)]
    gps_not_in_ved <-
      apply(
        gps_not_in_ved,
        c(1, 2),
        FUN = function(x)
          x * NA
      )
    acc_inform_dat <-
      cbind(trim_vedba_smooth[, names(trim_vedba_smooth) != 'corr_local_timestamp'], gps_not_in_ved)
    temp_gps <-
      trim_gps_speeds_smooth[, names(trim_gps_speeds_smooth) != 'corr_local_timestamp']
    
    # make sure columns are in the same order as temp_gps
    acc_inform_dat <- acc_inform_dat[, names(temp_gps)]
    stationary_inds <-
      which(acc_inform_dat < def_not_moving_thresh_2000s, arr.ind = T)
    temp_gps[stationary_inds] <- 0
    trim_gps_speeds_smooth[, names(trim_gps_speeds_smooth) != 'corr_local_timestamp'] <-
      temp_gps
    
  }
  
  
  ### trim the burst log VeDBA data (both the full bursts, as well as the bursts that only have the first second of the burst represented (so that the 2000s and 6000s collars are comparable) ) to the times when the thermal tracklets are present
  trim_burst_vedba <-
    merge(
      x = data.frame(corr_local_timestamp = all_times),
      y = burst_vedba,
      by = 'corr_local_timestamp',
      all.x = T,
      all.y = F,
      sort = F
    )
  trim_burst_vedba <-
    trim_burst_vedba[order(trim_burst_vedba$corr_local_timestamp),]
  
  trim_burst_vedba_one_sec <-
    merge(
      x = data.frame(corr_local_timestamp = all_times),
      y = burst_vedba_one_sec,
      by = 'corr_local_timestamp',
      all.x = T,
      all.y = F,
      sort = F
    )
  trim_burst_vedba_one_sec <-
    trim_burst_vedba_one_sec[order(trim_burst_vedba_one_sec$corr_local_timestamp),]
  
  ###### smoooth and trim the tracklet speeds #########
  
  ## put the speeds of the tracklets into wide format as well
  tracklet_speeds_temp <-
    reshape2::dcast(smooth_tracks, local_timestamp ~ id, value.var = 'speed')
  tracklet_veds_temp <-
    reshape2::dcast(smooth_tracks, local_timestamp ~ id, value.var = 'ved')
  
  #### smooth the tracklet speeds, if we input above that smoothing is desirable ####
  if (should_smooth_tracklets) {
    tracklet_speeds_unsmooth <-
      merge(
        x = data.frame(local_timestamp = all_times),
        y = tracklet_speeds_temp,
        by = 'local_timestamp',
        all.x = T,
        all.y = T,
        sort = F
      )
    
    tracklet_speeds_unsmooth <-
      tracklet_speeds_unsmooth[order(tracklet_speeds_unsmooth$local_timestamp) ,]
    
    smoothing_window <- 5
    
    tracklet_speeds <-
      apply(
        tracklet_speeds_unsmooth[, names(tracklet_speeds_unsmooth) != 'local_timestamp'],
        MARGIN = 2,
        FUN = pad_roll_med,
        k = smoothing_window
      )
    
    tracklet_speeds <- as.data.frame(tracklet_speeds)
    
    tracklet_speeds$local_timestamp <-
      tracklet_speeds_unsmooth$local_timestamp
    
  } else{
    tracklet_speeds <-
      merge(
        x = data.frame(local_timestamp = all_times),
        y = tracklet_speeds_temp,
        by = 'local_timestamp',
        all.x = T,
        all.y = T,
        sort = F
      )
    
    tracklet_speeds <-
      tracklet_speeds[order(tracklet_speeds$local_timestamp) ,]
    
  }
  

  track_ids <- unique(smooth_tracks$id)
  track_times <- unique(smooth_tracks$local_timestamp)
  
  
  
  
  ##### identification pipeline #####
  
  #### VEDBA ####
  # check matching of time series duplicate of following rows
  
  window_size <- 60 * 60 * 12
  step_size <- 60 * 60 * 12
  start_time <- min(trim_vedba_smooth$corr_local_timestamp)
  end_time <- max(trim_vedba_smooth$corr_local_timestamp)
  
  # window_start_times <-
  #   seq(from = start_time,
  #       to = (end_time - window_size + 1),
  #       by = step_size)
  
  window_start_times <-
    start_time
  
  # window_end_times <-
  #   seq(
  #     from = (start_time + window_size - 1),
  #     to = end_time,
  #     by = step_size)
    
  window_end_times <-
    end_time
  
  tags <-
    names(trim_vedba_smooth)[names(trim_vedba_smooth) != 'corr_local_timestamp']
  
  identity_scores_cont_ved <-
    data.frame(
      tag = rep(tags, each = length(window_start_times)),
      window_start_time = rep(window_start_times, times = length(tags)),
      candidate_tracklet = NA,
      score = NA,
      tracklet_moved = NA
    )
  
  speed_thresh <-
    0.2 # this is the speed in meters per second that a baboon needs to exceed to be considered moving
  
  tracklet_lengths <- smooth_tracks %>%
    group_by(id) %>%
    summarize(val = length(!is.na(speed)))
  
  # tracklet_lengths <- apply( tracklet_window_dat, MARGIN = 2, FUN = function( x ) sum( !is.na( x )  ) )
  names_list <- tracklet_lengths$id[tracklet_lengths$val > 0]
  colors <- rainbow(length(track_ids))
  
  win_size_cont <- 6
  min_speed_thres <- 0
  max_speed_thres <- 2.5
  move_speed_thres <- 0.3
  
  tag_vedba_thres <- 1.3
  pad_size <- win_size_cont / 2 - 0.5
  tag <- tags[3]
  plot_list <- list()
  
  tag <- tags[1]
  for (tag in tags) {
    plot_list <- list()
    print(tag)
    #trim_burst_vedba_one_sec
    # merge trim_burst_vedba_one_sec AND trim_vedba_smooth
    
    #input_tag_data[is.na(input_tag_data)] <- trim_burst_vedba_one_sec[,tag][is.na(input_tag_data)]
    
    tag_dat <- trim_vedba_smooth[, tag]
    tag_dat[is.na(tag_dat)] <- trim_burst_vedba_one_sec[,tag][is.na(tag_dat)]
    
    times_tag_all <- trim_vedba_smooth$corr_local_timestamp
    
    for (i in 1:length(window_start_times)) { 
      
      tag_window_dat <-
        tag_dat[times_tag_all >= window_start_times[i] &
                  times_tag_all <= window_end_times[i]]
      
      times_tag <-
        times_tag_all[times_tag_all >= window_start_times[i] &
                    times_tag_all <= window_end_times[i]]
      if (sum(!is.na(tag_window_dat)) > 0) {
        # sc <- scale(tag_window_dat,center = FALSE, scale = max(tag_window_dat,na.rm = T) - min(tag_window_dat,na.rm = T))
        
        tag_scaled <- log(tag_window_dat) > tag_vedba_thres
        # tag_scaled <- (tag_window_dat - min(tag_window_dat,na.rm = T)) / (max(tag_window_dat,na.rm = T) - min(tag_window_dat,na.rm = T))
        
        data_tag <- data.frame(times = times_tag,
                           tag = as.numeric(tag_scaled))
        
        # Create a ggplot object
        gg <- ggplot(na.omit(data_tag), aes(x = times, y = tag)) +
          geom_point() +
          labs(x = "Time",
               y = "Value",
               title = "Time Series Plot") +
          scale_x_datetime(labels = scales::date_format("%H:%M:%S") + 
          theme_minimal())
        
        # Change the x-axis tick label format
        
        
        track_ids <- unique(smooth_tracks$id)
        track_times <- unique(smooth_tracks$local_timestamp)
        cors <- numeric()
        durs <- numeric()
        flags <- numeric()
        counter = 1
        
        for (ii in 1:length(track_ids)) {
          track <- track_ids[ii]
          candidtae_track <-
            smooth_tracks[smooth_tracks$id == track, ]
          
          dx <- diff(candidtae_track$x_final)
          dy <- diff(candidtae_track$y_final)
          dt <-
            as.numeric(diff(candidtae_track$local_timestamp, units = "secs"))
          vx <- dx / dt
          vy <- dy / dt
          
          candidtae_track$speed[-1] = sqrt(vx ^ 2 + vy ^ 2)
          # Calculate acceleration components (ax and ay)
          dvx <- diff(vx)
          dvy <- diff(vy)
          dt <-
            as.numeric(diff(candidtae_track$local_timestamp[-1])) # Use t[-1] to remove the last timestamp, as diff reduces the length by 1
          ax <- dvx / dt
          ay <- dvy / dt
          pad_indexes <-
            floor((pad_size + 1):(length(ax) -  pad_size))
          tmp <-
            sqrt((dy_acc(ax, win_size = win_size_cont)) ** 2 + (dy_acc(ay, win_size = win_size_cont))**2)
          tmp <- tmp[!is.na(tmp)]
          if (sum(!is.na(tmp)) > 0) {
            
            if (length(candidtae_track$ved[pad_indexes]) == length(tmp)){
              candidtae_track$ved[pad_indexes] <- tmp
            } else {
              candidtae_track$ved[pad_indexes] <- tmp
            }
          }
          
          tracklet_window_dat <-
            candidtae_track[candidtae_track$local_timestamp >= window_start_times[i] &
                              candidtae_track$local_timestamp <= window_end_times[i], ]
          
          if (nrow(tracklet_window_dat) > 0) {
            # print(ii)
            # tracklet_scaled <- (tracklet_window_dat$speed - min_speed_thres) / (max_speed_thres - min_speed_thres)
            tracklet_scaled <-
              tracklet_window_dat$speed > move_speed_thres
            
            tag_scaled_trim <-
              tag_scaled[times >= min(tracklet_window_dat$local_timestamp) &
                           times <= max(tracklet_window_dat$local_timestamp)]
            
            # Find the intersection between the two vectors
            intersection <-
              intersect(times_tag, tracklet_window_dat$local_timestamp)
            # Get the indexes of the intersection elements in vector1
            indexes_tag <- which(times_tag %in% intersection)
            # Get the indexes of the intersection elements in vector2
            indexes_tracklet <-
              which(tracklet_window_dat$local_timestamp %in% intersection)
            
            tag_scaled_trim <- tag_scaled[indexes_tag]
            
            
            # cors[ii] <- cor.test( tag_scaled_trim, tracklet_scaled )$estimate
            # cors[ii] <- cor(as.numeric(tag_scaled_trim[!is.na(tag_scaled_trim)]), as.numeric(tracklet_scaled[!is.na(tag_scaled_trim)]))$estimate
            # tag_for_corr <-
            #   as.numeric(tag_scaled_trim[!is.na(tag_scaled_trim)])
            # tracklet_for_corr <-
            #   as.numeric(tracklet_scaled[!is.na(tag_scaled_trim)])
            # #data_df <- data.frame(tag_for_corr, tracklet_for_corr)
            
            # intersection <- sum(tag_for_corr & tracklet_for_corr)  # in both vectors
            # union <- sum(tag_for_corr | tracklet_for_corr)         # in either vector
            # cors[ii] <- intersection / union
            
            data_kappa <- data.frame(
              tag = tag_scaled_trim,
              tracklet = tracklet_scaled
            ) # times = tracklet_window_dat$local_timestamp  ,
            
            data_kappa <- na.omit(data_kappa)
            
            if (sum(!is.na(data_kappa$tag)) > 0) {
              cors[ii] <- kappa2(data_kappa)$value
              durs[ii] <- nrow(data_kappa)
            }
            # Calculate Cohen's Kappa
            data_tracklet_plot <- data.frame(
              times = tracklet_window_dat$local_timestamp  ,
              tracklet = as.numeric(tracklet_scaled) + counter
            )
            
            
            gg = gg + geom_point(
              data = na.omit(data_tracklet_plot),
              aes(x = times, y = tracklet),
              color = colors[ii]
            )
            counter <- counter + 1
            
          }
          
        }
      }
        # plot_list[[i]] <- gg
        # print(i)
        
        ## plot top N matches
        N <- 5
        dur_thres <- 5
        index_long_dur <- which(durs>dur_thres)
        top_indexes <- index_long_dur[order(cors[index_long_dur], decreasing = TRUE)[1:N]]
        
        data_top_plot <- data.frame(times = times_tag,
                           sc = as.numeric(tag_scaled))
        
        # Create a ggplot object
        gg_top <- ggplot(na.omit(data_top_plot), aes(x = times, y = sc)) +
          geom_point() +
          labs(x = "Time",
               y = "Value",
               title = "Time Series Plot") +
          scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
          theme_minimal()
        
        counter = 2
        for (tt in top_indexes) {
          track <- track_ids[tt]
          candidtae_track <-
            smooth_tracks[smooth_tracks$id == track, ]
          
          
          dx <- diff(candidtae_track$x_final)
          dy <- diff(candidtae_track$y_final)
          dt <-
            as.numeric(diff(candidtae_track$local_timestamp, units = "secs"))
          vx <- dx / dt
          vy <- dy / dt
          
          candidtae_track$speed[-1] = sqrt(vx ^ 2 + vy ^ 2)
          
          tracklet_window_dat <-
            candidtae_track[candidtae_track$local_timestamp >= window_start_times[i] &
                              candidtae_track$local_timestamp <= window_end_times[i], ]
          
          if (nrow(tracklet_window_dat) > 0) {
            #print(track)
            #print(sc)
            
            tracklet_scaled <-
              tracklet_window_dat$speed > move_speed_thres
            
            #print(ii)
            
            tag_scaled_trim <-
              tag_scaled[times >= min(tracklet_window_dat$local_timestamp) &
                           times <= max(tracklet_window_dat$local_timestamp)]
          
            data_top <- data.frame(
              times = tracklet_window_dat$local_timestamp  ,
              sc = as.numeric(tracklet_scaled) + counter
            )
            
            # Create a ggplot object
            # gg <- ggplot(data, aes(x = times, y = sc)) +
            #   geom_point() +
            #   labs(x = "Time", y = "Value", title = "Time Series Plot") +
            #   scale_x_datetime(labels = scales::date_format("%H:%M:%S"))
            #
            #sc <- scale(tracklet_window_dat$ved,center = TRUE, scale = max(tracklet_window_dat$speed,na.rm = T) - min(tracklet_window_dat$speed,na.rm = T))
            #print(ii)
            
            gg_top <-
              gg_top + geom_point(data = na.omit(data_top),
                                  aes(x = times, y = sc),
                                  color = colors[tt], size = 5) + geom_hline(yintercept = counter + 1.5, linetype = "dashed", color = "red")
              
            counter <- counter + 2
          }
          
          
        }
        vec_text = seq(3.25, by = 2, length.out = N)
        
        text_data <- data.frame(
               x = c(rep(times_tag[1], times = c(N))),
               y = c(vec_text),
               label = c(sprintf("%.2f", cors[top_indexes]))
           )
        gg_top <- gg_top +
              geom_text(data = text_data, aes(x = x, y = y, label = label), size = 4, color = "red")
        
        plot_list[[i]] <- gg_top
    }
    pdf_file <- gsub(" ", "_",paste(tag, 'top.pdf'))
    
    # Open the PDF device for writing
    pdf(pdf_file, width = 6, height = 4)  # Adjust width and height as needed
    
    # Loop through the list of plots and save each plot as a separate page in the PDF
    for (k in 1:length(plot_list)) {
      print(k)
      print(plot_list[[k]])
    }
    
    # Close the PDF device
    dev.off()
    
    # Check if the PDF file was saved successfully
    file.exists(pdf_file)
    
    }
        
        


#### Vedba run on each tracklet

window_size <- 60 * 60 * 12
step_size <- 60 * 60 * 12
times_tag <- trim_vedba_smooth$corr_local_timestamp

start_time <- min(trim_vedba_smooth$corr_local_timestamp)
end_time <- max(trim_vedba_smooth$corr_local_timestamp)

window_start_times <-
  start_time
window_end_times <-
  end_time

tags <-
  names(trim_vedba_smooth)[names(trim_vedba_smooth) != 'corr_local_timestamp']

identity_scores_cont_ved <-
  data.frame(
    tag = rep(tags, each = length(window_start_times)),
    window_start_time = rep(window_start_times, times = length(tags)),
    candidate_tracklet = NA,
    score = NA,
    tracklet_moved = NA
  )

speed_thresh <-
  0.2 # this is the speed in meters per second that a baboon needs to exceed to be considered moving

names_list <- names(tracklet_lengths[tracklet_lengths > 0])
colors <- rainbow(length(track_ids))

win_size_cont <- 6
min_speed_thres <- 0
max_speed_thres <- 2.5
move_speed_thres <- 0.2
min_dur <- 10
tag_vedba_thres <- 1.1
pad_size <- (win_size_cont / 2) - 0.5
tag <- tags[3]
plot_list <- list()
cors <- matrix(0, nrow = length(track_ids), ncol = length(tags))
durs <- matrix(0, nrow = length(track_ids), ncol = length(tags))
smooth_tracks$ved <- NA
####
data_top_plot <- data.frame(times = times_tag,
                            sc = as.numeric(tag_scaled))

# Create a ggplot object
gg_top <- ggplot(na.omit(data_top_plot), aes(x = times, y = sc)) +
  geom_point() +
  labs(x = "Time",
       y = "Value",
       title = "Time Series Plot") +
  scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
  theme_minimal()

#### run tracklet by tracklet - no time window and find matching tags

for (ii in 1:length(track_ids)) {
  track <- track_ids[ii]
  candidtae_track <-
    smooth_tracks[smooth_tracks$id == track, ]
  
  dx <- diff(candidtae_track$x_final)
  dy <- diff(candidtae_track$y_final)
  dt <-
    as.numeric(diff(candidtae_track$local_timestamp, units = "secs"))
  vx <- dx / dt
  vy <- dy / dt
  
  candidtae_track$speed[-1] = sqrt(vx ^ 2 + vy ^ 2)
  # Calculate acceleration components (ax and ay)
  dvx <- diff(vx)
  dvy <- diff(vy)
  dt <-
    as.numeric(diff(candidtae_track$local_timestamp[-1])) # Use t[-1] to remove the last timestamp, as diff reduces the length by 1
  ax <- dvx / dt
  ay <- dvy / dt
  pad_indexes <-
    floor((pad_size + 1):(length(ax) -  pad_size))
  tmp <-
    sqrt((dy_acc(ax, win_size = win_size_cont)) ** 2 + (dy_acc(ay, win_size = win_size_cont))**2)
  #tmp <- tmp[!is.na(tmp)]
  if (sum(!is.na(tmp)) > 0) {
    if (length(candidtae_track$speed[pad_indexes]) == length(tmp)){
      candidtae_track$ved[pad_indexes] <- tmp
    } else if (length(candidtae_track$speed[seq(2,length(candidtae_track$speed)-1)]) == length(tmp)) {
      candidtae_track$ved[seq(2,length(candidtae_track$ved)-1)] <- tmp
    } else {
      candidtae_track$ved[pad_indexes] <- tmp
    }
  }
  
  tracklet_scaled <-
    candidtae_track$speed > move_speed_thres
  
  if (nrow(candidtae_track) > 0) {
    
    for (tt in 1:length(tags)) {
      tag <- tags[tt]
      # merge trim_burst_vedba_one_sec AND trim_vedba_smooth
      tag_dat <- trim_vedba_smooth[, tag]
      tag_dat[is.na(tag_dat)] <- trim_burst_vedba_one_sec[,tag][is.na(tag_dat)]
      
      # filter matching times and data
      times_tag_all <- trim_vedba_smooth$corr_local_timestamp
      
      tag_scaled <- log(tag_dat) > tag_vedba_thres
      
      # Find the intersection between the two vectors
      intersection <-
        intersect(times_tag_all, candidtae_track$local_timestamp)
      # Get the indexes of the intersection elements in vector1
      indexes_tag <- which(times_tag_all %in% intersection)
      # Get the indexes of the intersection elements in vector2
      indexes_tracklet <-
        which(candidtae_track$local_timestamp %in% intersection)
      
      tag_scaled_trim <- tag_scaled[indexes_tag]
      tracklet_scaled_trim <- tracklet_scaled[indexes_tracklet]
      
      # calculate correlation 
      data_kappa <- data.frame(
        tag = tag_scaled_trim,
        tracklet = tracklet_scaled_trim
      )
      
      data_kappa <- na.omit(data_kappa)
      # Calculate Cohen's Kappa  - NAN are all true events - usually short ones
      if (sum(data_kappa$tracklet == T,na.rm = T) > 0) {
        cors[ii,tt] <- kappa2(data_kappa)$value
        durs[ii,tt] <- nrow(data_kappa)
      }
      
      # 
    }
  
  }
  
}

colors_tag <- rainbow(length(tags))
thres_cor <- 0.1
smooth_tracks$tag <- NA
gg_tracks <- ggplot() +
  labs(x = "Time",
       y = "Value",
       title = "Time Series Plot") +
  scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
  theme_minimal()
counter = 1
for (ii in 1:length(track_ids)) {
  track <- track_ids[ii]
  if (length(cors[ii,durs[ii, ] > min_dur])>0) {
    max_val <- max(cors[ii,durs[ii, ] > min_dur],na.rm = T)
    max_ind <- which.max(cors[ii,durs[ii, ] > min_dur])
    data_tracks <- data.frame(
      time = smooth_tracks$local_timestamp[smooth_tracks$id == track],
      tracklet = rep(counter,length.out = length(smooth_tracks$local_timestamp[smooth_tracks$id == track])))   
    if (max_val > thres_cor) {
      smooth_tracks$tag[smooth_tracks$id == track] <- as.numeric(tags[max_ind])
      gg_tracks = gg_tracks + 
        geom_point(data = data_tracks, aes(x = time, y = tracklet), size = 4, color = colors_tag[max_ind]) 
      counter <- counter + 1
    } else {
      gg_tracks = gg_tracks + 
        geom_point(data = data_tracks, aes(x = time, y = tracklet), size = 4, color = 'gray') 
      counter <- counter + 1
  }
  }
}
gg_tracks + scale_color_manual(values = colors_tag, labels = tags) + theme(legend.title = element_blank(), 
                                                                           legend.position = "right")

}
#######################
#######################
tag <- tags[1]
for (tag in tags) {
  plot_list <- list()
  print(tag)
  trim_burst_vedba_one_sec
  # merge trim_burst_vedba_one_sec AND trim_vedba_smooth
  
  #input_tag_data[is.na(input_tag_data)] <- trim_burst_vedba_one_sec[,tag][is.na(input_tag_data)]
  
  tag_dat <- trim_vedba_smooth[, tag]
  tag_dat[is.na(tag_dat)] <- trim_burst_vedba_one_sec[,tag][is.na(tag_dat)]
  
  times_tag_all <- input_tag_data$corr_local_timestamp
  
  for (i in 1:length(window_start_times)) { 
    
    tag_window_dat <-
      tag_dat[times_tag_all >= window_start_times[i] &
                times_tag_all <= window_end_times[i]]
    
    times_tag <-
      times_tag_all[times_tag_all >= window_start_times[i] &
                      times_tag_all <= window_end_times[i]]
    if (sum(!is.na(tag_window_dat)) > 0) {
      # sc <- scale(tag_window_dat,center = FALSE, scale = max(tag_window_dat,na.rm = T) - min(tag_window_dat,na.rm = T))
      
      tag_scaled <- log(tag_window_dat) > tag_vedba_thres
      # tag_scaled <- (tag_window_dat - min(tag_window_dat,na.rm = T)) / (max(tag_window_dat,na.rm = T) - min(tag_window_dat,na.rm = T))
      
      data_tag <- data.frame(times = times_tag,
                             tag = as.numeric(tag_scaled))
      
      # Create a ggplot object
      gg <- ggplot(na.omit(data_tag), aes(x = times, y = tag)) +
        geom_point() +
        labs(x = "Time",
             y = "Value",
             title = "Time Series Plot") +
        scale_x_datetime(labels = scales::date_format("%H:%M:%S") + 
                           theme_minimal())
      
      # Change the x-axis tick label format
      
      
      track_ids <- unique(smooth_tracks$id)
      track_times <- unique(smooth_tracks$local_timestamp)
      cors <- numeric()
      durs <- numeric()
      flags <- numeric()
      counter = 1
      for (ii in 1:length(track_ids)) {
        track <- track_ids[ii]
        candidtae_track <-
          smooth_tracks[smooth_tracks$id == track, ]
        
        dx <- diff(candidtae_track$x_final)
        dy <- diff(candidtae_track$y_final)
        dt <-
          as.numeric(diff(candidtae_track$local_timestamp, units = "secs"))
        vx <- dx / dt
        vy <- dy / dt
        
        candidtae_track$speed[-1] = sqrt(vx ^ 2 + vy ^ 2)
        # Calculate acceleration components (ax and ay)
        dvx <- diff(vx)
        dvy <- diff(vy)
        dt <-
          as.numeric(diff(candidtae_track$local_timestamp[-1])) # Use t[-1] to remove the last timestamp, as diff reduces the length by 1
        ax <- dvx / dt
        ay <- dvy / dt
        pad_indexes <-
          floor((pad_size + 1):(length(ax) -  pad_size))
        tmp <-
          sqrt((dy_acc(ax, win_size = win_size_cont)) ** 2 + (dy_acc(ay, win_size = win_size_cont))**2)
        tmp <- tmp[!is.na(tmp)]
        if (sum(!is.na(tmp)) > 0) {
          
          if (length(candidtae_track$ved[pad_indexes]) == length(tmp)){
            candidtae_track$ved[pad_indexes] <- tmp
          } else {
            candidtae_track$ved[pad_indexes] <- tmp
          }
        }
        
        tracklet_window_dat <-
          candidtae_track[candidtae_track$local_timestamp >= window_start_times[i] &
                            candidtae_track$local_timestamp <= window_end_times[i], ]
        
        if (nrow(tracklet_window_dat) > 0) {
          # print(ii)
          # tracklet_scaled <- (tracklet_window_dat$speed - min_speed_thres) / (max_speed_thres - min_speed_thres)
          tracklet_scaled <-
            tracklet_window_dat$speed > move_speed_thres
          
          tag_scaled_trim <-
            tag_scaled[times >= min(tracklet_window_dat$local_timestamp) &
                         times <= max(tracklet_window_dat$local_timestamp)]
          
          # Find the intersection between the two vectors
          intersection <-
            intersect(times_tag, tracklet_window_dat$local_timestamp)
          # Get the indexes of the intersection elements in vector1
          indexes_tag <- which(times_tag %in% intersection)
          # Get the indexes of the intersection elements in vector2
          indexes_tracklet <-
            which(tracklet_window_dat$local_timestamp %in% intersection)
          
          tag_scaled_trim <- tag_scaled[indexes_tag]
          
          
          # cors[ii] <- cor.test( tag_scaled_trim, tracklet_scaled )$estimate
          # cors[ii] <- cor(as.numeric(tag_scaled_trim[!is.na(tag_scaled_trim)]), as.numeric(tracklet_scaled[!is.na(tag_scaled_trim)]))$estimate
          # tag_for_corr <-
          #   as.numeric(tag_scaled_trim[!is.na(tag_scaled_trim)])
          # tracklet_for_corr <-
          #   as.numeric(tracklet_scaled[!is.na(tag_scaled_trim)])
          # #data_df <- data.frame(tag_for_corr, tracklet_for_corr)
          
          # intersection <- sum(tag_for_corr & tracklet_for_corr)  # in both vectors
          # union <- sum(tag_for_corr | tracklet_for_corr)         # in either vector
          # cors[ii] <- intersection / union
          
          data_kappa <- data.frame(
            tag = tag_scaled_trim,
            tracklet = tracklet_scaled
          ) # times = tracklet_window_dat$local_timestamp  ,
          
          data_kappa <- na.omit(data_kappa)
          
          if (sum(!is.na(data_kappa$tag)) > 0) {
            cors[ii] <- kappa2(data_kappa)$value
            durs[ii] <- nrow(data_kappa)
          }
          # Calculate Cohen's Kappa
          data_tracklet_plot <- data.frame(
            times = tracklet_window_dat$local_timestamp  ,
            tracklet = as.numeric(tracklet_scaled) + counter
          )
          
          
          gg = gg + geom_point(
            data = na.omit(data_tracklet_plot),
            aes(x = times, y = tracklet),
            color = colors[ii]
          )
          counter <- counter + 1
          
        }
        
      }
    }
    # plot_list[[i]] <- gg
    # print(i)
    
    ## plot top N matches
    N <- 5
    dur_thres <- 5
    index_long_dur <- which(durs>dur_thres)
    top_indexes <- index_long_dur[order(cors[index_long_dur], decreasing = TRUE)[1:N]]
    
    data_top_plot <- data.frame(times = times_tag,
                                sc = as.numeric(tag_scaled))
    
    # Create a ggplot object
    gg_top <- ggplot(na.omit(data_top_plot), aes(x = times, y = sc)) +
      geom_point() +
      labs(x = "Time",
           y = "Value",
           title = "Time Series Plot") +
      scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
      theme_minimal()
    
    counter = 2
    for (tt in top_indexes) {
      track <- track_ids[tt]
      candidtae_track <-
        smooth_tracks[smooth_tracks$id == track, ]
      
      
      dx <- diff(candidtae_track$x_final)
      dy <- diff(candidtae_track$y_final)
      dt <-
        as.numeric(diff(candidtae_track$local_timestamp, units = "secs"))
      vx <- dx / dt
      vy <- dy / dt
      
      candidtae_track$speed[-1] = sqrt(vx ^ 2 + vy ^ 2)
      
      tracklet_window_dat <-
        candidtae_track[candidtae_track$local_timestamp >= window_start_times[i] &
                          candidtae_track$local_timestamp <= window_end_times[i], ]
      
      if (nrow(tracklet_window_dat) > 0) {
        #print(track)
        #print(sc)
        
        tracklet_scaled <-
          tracklet_window_dat$speed > move_speed_thres
        
        #print(ii)
        
        tag_scaled_trim <-
          tag_scaled[times >= min(tracklet_window_dat$local_timestamp) &
                       times <= max(tracklet_window_dat$local_timestamp)]
        
        data_top <- data.frame(
          times = tracklet_window_dat$local_timestamp  ,
          sc = as.numeric(tracklet_scaled) + counter
        )
        
        # Create a ggplot object
        # gg <- ggplot(data, aes(x = times, y = sc)) +
        #   geom_point() +
        #   labs(x = "Time", y = "Value", title = "Time Series Plot") +
        #   scale_x_datetime(labels = scales::date_format("%H:%M:%S"))
        #
        #sc <- scale(tracklet_window_dat$ved,center = TRUE, scale = max(tracklet_window_dat$speed,na.rm = T) - min(tracklet_window_dat$speed,na.rm = T))
        #print(ii)
        
        gg_top <-
          gg_top + geom_point(data = na.omit(data_top),
                              aes(x = times, y = sc),
                              color = colors[tt], size = 5) + geom_hline(yintercept = counter + 1.5, linetype = "dashed", color = "red")
        
        counter <- counter + 2
      }
      
      
    }
    vec_text = seq(3.25, by = 2, length.out = N)
    
    text_data <- data.frame(
      x = c(rep(times_tag[1], times = c(N))),
      y = c(vec_text),
      label = c(sprintf("%.2f", cors[top_indexes]))
    )
    gg_top <- gg_top +
      geom_text(data = text_data, aes(x = x, y = y, label = label), size = 4, color = "red")
    
    plot_list[[i]] <- gg_top
  }
  pdf_file <- gsub(" ", "_",paste(tag, 'top.pdf'))
  
  # Open the PDF device for writing
  pdf(pdf_file, width = 6, height = 4)  # Adjust width and height as needed
  
  # Loop through the list of plots and save each plot as a separate page in the PDF
  for (k in 1:length(plot_list)) {
    print(k)
    print(plot_list[[k]])
  }
  
  # Close the PDF device
  dev.off()
  
  # Check if the PDF file was saved successfully
  file.exists(pdf_file)
  
}
#######################
#######################

      
#### rest of the code ####

    
    tracklet_window_dat <-
      candidtae_track[candidtae_track$local_timestamp >= window_start_times[i] &
                        candidtae_track$local_timestamp <= window_end_times[i], ]
    
    
    if (nrow(tracklet_window_dat) > 0) {
      print(track)
      #print(sc)
      
      sc <-
        scale(
          tracklet_window_dat$ved,
          center = TRUE,
          scale = max(tracklet_window_dat$speed, na.rm = T) - min(tracklet_window_dat$speed, na.rm = T)
        )
      print(ii)
      
      data <- data.frame(times = times,
                         sc = sc)
      
      # Create a ggplot object
      gg <- ggplot(data, aes(x = times, y = sc)) +
        geom_point() +
        labs(x = "Time", y = "Value", title = "Time Series Plot") +
        scale_x_datetime(labels = scales::date_format("%H:%M:%S"))
      
      sc <-
        scale(
          tracklet_window_dat$ved,
          center = TRUE,
          scale = max(tracklet_window_dat$speed, na.rm = T) - min(tracklet_window_dat$speed, na.rm = T)
        )
      print(ii)
      
      data <- data.frame(times = tracklet_window_dat$local_timestamp,
                         sc = sc)
      
      gg + geom_line(data = data, aes(x = times, y = sc), color = colors[ii])
      
      
      # lines(tracklet_window_dat$local_timestamp,tracklet_window_dat$speed, col = colors[ii])
      
    }
  

print(gg)

#  "2019-08-06 17:03:37 UTC" "2019-08-06 17:13:36 UTC"
######################################################
#####################################################

for (tag in tags) {
  print(tag)
  
  tag_dat <- trim_vedba_smooth[, tag]
  
  for (i in 1:length(window_start_times)) {
    score <- 0
    
    tag_window_dat <-
      tag_dat[trim_vedba_smooth$corr_local_timestamp >= window_start_times[i] &
                trim_vedba_smooth$corr_local_timestamp <= window_end_times[i]]
    
    if (sum(!is.na(tag_window_dat)) > 0) {
      tracklet_window_dat <-
        trim_tracklet_speeds[trim_tracklet_speeds$local_timestamp >= window_start_times[i] &
                               trim_tracklet_speeds$local_timestamp <= window_end_times[i] , names(trim_tracklet_speeds) != 'local_timestamp']
      
      #track_ids <- unique(smooth_tracks$id)
      #track_times <- unique(smooth_tracks$local_timestamp)
      
      
      cors <-
        apply(
          tracklet_window_dat,
          2,
          FUN = function(x)
            cor_function(x, tag_window_dat)
        )
      
      plot(scale(tag_window_dat), ylim = c(0, 10))
      tracklet_lengths <-
        apply(
          tracklet_window_dat,
          MARGIN = 2,
          FUN = function(x)
            sum(!is.na(x))
        )
      
      # plot example
      names_list <- names(tracklet_lengths[tracklet_lengths > 0])
      colors <- rainbow(length(tracklet_lengths))
      
      for (i in 1:length(names_list)) {
        lines(scale(tracklet_window_dat[names_list[[i]]]), col = colors[i])
      }
      
      
      
      longest_track <- max(tracklet_lengths)
      
      if (longest_track == 0) {
        # if there are not tracklets during this time window (that pretty much only happens when we have completed tracks at the beginning of the night and at the end of the night, but not in the middle of the night -- and now we are looking to match the tag with tracklets in the middle of the night), then skip this time window and move on to the next
        
        next
        
      }
      
      inds_to_remove <-
        which(tracklet_lengths < 0.9 * longest_track)
      
      full_tracklets_window <-
        tracklet_window_dat[,-inds_to_remove]
      
      rm(closest_tracklet) # just to be safe
      
      if ('numeric' %in% class(full_tracklets_window)) {
        # if there is only one tracklet that takes up the full window
        
        closest_tracklet <-
          names(which(tracklet_lengths == longest_track))
        
        correl <-
          cor_function(full_tracklets_window, tag_window_dat)
        
        if (is.na(correl)) {
          next
          
        }
        
      } else{
        cors <-
          apply(
            full_tracklets_window,
            2,
            FUN = function(x)
              cor_function(x, tag_window_dat)
          )
        
        
        # this needs to be here because sometimes there is tracklets that reach the duration requirements, but can still have little or no overlap with the tag data becase the tag data itself is quite lacking
        cors <- cors[!is.na(cors)]
        
        if (sum(!is.na(cors)) == 0) {
          next
          
        }
        
        closest_tracklet <- names(which.max(cors))
        
        if (length(cors) >= 4) {
          outlier_scores <-
            DescTools::LOF(cors, max(floor(length(cors) / 3), 2))
          
          cand_outlier_score <-
            outlier_scores[names(cors) == closest_tracklet]
          
          score <-
            ifelse(cand_outlier_score > 2, score + 1, score)
        }
        
      }
      
      identity_scores_cont_ved[identity_scores_cont_ved$tag == tag &
                                 identity_scores_cont_ved$window_start_time == window_start_times[i], 'candidate_tracklet'] <-
        closest_tracklet
      
      ## subset to the tracklet data during this time window
      focal_tracklet_window_dat <-
        trim_tracklet_speeds[trim_tracklet_speeds$local_timestamp >= window_start_times[i] &
                               trim_tracklet_speeds$local_timestamp <= window_end_times[i] , closest_tracklet]
      
      tags_window_dat <-
        trim_vedba_smooth[trim_vedba_smooth$corr_local_timestamp >= window_start_times[i] &
                            trim_vedba_smooth$corr_local_timestamp <= window_end_times[i] , names(trim_vedba_smooth) != 'corr_local_timestamp']
      
      tracklet_cors <-
        apply(
          tags_window_dat,
          2,
          FUN = function(x)
            cor_function(x, focal_tracklet_window_dat)
        )
      
      if (names(tracklet_cors)[which.max(tracklet_cors)] == tag) {
        score <- score + 1
        
        tracklet_cors <- tracklet_cors[!is.na(tracklet_cors)]
        
        if (length(tracklet_cors) >= 4) {
          outlier_scores <-
            DescTools::LOF(tracklet_cors, max(floor(length(
              tracklet_cors
            ) / 3), 2))
          
          tag_outlier_score <-
            outlier_scores[names(tracklet_cors) == tag]
          
          score <-
            ifelse(tag_outlier_score > 2, score + 1, score)
          
        }
      }
      
      moved <-
        sum(focal_tracklet_window_dat > speed_thresh, na.rm = T)
      
      if (moved > 0) {
        score <- score + 1
        
        identity_scores_cont_ved[identity_scores_cont_ved$tag == tag &
                                   identity_scores_cont_ved$window_start_time == window_start_times[i], 'tracklet_moved'] <-
          1
        
      } else{
        identity_scores_cont_ved[identity_scores_cont_ved$tag == tag &
                                   identity_scores_cont_ved$window_start_time == window_start_times[i], 'tracklet_moved'] <-
          0 # could just instantiate this column filled with 0s and then leave this else clause out, but this will be more likely to catch mistakes in the code, as there shouldn't be any Nas left
        
      }
      
      identity_scores_cont_ved[identity_scores_cont_ved$tag == tag &
                                 identity_scores_cont_ved$window_start_time == window_start_times[i], 'score'] <-
        score
      
    }
  }
}


identity_scores_cont_ved$window_start_time <-
  as.POSIXct(identity_scores_cont_ved$window_start_time,
             origin = '1970-01-01',
             tz = 'UTC')

identity_scores_cont_ved[which(identity_scores_cont_ved$score >= 2),]

write.csv(
  identity_scores_cont_ved,
  paste0(
    'DATA/thermal_tracks/tracklet_identification/',
    night_of_int,
    '/identity_scores_cont_ved.csv'
  ),
  row.names = F
)

## trim_burst_vedba

window_size <- 10 * 60

step_size <- 1 * 60

start_time <- min(trim_burst_vedba$corr_local_timestamp)

end_time <- max(trim_burst_vedba$corr_local_timestamp)

window_start_times <-
  seq(from = start_time,
      to = (end_time - window_size + 1),
      by = step_size)

window_end_times <-
  seq(from = (start_time + window_size - 1),
      to = end_time,
      by = step_size)

tags <-
  names(trim_burst_vedba)[names(trim_burst_vedba) != 'corr_local_timestamp']

identity_scores_burst_ved <-
  data.frame(
    tag = rep(tags, each = length(window_start_times)),
    window_start_time = rep(window_start_times, times = length(tags)),
    candidate_tracklet = NA,
    score = NA,
    tracklet_moved = NA
  )

speed_thresh <-
  0.2 # this is the speed in meters per second that a baboon needs to exceed to be considered moving


trim_tracklet_speeds_to_bursts <-
  tracklet_speeds[tracklet_speeds$local_timestamp %in% trim_burst_vedba$corr_local_timestamp,]

for (tag in tags) {
  print(tag)
  
  tag_dat <- trim_burst_vedba[, tag]
  
  dat_times <-
    trim_burst_vedba$corr_local_timestamp[!is.na(trim_burst_vedba[, tag])]
  
  ## rel_times is short for relevant times
  rel_times_smooth_tracks <-
    smooth_tracks[smooth_tracks$local_timestamp %in% dat_times,]
  
  for (i in 1:length(window_start_times)) {
    score <- 0
    
    tag_window_dat <-
      tag_dat[trim_burst_vedba$corr_local_timestamp >= window_start_times[i] &
                trim_burst_vedba$corr_local_timestamp <= window_end_times[i]]
    
    if (sum(!is.na(tag_window_dat)) > 0) {
      tracklet_window_dat <-
        trim_tracklet_speeds_to_bursts[trim_tracklet_speeds_to_bursts$local_timestamp >= window_start_times[i] &
                                         trim_tracklet_speeds_to_bursts$local_timestamp <= window_end_times[i] , names(trim_tracklet_speeds_to_bursts) != 'local_timestamp']
      
      cors <-
        apply(
          tracklet_window_dat,
          2,
          FUN = function(x)
            cor_function(x, tag_window_dat)
        )
      
      tracklet_lengths <-
        apply(
          tracklet_window_dat,
          MARGIN = 2,
          FUN = function(x)
            sum(!is.na(x))
        )
      
      longest_track <- max(tracklet_lengths)
      
      if (longest_track == 0) {
        # if there are not tracklets during this time window (that pretty much only happens when we have completed tracks at the beginning of the night and at the end of the night, but not in the middle of the night -- and now we are looking to match the tag with tracklets in the middle of the night), then skip this time window and move on to the next
        
        next
        
      }
      
      inds_to_remove <-
        which(tracklet_lengths < 0.9 * longest_track)
      
      full_tracklets_window <-
        tracklet_window_dat[,-inds_to_remove]
      
      rm(closest_tracklet) # just to be safe
      
      if ('numeric' %in% class(full_tracklets_window)) {
        # if there is only one tracklet that takes up the full window
        
        closest_tracklet <-
          names(which(tracklet_lengths == longest_track))
        
        correl <-
          cor_function(full_tracklets_window, tag_window_dat)
        
        if (is.na(correl)) {
          next
          
        }
        
      } else{
        cors <-
          apply(
            full_tracklets_window,
            2,
            FUN = function(x)
              cor_function(x, tag_window_dat)
          )
        
        # this needs to be here because sometimes there is tracklets that reach the duration requirements, but can still have little or no overlap with the tag data becase the tag data itself is quite lacking
        cors <- cors[!is.na(cors)]
        
        if (sum(!is.na(cors)) == 0) {
          next
          
        }
        
        closest_tracklet <- names(which.max(cors))
        
        if (length(cors) >= 4) {
          outlier_scores <-
            DescTools::LOF(cors, max(floor(length(cors) / 3), 2))
          
          cand_outlier_score <-
            outlier_scores[names(cors) == closest_tracklet]
          
          score <-
            ifelse(cand_outlier_score > 2, score + 1, score)
        }
        
      }
      
      identity_scores_burst_ved[identity_scores_burst_ved$tag == tag &
                                  identity_scores_burst_ved$window_start_time == window_start_times[i], 'candidate_tracklet'] <-
        closest_tracklet
      
      ## subset to the tracklet data during this time window
      focal_tracklet_window_dat <-
        trim_tracklet_speeds[trim_tracklet_speeds$local_timestamp >= window_start_times[i] &
                               trim_tracklet_speeds$local_timestamp <= window_end_times[i] , closest_tracklet]
      
      tags_window_dat <-
        trim_burst_vedba_one_sec[trim_burst_vedba_one_sec$corr_local_timestamp >= window_start_times[i] &
                                   trim_burst_vedba_one_sec$corr_local_timestamp <= window_end_times[i] , names(trim_burst_vedba_one_sec) != 'corr_local_timestamp']
      
      tracklet_cors <-
        apply(
          tags_window_dat,
          2,
          FUN = function(x)
            cor_function(x, focal_tracklet_window_dat)
        )
      
      if (sum(!is.na(tracklet_cors)) > 0) {
        if (names(tracklet_cors)[which.max(tracklet_cors)] == tag) {
          score <- score + 1
          
          tracklet_cors <- tracklet_cors[!is.na(tracklet_cors)]
          
          if (length(tracklet_cors) >= 4) {
            outlier_scores <-
              DescTools::LOF(tracklet_cors, max(floor(length(
                tracklet_cors
              ) / 3), 2))
            
            tag_outlier_score <-
              outlier_scores[names(tracklet_cors) == tag]
            
            score <-
              ifelse(tag_outlier_score > 2, score + 1, score)
            
          }
        }
        
      }
      
      moved <-
        sum(rel_times_smooth_tracks[rel_times_smooth_tracks$local_timestamp >= window_start_times[i] &
                                      rel_times_smooth_tracks$local_timestamp <= window_end_times[i] &
                                      rel_times_smooth_tracks$id == closest_tracklet, 'speed'] > speed_thresh, na.rm = T)
      
      if (moved > 0) {
        score <- score + 1
        
        identity_scores_burst_ved[identity_scores_burst_ved$tag == tag &
                                    identity_scores_burst_ved$window_start_time == window_start_times[i], 'tracklet_moved'] <-
          1
        
      } else{
        identity_scores_burst_ved[identity_scores_burst_ved$tag == tag &
                                    identity_scores_burst_ved$window_start_time == window_start_times[i], 'tracklet_moved'] <-
          0 # could just instantiate this column filled with 0s and then leave this else clause out, but this will be more likely to catch mistakes in the code, as there shouldn't be any Nas left
        
      }
      
      identity_scores_burst_ved[identity_scores_burst_ved$tag == tag &
                                  identity_scores_burst_ved$window_start_time == window_start_times[i], 'score'] <-
        score
      
    }
  }
}

identity_scores_burst_ved$window_start_time <-
  as.POSIXct(identity_scores_burst_ved$window_start_time,
             origin = '1970-01-01',
             tz = 'UTC')

identity_scores_burst_ved[which(identity_scores_burst_ved$score >= 3),]

write.csv(
  identity_scores_burst_ved,
  paste0(
    'DATA/thermal_tracks/tracklet_identification/',
    night_of_int,
    '/identity_scores_burst_ved.csv'
  ),
  row.names = F
)



save.image(
  paste0(
    'DATA/thermal_tracks/tracklet_identification/',
    night_of_int,
    '/tracklet_id_dat.RData'
  )
)
#
# load( paste0( 'DATA/thermal_tracks/tracklet_identification/', night_of_int,  '/tracklet_id_dat.RData' )  )
#

####### assigning identities with burst ved ##########

# a score equal to or greater than the score_thresh will be considered as contributing the identity assignment
score_thresh <- 3

# as soon as an tag pairs with a tracklet with a score higher than the score_thresh for number_needed different times (within the consec window), we will assign the tracklet as the identity of the tag in the thermal imagery. Note the number_needed will get reset after an identity gets assigned
number_needed <- 3

# consec_window is the amount of time (in seconds) within which the number_needed of instances of the score_thresh being exceeded needs to be reached to in order to assign a tracklet identity to a tag (just make this really large if you don't really want to limit it). This is good to set just to prevent tracklets randomly accumulating scores over the score_thresh and then eventually reaching the number_needed
consec_window <- 20 * 60 # this parameter seems to have little affect

tags <- as.character(unique(identity_scores_burst_ved$tag))

ident_dat_burst_ved <-
  as.data.frame(matrix(NA, nrow = 0, ncol = 3))

## identity start time will represent the time at which the thresholds above were reached. So it is not the first time the tracklet and the tag show a good match, but rather, the time at which they have first shown a sustained match within a given time period
names(ident_dat_burst_ved) <-
  c('tag', 'ident_start_time', 'tracklet')

options(warn = 0)

for (tag in tags) {
  switch <- 0
  
  tag_dat <-
    identity_scores_burst_ved[identity_scores_burst_ved$tag == tag,]
  
  for (i in 1:nrow(tag_dat)) {
    if (switch == 0) {
      inds_in_play <-
        which(tag_dat$window_start_time > (tag_dat$window_start_time[i] - consec_window))
      
    } else{
      inds_in_play <-
        which(
          tag_dat$window_start_time > (tag_dat$window_start_time[i] - consec_window) &
            tag_dat$window_start_time > id_time
        )
    }
    
    window_dat <- tag_dat[min(inds_in_play):i,]
    
    high_score_dat <-
      window_dat[which(window_dat$score >= score_thresh),]
    
    num_instances_tab <-
      table(high_score_dat$candidate_tracklet)
    
    if (sum(num_instances_tab >= number_needed) > 0) {
      id_tracklets <-
        names(num_instances_tab)[which(num_instances_tab >= number_needed)]
      
      for (id_tracklet in id_tracklets) {
        tracklet_dat <-
          high_score_dat[high_score_dat$candidate_tracklet == id_tracklet,]
        
        if (sum(tracklet_dat$tracklet_moved) > 0) {
          ###### TAKE OUT THE EQUALS SIGN IF YOU WANT THE CONDITION THAT IS HAS TO BE MOVING TO
          
          switch <- 1
          
          id_time <-
            tracklet_dat$window_start_time[nrow(tracklet_dat)]
          
          ident_dat_burst_ved <-
            rbind(
              ident_dat_burst_ved,
              data.frame(
                tag = tag,
                ident_start_time = id_time,
                tracklet = id_tracklet
              )
            )
          
          break
        }
        
      }
      
    }
    
  }
  
}



ident_dat_burst_ved

length(unique(ident_dat_burst_ved$tag))

if (nrow(ident_dat_burst_ved) != 0) {
  ## remove the duplicates (aka consecutive reindification of the same tracklet)
  rem_inds <- c()
  
  if (nrow(ident_dat_burst_ved) > 1) {
    for (i in 2:nrow(ident_dat_burst_ved)) {
      if (ident_dat_burst_ved$tag[i] == ident_dat_burst_ved$tag[i - 1] &
          ident_dat_burst_ved$tracklet[i] == ident_dat_burst_ved$tracklet[i - 1]) {
        rem_inds <- c(rem_inds, i)
        
      }
      
    }
    
    if (length(rem_inds) != 0) {
      ident_dat_burst_ved <- ident_dat_burst_ved[-rem_inds,]
      
    }
    
  }
  
  
  ## determine the end time of the identification. This is either the end time of the tracklet or the time when the identification switches to a different tracklet
  ident_dat_burst_ved$ident_end_time <- NA
  
  tags <- as.character(unique(ident_dat_burst_ved$tag))
  
  for (tag in tags) {
    tag_dat <- ident_dat_burst_ved[ident_dat_burst_ved$tag == tag,]
    
    for (i in 1:nrow(tag_dat)) {
      target_tracklet <- tag_dat$tracklet[i]
      
      tracklet_end <-
        max(smooth_tracks[smooth_tracks$id == target_tracklet, 'local_timestamp'])
      
      if (i == nrow(tag_dat)) {
        tag_dat$ident_end_time[i] <- tracklet_end
        
      } else{
        next_tracklet_start <- tag_dat$ident_start[i + 1]
        
        tag_dat$ident_end_time[i] <-
          min(tracklet_end, next_tracklet_start)
        
      }
      
    }
    
    ident_dat_burst_ved[ident_dat_burst_ved$tag == tag,] <- tag_dat
    
  }
  
  ident_dat_burst_ved$ident_end_time <-
    as.POSIXct(ident_dat_burst_ved$ident_end_time,
               origin = '1970-01-01 00:00:00',
               tz = 'UTC')
  
  
  ## solve conflicts over two individuals wanting the same tracklet
  dup_tracklets <-
    as.character(unique(ident_dat_burst_ved$tracklet[duplicated(ident_dat_burst_ved$tracklet)]))
  
  for (tracklet in dup_tracklets) {
    tracklet_dat <-
      ident_dat_burst_ved[ident_dat_burst_ved$tracklet == tracklet,]
    
    if (length(unique(tracklet_dat$tag)) != 1) {
      # remove the data from the ident_dat_burst_ved dataframe
      to_remove <- apply(tracklet_dat, 1 , paste, collapse = '')
      
      removed_dat <-
        ident_dat_burst_ved[apply(ident_dat_burst_ved, 1 , paste, collapse = '') %in% to_remove,]
      
      ident_dat_burst_ved <-
        ident_dat_burst_ved[!apply(ident_dat_burst_ved, 1 , paste, collapse = '') %in% to_remove,]
      
      replacement_dat <- tracklet_dat[0,]
      
      times <- vector(mode = 'list')
      
      earliest_time <- min(tracklet_dat$ident_start_time)
      
      latest_time <- max(tracklet_dat$ident_end_time)
      
      whole_ts <- seq(earliest_time, latest_time, by = '1 sec')
      
      contested_score <- rep(0, length(whole_ts))
      
      for (i in 1:nrow(tracklet_dat)) {
        times[[i]] <-
          seq(tracklet_dat$ident_start_time[i],
              tracklet_dat$ident_end_time[i],
              by = '1 sec')
        
        contested_score <-
          contested_score + whole_ts %in% times[[i]]
        
      }
      
      contested_times <- whole_ts[contested_score > 1]
      
      if (length(contested_times) == 0) {
        # if there is actually no overlap in the times, then just add the removed rows back to the dataframe and move on to the next duplicate
        
        ident_dat_burst_ved <-
          rbind(ident_dat_burst_ved, removed_dat)
        
      } else{
        period <- c(0, cumsum(diff(contested_times) != 1)) + 1
        
        un_periods <- as.numeric(as.character(unique(period)))
        
        for (per in un_periods) {
          contested_per <- contested_times[period == per]
          
          contestant_inds <-
            which(sapply(
              times,
              FUN = function(x)
                sum(contested_per %in% x) > 0
            ))
          
          order_of_go_scores <- c()
          
          for (j in contestant_inds) {
            start_window_comp <- tracklet_dat$ident_start_time[j]
            
            end_window_comp <- tracklet_dat$ident_end_time[j]
            
            ## find the correlation between this individual and the tracklet for the contested period (using the one sec)
            tracklet_per_dat <-
              trim_tracklet_speeds[trim_tracklet_speeds$local_timestamp >= start_window_comp &
                                     trim_tracklet_speeds$local_timestamp <= end_window_comp, tracklet]
            
            tag_per_dat <-
              trim_burst_vedba_one_sec[trim_burst_vedba_one_sec$corr_local_timestamp >= start_window_comp &
                                         trim_burst_vedba_one_sec$corr_local_timestamp <= end_window_comp, tracklet_dat$tag[j]]
            
            final_corr <-
              cor_function(tracklet_per_dat, tag_per_dat)
            
            order_of_go_scores <-
              c(order_of_go_scores, final_corr)
            
          }
          
          order_of_go <-
            contestant_inds[order(order_of_go_scores, decreasing = T)]
          
          already_taken <- c()
          
          for (ind in order_of_go) {
            desired_times <- times[[ind]]
            
            confirmed_times <-
              desired_times[!desired_times %in% already_taken]
            
            times[[ind]] <- confirmed_times
            
            times_removed <-
              contested_per[contested_per %in% confirmed_times]
            
            already_taken <- c(already_taken, times_removed)
            
          }
        }
        
        for (k in 1:length(times)) {
          time_vec <- sort(times[[k]])
          
          if (length(time_vec) != 0) {
            diffs <- c(as.numeric(diff(time_vec), unit = 'secs'))
            
            end_inds <- which(diffs > 1)
            
            id_start_time_inds <- c(1, end_inds + 1)
            
            id_end_time_inds <- c(end_inds, length(time_vec))
            
            replacement_dat <-
              rbind(
                replacement_dat,
                data.frame(
                  tag = tracklet_dat$tag[k],
                  ident_start_time = time_vec[id_start_time_inds],
                  tracklet = tracklet,
                  ident_end_time = time_vec[id_end_time_inds]
                )
              )
            
          }
        }
        
        ident_dat_burst_ved <-
          rbind(ident_dat_burst_ved, replacement_dat)
        
      }
    }
  }
}


write.csv(
  ident_dat_burst_ved,
  paste0(
    'DATA/thermal_tracks/tracklet_identification/',
    night_of_int,
    '/ident_dat_burst_ved.csv'
  ),
  row.names = F
)


########### assigning identities with continuous vedba #############


# a score equal to or greater than the score_thresh will be considered as contributing the identity assignment
score_thresh <- 2

# as soon as an tag pairs with a tracklet with a score higher than the score_thresh for number_needed different times (within the consec window), we will assign the tracklet as the identity of the tag in the thermal imagery. Note the number_needed will get reset after an identity gets assigned
number_needed <- 3

# consec_window is the amount of time (in seconds) within which the number_needed of instances of the score_thresh being exceeded needs to be reached to in order to assign a tracklet identity to a tag (just make this really large if you don't really want to limit it). This is good to set just to prevent tracklets randomly accumulating scores over the score_thresh and then eventually reaching the number_needed
consec_window <- 40 * 60 # this parameter seems to have little affect

tags <- as.character(unique(identity_scores_cont_ved$tag))

ident_dat_cont_ved <-
  as.data.frame(matrix(NA, nrow = 0, ncol = 3))

## identity start time will represent the time at which the thresholds above were reached. So it is not the first time the tracklet and the tag show a good match, but rather, the time at which they have first shown a sustained match within a given time period
names(ident_dat_cont_ved) <-
  c('tag', 'ident_start_time', 'tracklet')

options(warn = 0)
for (tag in tags) {
  switch <- 0
  
  tag_dat <-
    identity_scores_cont_ved[identity_scores_cont_ved$tag == tag,]
  
  for (i in 1:nrow(tag_dat)) {
    if (switch == 0) {
      inds_in_play <-
        which(tag_dat$window_start_time > (tag_dat$window_start_time[i] - consec_window))
      
    } else{
      inds_in_play <-
        which(
          tag_dat$window_start_time > (tag_dat$window_start_time[i] - consec_window) &
            tag_dat$window_start_time > id_time
        )
    }
    
    window_dat <- tag_dat[min(inds_in_play):i,]
    
    high_score_dat <-
      window_dat[which(window_dat$score >= score_thresh),]
    
    num_instances_tab <-
      table(high_score_dat$candidate_tracklet)
    
    if (sum(num_instances_tab >= number_needed) > 0) {
      id_tracklets <-
        names(num_instances_tab)[which(num_instances_tab >= number_needed)]
      
      for (id_tracklet in id_tracklets) {
        tracklet_dat <-
          high_score_dat[high_score_dat$candidate_tracklet == id_tracklet,]
        
        if (sum(tracklet_dat$tracklet_moved) > 0) {
          ###### TAKE OUT THE EQUALS SIGN IF YOU WANT THE CONDITION THAT IS HAS TO BE MOVING TO
          
          switch <- 1
          
          id_time <-
            tracklet_dat$window_start_time[nrow(tracklet_dat)]
          
          ident_dat_cont_ved <-
            rbind(
              ident_dat_cont_ved,
              data.frame(
                tag = tag,
                ident_start_time = id_time,
                tracklet = id_tracklet
              )
            )
          
          break
        }
        
      }
      
    }
    
  }
  
}



ident_dat_cont_ved

length(unique(ident_dat_cont_ved$tag))

if (nrow(ident_dat_cont_ved) != 0) {
  ## remove the duplicates (aka consecutive reidentification of the same tracklet)
  rem_inds <- c()
  
  if (nrow(ident_dat_cont_ved) > 1) {
    for (i in 2:nrow(ident_dat_cont_ved)) {
      if (ident_dat_cont_ved$tag[i] == ident_dat_cont_ved$tag[i - 1] &
          ident_dat_cont_ved$tracklet[i] == ident_dat_cont_ved$tracklet[i - 1]) {
        rem_inds <- c(rem_inds, i)
        
      }
      
    }
    
    if (length(rem_inds) != 0) {
      ident_dat_cont_ved <- ident_dat_cont_ved[-rem_inds,]
      
    }
    
  }
  
  ## determine the end time of the identification. This is either the end time of the tracklet or the time when the identification switches to a different tracklet
  ident_dat_cont_ved$ident_end_time <- NA
  
  tags <- as.character(unique(ident_dat_cont_ved$tag))
  
  for (tag in tags) {
    tag_dat <- ident_dat_cont_ved[ident_dat_cont_ved$tag == tag,]
    
    for (i in 1:nrow(tag_dat)) {
      target_tracklet <- tag_dat$tracklet[i]
      
      tracklet_end <-
        max(smooth_tracks[smooth_tracks$id == target_tracklet, 'local_timestamp'])
      
      if (i == nrow(tag_dat)) {
        tag_dat$ident_end_time[i] <- tracklet_end
        
      } else{
        next_tracklet_start <- tag_dat$ident_start[i + 1]
        
        tag_dat$ident_end_time[i] <-
          min(tracklet_end, next_tracklet_start)
        
      }
      
    }
    
    ident_dat_cont_ved[ident_dat_cont_ved$tag == tag,] <- tag_dat
    
  }
  
  ident_dat_cont_ved$ident_end_time <-
    as.POSIXct(ident_dat_cont_ved$ident_end_time,
               origin = '1970-01-01 00:00:00',
               tz = 'UTC')
  
  
  ## solve conflicts over two individuals wanting the same tracklet
  dup_tracklets <-
    as.character(unique(ident_dat_cont_ved$tracklet[duplicated(ident_dat_cont_ved$tracklet)]))
  
  for (tracklet in dup_tracklets) {
    tracklet_dat <-
      ident_dat_cont_ved[ident_dat_cont_ved$tracklet == tracklet,]
    
    if (length(unique(tracklet_dat$tag)) != 1) {
      # remove the data from the ident_dat_cont_ved dataframe
      to_remove <- apply(tracklet_dat, 1 , paste, collapse = '')
      
      removed_dat <-
        ident_dat_cont_ved[apply(ident_dat_cont_ved, 1 , paste, collapse = '') %in% to_remove,]
      
      ident_dat_cont_ved <-
        ident_dat_cont_ved[!apply(ident_dat_cont_ved, 1 , paste, collapse = '') %in% to_remove,]
      
      replacement_dat <- tracklet_dat[0,]
      
      times <- vector(mode = 'list')
      
      earliest_time <- min(tracklet_dat$ident_start_time)
      
      latest_time <- max(tracklet_dat$ident_end_time)
      
      whole_ts <- seq(earliest_time, latest_time, by = '1 sec')
      
      contested_score <- rep(0, length(whole_ts))
      
      for (i in 1:nrow(tracklet_dat)) {
        times[[i]] <-
          seq(tracklet_dat$ident_start_time[i],
              tracklet_dat$ident_end_time[i],
              by = '1 sec')
        
        contested_score <-
          contested_score + whole_ts %in% times[[i]]
        
      }
      
      contested_times <- whole_ts[contested_score > 1]
      
      if (length(contested_times) == 0) {
        # if there is actually no overlap in the times, then just add the removed rows back to the dataframe and move on to the next duplicate
        
        ident_dat_cont_ved <-
          rbind(ident_dat_cont_ved, removed_dat)
        
      } else{
        period <- c(0, cumsum(diff(contested_times) != 1)) + 1
        
        un_periods <- as.numeric(as.character(unique(period)))
        
        for (per in un_periods) {
          contested_per <- contested_times[period == per]
          
          contestant_inds <-
            which(sapply(
              times,
              FUN = function(x)
                sum(contested_per %in% x) > 0
            ))
          
          order_of_go_scores <- c()
          
          for (j in contestant_inds) {
            start_window_comp <- tracklet_dat$ident_start_time[j]
            
            end_window_comp <- tracklet_dat$ident_end_time[j]
            
            ## find the correlation between this individual and the tracklet for the contested period (using the one sec)
            tracklet_per_dat <-
              trim_tracklet_speeds[trim_tracklet_speeds$local_timestamp >= start_window_comp &
                                     trim_tracklet_speeds$local_timestamp <= end_window_comp, tracklet]
            
            tag_per_dat <-
              trim_vedba_smooth[trim_vedba_smooth$corr_local_timestamp >= start_window_comp &
                                  trim_vedba_smooth$corr_local_timestamp <= end_window_comp, tracklet_dat$tag[j]]
            
            final_corr <-
              cor_function(tracklet_per_dat, tag_per_dat)
            
            order_of_go_scores <-
              c(order_of_go_scores, final_corr)
            
          }
          
          order_of_go <-
            contestant_inds[order(order_of_go_scores, decreasing = T)]
          
          already_taken <- c()
          
          for (ind in order_of_go) {
            desired_times <- times[[ind]]
            
            confirmed_times <-
              desired_times[!desired_times %in% already_taken]
            
            times[[ind]] <- confirmed_times
            
            times_removed <-
              contested_per[contested_per %in% confirmed_times]
            
            already_taken <- c(already_taken, times_removed)
            
          }
        }
        
        for (k in 1:length(times)) {
          time_vec <- sort(times[[k]])
          
          if (length(time_vec) != 0) {
            diffs <- c(as.numeric(diff(time_vec), unit = 'secs'))
            
            end_inds <- which(diffs > 1)
            
            id_start_time_inds <- c(1, end_inds + 1)
            
            id_end_time_inds <- c(end_inds, length(time_vec))
            
            replacement_dat <-
              rbind(
                replacement_dat,
                data.frame(
                  tag = tracklet_dat$tag[k],
                  ident_start_time = time_vec[id_start_time_inds],
                  tracklet = tracklet,
                  ident_end_time = time_vec[id_end_time_inds]
                )
              )
            
          }
        }
        
        ident_dat_cont_ved <-
          rbind(ident_dat_cont_ved, replacement_dat)
        
      }
    }
  }
}


ident_dat_cont_ved

write.csv(
  ident_dat_cont_ved,
  paste0(
    'DATA/thermal_tracks/tracklet_identification/',
    night_of_int,
    '/ident_dat_cont_ved.csv'
  ),
  row.names = F
)


########## assigning identities with GPS speeds #########


# a score equal to or greater than the score_thresh will be considered as contributing the identity assignment
score_thresh <- 3

# as soon as an tag pairs with a tracklet with a score higher than the score_thresh for number_needed different times (within the consec window), we will assign the tracklet as the identity of the tag in the thermal imagery. Note the number_needed will get reset after an identity gets assigned
number_needed <- 3

# consec_window is the amount of time (in seconds) within which the number_needed of instances of the score_thresh being exceeded needs to be reached to in order to assign a tracklet identity to a tag (just make this really large if you don't really want to limit it). This is good to set just to prevent tracklets randomly accumulating scores over the score_thresh and then eventually reaching the number_needed
consec_window <- 40 * 60 # this parameter seems to have little affect

tags <- as.character(unique(identity_scores_gps$tag))

ident_dat_gps_speed <-
  as.data.frame(matrix(NA, nrow = 0, ncol = 3))

## identity start time will represent the time at which the thresholds above were reached. So it is not the first time the tracklet and the tag show a good match, but rather, the time at which they have first shown a sustained match within a given time period
names(ident_dat_gps_speed) <-
  c('tag', 'ident_start_time', 'tracklet')

options(warn = 0)
for (tag in tags) {
  switch <- 0
  
  tag_dat <- identity_scores_gps[identity_scores_gps$tag == tag,]
  
  for (i in 1:nrow(tag_dat)) {
    if (switch == 0) {
      inds_in_play <-
        which(tag_dat$window_start_time > (tag_dat$window_start_time[i] - consec_window))
      
    } else{
      inds_in_play <-
        which(
          tag_dat$window_start_time > (tag_dat$window_start_time[i] - consec_window) &
            tag_dat$window_start_time > id_time
        )
    }
    
    window_dat <- tag_dat[min(inds_in_play):i,]
    
    high_score_dat <-
      window_dat[which(window_dat$score >= score_thresh),]
    
    num_instances_tab <-
      table(high_score_dat$candidate_tracklet)
    
    if (sum(num_instances_tab >= number_needed) > 0) {
      id_tracklets <-
        names(num_instances_tab)[which(num_instances_tab >= number_needed)]
      
      for (id_tracklet in id_tracklets) {
        tracklet_dat <-
          high_score_dat[high_score_dat$candidate_tracklet == id_tracklet,]
        
        if (sum(tracklet_dat$tracklet_moved) > 0) {
          ###### TAKE OUT THE EQUALS SIGN IF YOU WANT THE CONDITION THAT IS HAS TO BE MOVING TO
          
          switch <- 1
          
          id_time <-
            tracklet_dat$window_start_time[nrow(tracklet_dat)]
          
          ident_dat_gps_speed <-
            rbind(
              ident_dat_gps_speed,
              data.frame(
                tag = tag,
                ident_start_time = id_time,
                tracklet = id_tracklet
              )
            )
          
          break
        }
        
      }
      
    }
    
  }
  
}



ident_dat_gps_speed

length(unique(ident_dat_gps_speed$tag))

if (nrow(ident_dat_gps_speed != 0)) {
  ## remove the duplicates (aka consecutive reindification of the same tracklet)
  rem_inds <- c()
  
  if (nrow(ident_dat_gps_speed) > 1) {
    for (i in 2:nrow(ident_dat_gps_speed)) {
      if (ident_dat_gps_speed$tag[i] == ident_dat_gps_speed$tag[i - 1] &
          ident_dat_gps_speed$tracklet[i] == ident_dat_gps_speed$tracklet[i - 1]) {
        rem_inds <- c(rem_inds, i)
        
      }
      
    }
    
    
    if (length(rem_inds) != 0) {
      ident_dat_gps_speed <- ident_dat_gps_speed[-rem_inds,]
      
    }
    
  }
  
  ## determine the end time of the identification. This is either the end time of the tracklet or the time when the identification switches to a different tracklet
  ident_dat_gps_speed$ident_end_time <- NA
  
  tags <- as.character(unique(ident_dat_gps_speed$tag))
  
  for (tag in tags) {
    tag_dat <- ident_dat_gps_speed[ident_dat_gps_speed$tag == tag,]
    
    for (i in 1:nrow(tag_dat)) {
      target_tracklet <- tag_dat$tracklet[i]
      
      tracklet_end <-
        max(smooth_tracks[smooth_tracks$id == target_tracklet, 'local_timestamp'])
      
      if (i == nrow(tag_dat)) {
        tag_dat$ident_end_time[i] <- tracklet_end
        
      } else{
        next_tracklet_start <- tag_dat$ident_start[i + 1]
        
        tag_dat$ident_end_time[i] <-
          min(tracklet_end, next_tracklet_start)
        
      }
      
    }
    
    ident_dat_gps_speed[ident_dat_gps_speed$tag == tag,] <- tag_dat
    
  }
  
  ident_dat_gps_speed$ident_end_time <-
    as.POSIXct(ident_dat_gps_speed$ident_end_time,
               origin = '1970-01-01 00:00:00',
               tz = 'UTC')
  
  
  ## solve conflicts over two individuals wanting the same tracklet
  dup_tracklets <-
    as.character(unique(ident_dat_gps_speed$tracklet[duplicated(ident_dat_gps_speed$tracklet)]))
  
  for (tracklet in dup_tracklets) {
    tracklet_dat <-
      ident_dat_gps_speed[ident_dat_gps_speed$tracklet == tracklet,]
    
    if (length(unique(tracklet_dat$tag)) != 1) {
      # remove the data from the ident_dat_gps_speed dataframe
      to_remove <- apply(tracklet_dat, 1 , paste, collapse = '')
      
      removed_dat <-
        ident_dat_gps_speed[apply(ident_dat_gps_speed, 1 , paste, collapse = '') %in% to_remove,]
      
      ident_dat_gps_speed <-
        ident_dat_gps_speed[!apply(ident_dat_gps_speed, 1 , paste, collapse = '') %in% to_remove,]
      
      replacement_dat <- tracklet_dat[0,]
      
      times <- vector(mode = 'list')
      
      earliest_time <- min(tracklet_dat$ident_start_time)
      
      latest_time <- max(tracklet_dat$ident_end_time)
      
      whole_ts <- seq(earliest_time, latest_time, by = '1 sec')
      
      contested_score <- rep(0, length(whole_ts))
      
      for (i in 1:nrow(tracklet_dat)) {
        times[[i]] <-
          seq(tracklet_dat$ident_start_time[i],
              tracklet_dat$ident_end_time[i],
              by = '1 sec')
        
        contested_score <-
          contested_score + whole_ts %in% times[[i]]
        
      }
      
      contested_times <- whole_ts[contested_score > 1]
      
      if (length(contested_times) == 0) {
        # if there is actually no overlap in the times, then just add the removed rows back to the dataframe and move on to the next duplicate
        
        ident_dat_gps_speed <-
          rbind(ident_dat_gps_speed, removed_dat)
        
      } else{
        period <- c(0, cumsum(diff(contested_times) != 1)) + 1
        
        un_periods <- as.numeric(as.character(unique(period)))
        
        for (per in un_periods) {
          contested_per <- contested_times[period == per]
          
          contestant_inds <-
            which(sapply(
              times,
              FUN = function(x)
                sum(contested_per %in% x) > 0
            ))
          
          order_of_go_scores <- c()
          
          for (j in contestant_inds) {
            start_window_comp <- tracklet_dat$ident_start_time[j]
            
            end_window_comp <- tracklet_dat$ident_end_time[j]
            
            ## find the correlation between this individual and the tracklet for the contested period (using the one sec)
            tracklet_per_dat <-
              trim_tracklet_speeds[trim_tracklet_speeds$local_timestamp >= start_window_comp &
                                     trim_tracklet_speeds$local_timestamp <= end_window_comp, tracklet]
            
            tag_per_dat <-
              trim_gps_speeds_smooth[trim_gps_speeds_smooth$corr_local_timestamp >= start_window_comp &
                                       trim_gps_speeds_smooth$corr_local_timestamp <= end_window_comp, tracklet_dat$tag[j]]
            
            final_corr <-
              cor_function(tracklet_per_dat, tag_per_dat)
            
            order_of_go_scores <-
              c(order_of_go_scores, final_corr)
            
          }
          
          order_of_go <-
            contestant_inds[order(order_of_go_scores, decreasing = T)]
          
          already_taken <- c()
          
          for (ind in order_of_go) {
            desired_times <- times[[ind]]
            
            confirmed_times <-
              desired_times[!desired_times %in% already_taken]
            
            times[[ind]] <- confirmed_times
            
            times_removed <-
              contested_per[contested_per %in% confirmed_times]
            
            already_taken <- c(already_taken, times_removed)
            
          }
        }
        
        for (k in 1:length(times)) {
          time_vec <- sort(times[[k]])
          
          if (length(time_vec) != 0) {
            diffs <- c(as.numeric(diff(time_vec), unit = 'secs'))
            
            end_inds <- which(diffs > 1)
            
            id_start_time_inds <- c(1, end_inds + 1)
            
            id_end_time_inds <- c(end_inds, length(time_vec))
            
            replacement_dat <-
              rbind(
                replacement_dat,
                data.frame(
                  tag = tracklet_dat$tag[k],
                  ident_start_time = time_vec[id_start_time_inds],
                  tracklet = tracklet,
                  ident_end_time = time_vec[id_end_time_inds]
                )
              )
            
          }
        }
        
        ident_dat_gps_speed <-
          rbind(ident_dat_gps_speed, replacement_dat)
      }
    }
  }
  
  ident_dat_gps_speed
  
}

write.csv(
  ident_dat_gps_speed,
  paste0(
    'DATA/thermal_tracks/tracklet_identification/',
    night_of_int,
    '/ident_dat_gps_speed.csv'
  ),
  row.names = F
)


smooth_tracks$tag <- NA

if (nrow(ident_dat_gps_speed) != 0) {
  for (i in 1:nrow(ident_dat_gps_speed)) {
    smooth_tracks$tag[which(
      smooth_tracks$local_timestamp >= ident_dat_gps_speed$ident_start_time[i] &
        smooth_tracks$local_timestamp < ident_dat_gps_speed$ident_end_time[i] &
        smooth_tracks$id == ident_dat_gps_speed$tracklet[i]
    )] <- ident_dat_gps_speed$tag[i]
    
  }
  
}

if (nrow(ident_dat_burst_ved) != 0) {
  for (i in 1:nrow(ident_dat_burst_ved)) {
    smooth_tracks$tag[which(
      smooth_tracks$local_timestamp >= ident_dat_burst_ved$ident_start_time[i] &
        smooth_tracks$local_timestamp < ident_dat_burst_ved$ident_end_time[i] &
        smooth_tracks$id == ident_dat_burst_ved$tracklet[i]
    )] <- ident_dat_burst_ved$tag[i]
    
    
    #smooth_tracks$tag[ which( smooth_tracks$local_timestamp >= ident_dat_gps_speed$ident_start_time[ i ] & smooth_tracks$local_timestamp <= ident_dat_gps_speed$ident_end_time[ i ] & smooth_tracks$id != ident_dat_gps_speed$tracklet[ i ] & smooth_tracks$tag == ident_dat_gps_speed$tag[ i ] ) ] <- ident_dat_gps_speed$tag[ i ]
    
    ## remove other identifications that have been made of this tag at the same time (by previous identification method (i.e. the gps speeds))
    
    if (length(
      which(
        smooth_tracks$local_timestamp >= ident_dat_burst_ved$ident_start_time[i] &
        smooth_tracks$local_timestamp < ident_dat_burst_ved$ident_end_time[i] &
        smooth_tracks$id != ident_dat_burst_ved$tracklet[i] &
        smooth_tracks$tag == ident_dat_burst_ved$tag[i]
      )
    ) > 0) {
      smooth_tracks$tag[which(
        smooth_tracks$local_timestamp >= ident_dat_burst_ved$ident_start_time[i] &
          smooth_tracks$local_timestamp < ident_dat_burst_ved$ident_end_time[i] &
          smooth_tracks$id != ident_dat_burst_ved$tracklet[i] &
          smooth_tracks$tag == ident_dat_burst_ved$tag[i]
      )] <- NA
      
      
    }
  }
  
}

if (nrow(ident_dat_cont_ved) != 0) {
  for (i in 1:nrow(ident_dat_cont_ved)) {
    smooth_tracks$tag[which(
      smooth_tracks$local_timestamp >= ident_dat_cont_ved$ident_start_time[i] &
        smooth_tracks$local_timestamp < ident_dat_cont_ved$ident_end_time[i] &
        smooth_tracks$id == ident_dat_cont_ved$tracklet[i]
    )] <- ident_dat_cont_ved$tag[i]
    
    ## remove other identifications that have been made of this tag at the same time (by previous identification method (i.e. the gps speeds, or burst vedba))
    
    if (length(
      which(
        smooth_tracks$local_timestamp >= ident_dat_cont_ved$ident_start_time[i] &
        smooth_tracks$local_timestamp < ident_dat_cont_ved$ident_end_time[i] &
        smooth_tracks$id != ident_dat_cont_ved$tracklet[i] &
        smooth_tracks$tag == ident_dat_cont_ved$tag[i]
      )
    ) > 0) {
      smooth_tracks$tag[which(
        smooth_tracks$local_timestamp >= ident_dat_cont_ved$ident_start_time[i] &
          smooth_tracks$local_timestamp < ident_dat_cont_ved$ident_end_time[i] &
          smooth_tracks$id != ident_dat_cont_ved$tracklet[i] &
          smooth_tracks$tag == ident_dat_cont_ved$tag[i]
      )] <- NA
      
    }
    
  }
  
}
browser()
saveRDS(
  smooth_tracks,
  paste0(
    "DATA/thermal_tracks/identified_tracks/",
    input_data,
    '/',
    night_of_int,
    "_ident_tracks.rds"
  )
)

return(smooth_tracks)

}


stopImplicitCluster()
