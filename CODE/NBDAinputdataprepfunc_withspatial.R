


library( igraph )
library( hms )
library( raster )

###### User-inputs ######

# contagion_type: must be set to either 'sleep' or 'waking'. Defines whether we are testing for the spread of the transition from sleep to wake ('waking') or the transition from wake to sleep ('sleep')

# night_start and night_end define the period during which we will test for contagion. There are other ways to choose the period in which we will test for contagion aside from setting predefined times, like starting to look for contagion after the majority of the individuals are sleeping. But I think it is safest to start with predefined times and build off of that if necessary
#night_start <- '21:00:00'

#night_end <- '05:00:00' # this has to be some time prior to 06:30

#cosit_net_to_use_network: must be set to either 'daily' or 'aggregated'. This indicates whether we should use the daily co-sitting network to predict diffusion during the following night or whether we should use the co-sitting network aggregated over the full study period to predict diffusion (on all nights)

#cosit_net_to_use_node_attr: must be set to either 'daily' or 'aggregated'. This indicates whether we should use the daily co-sitting network or the co-sitting network aggregated over the full study period to extract eigenvector centralities for the node attributes 

#dup_times_allowable: this declares whether, in the times_of_acq (times of acquisition list), two individuals can have the same time of acquisition or not. If this is set to F, it will add 0.01 seconds to the duplicate times of acquisition (recursively, so that if there are, say, three 4's, the first will remain 4, the second will become 4.01, and the third will become 4.02)

#randomize_ties: declares whether the order of acquisition within true ties needs to be randomized or not

# night_rerun_number: the number of times to rerun each night with the order of true ties randomized each time

# nights_to_use: a vector of indices indicating which nights of the data we want to include in the NBDA analysis. Default is 1:20


# these are just here in case they need to be run for troubleshooting
# contagion_type = 'waking'; night_start = '21:00:00'; night_end = '05:00:00'; cosit_net_to_use_network = 'aggregated'; cosit_net_to_use_node_attr = NULL; dom_net_to_use_network = 'aggregated'; dom_net_to_use_node_attr = NULL; dup_times_allowable = F; randomize_ties = T; night_rerun_number = 1; nights_to_use = 1:20 
# contagion_type = 'waking'; night_start; night_end; cosit_net_to_use_network; cosit_net_to_use_node_attr; dom_net_to_use_network; dom_net_to_use_node_attr; dup_times_allowable = F; randomize_ties = T; night_rerun_number; nights_to_use = 1:20; input_seed = 234; min_tags_per_diffusion = 1;  min_events_per_diffusion = 1

prepare_NBDA_input_spatial <- function( contagion_type = 'waking', night_start, night_end, cosit_net_to_use_network, cosit_net_to_use_node_attr, dom_net_to_use_network, dom_net_to_use_node_attr, dup_times_allowable = F, randomize_ties = T, night_rerun_number, nights_to_use = 1:20, min_tags_per_diffusion = 1, min_events_per_diffusion = 1, min_diffusion_duration = 2, input_seed = 234 ){
  
  set.seed( input_seed )
  
  # save the number of minutes in the diffusion as a global variable. We will need it later in the NBDA 
  if( ( night_start > "12:00:00" & night_end > '12:00:00' ) | ( night_start < "12:00:00" & night_end < '12:00:00') ){
    
    num_mins_in_diffusion <<- ( ( as.numeric( as_hms( night_end ) ) - as.numeric( as_hms( night_start ) ) ) / 60 ) - 1 
    
  }else{
    
    num_mins_in_diffusion <<- ( ( ( as.numeric( as_hms( night_end ) ) + 24*60*60 ) - as.numeric( as_hms( night_start ) ) ) / 60 ) - 1
    
  }
  
  ###### Read in data ######
  
  # load in the sleep-wake data for the whole study period
  total_dat <- readRDS( 'DATA/sleep_analysis_data/total_dat.rds' ) # total_dat contains a row for every minute of the study period, and columns for each individual indicating whether they were asleep (0) or awake (1), as well as columns indicating the local_timestamp, date of that night (with a night lasting from noon to noon, and the data associated with it being the date of the first noon), and local_time. Night and local time are both extractable from local_timestamp, but I find it easiest to have them as their own columns because I typically use them often
  
  # change the night column to a character class
  total_dat$night <- as.character( total_dat$night )
  
  x_dat <- readRDS( 'DATA/sleep_analysis_data/x_dat.rds' )
  y_dat <- readRDS( 'DATA/sleep_analysis_data/y_dat.rds' )
  
  x_dat$night <- as.character( x_dat$night )
  y_dat$night <- as.character( y_dat$night )
  
  # load in the daytime co-sitting network data (this data came from Roi) 
  load( 'DATA/social_data/cosit_2019.RData' )
  # cosit_all - overall rate of co-sitting (26x26)
  # cosit_daily - day by day rate of co-sitting (26x26x30)
  # tgs - tag IDs (26)
  # days - dates (30)
  
  dimnames( cosit_all ) <- list( tgs, tgs )
  dimnames( cosit_daily ) <- list( tgs, tgs, as.character( as.Date(  days ) ) )
  
  # load in the displacement data
  
  if( !is.null( dom_net_to_use_network ) | !is.null( dom_net_to_use_node_attr ) ){
    
    displace_dat <- read.csv( 'DATA/social_data/Displace.csv', header = F )
    
    displace_dat <- as.matrix( displace_dat )
    
    rownames( displace_dat ) <- tgs
    colnames( displace_dat ) <- tgs
    
    diag( displace_dat ) <- 0
    
  }
  
  # load in the metadata from the study that indicates the age and sex class of the individuals, among other things
  meta_df <- read.csv( 'DATA/Collaring_data.csv' )
  
  
  ###### trim the sleep-wake data as needed for NBDA. Nevermind, this actually doesn't do any trimming anymore because we decided the trimming wasn't necessary. But it still visualizes the data ######
  
  # extract the individual identities from the column names of the sleep-wake data
  tag_names <- sort( names( total_dat )[ startsWith( names( total_dat ), '2' ) | startsWith( names( total_dat ), '6' ) ]  )
  
  ### visualize the dropout of collars over time ###
  
  # make a vector of the number of non-NAs in each row of the sleep-wake data (i.e. at each timestamp of the study )
  num_non_na <- apply( total_dat[ , tag_names ], MARGIN = 1, FUN = function( x ) sum( !is.na( x ) ) )
  
  # plot the number of non-NAs as a function of timestamp
  plot( num_non_na ~ total_dat$local_timestamp, type = 'l', xaxt = 'n' )
  axis( 1, at = as.POSIXct( unique( as.Date( total_dat$local_timestamp ) ), tz = 'UTC' ), labels =  unique( as.Date( total_dat$local_timestamp ) ), las = 2 )
  
  # add ablines to indicate the start and end of the night (i.e. the relevant periods for this analysis). Blue dotted lines indicate the beginning of the night and green dotted lines indicate the end of the nights
  abline( v = as.POSIXct( paste( days, night_start ), tz = 'UTC' ), col = 'blue', lty = 2 )
  abline( v = as.POSIXct( paste( days, night_end ), tz = 'UTC' ), col = 'green', lty = 2 )
  
  
  ###### Prepare the co-sitting network for analysis, if it should be used ######
  
  # preparation depends on whether we are using the daily networks or one network for the entire study duration
  
  if( is.null( cosit_net_to_use_network ) ){
    
    # we don't need to trim any nights based on the cositting data if we aren't going to use the cositting data
    total_trim <- total_dat
    
  }else{
    
    if( cosit_net_to_use_network == 'daily' ){ # if we are using the daily networks...
      
      # set the daily networks to be the array that we will use for the NBDA analysis
      cosit_net <- cosit_daily 
      
      # if we are using daily networks, we can only analyze nights following days on which we have a cositting network. Find the nights on which we have both sleep-wake data and a co-sitting network from the day before
      nights <- intersect( as.character( unique( total_dat$night ) ), as.character( days ) )
      
      # further trim the sleep-wake data to only include these nights (namely, this will cut out the first night, as we have no data on the day prior to this night, and thus no co-sitting network) 
      total_trim <- total_dat[ total_dat$night %in% nights, ]
      
      # name the dimensions of the cositting array
      dimnames( cosit_net ) <- list( tgs, tgs, as.character( as.Date( days ) ) )
      
    }else{
      
      if( cosit_net_to_use_network != 'aggregated' ){ # if we aren't using daily networks, then we must be using the aggregated network. If so...
        
        stop( 'cosit_net_to_use_network must be set to daily, aggregated, or NULL' )
      }
      
      # replicate the cositting network for the whole study by the number of days in the study, so we have network for each day (all networks will be the same)
      cosit_net <- replicate( length( as.character( unique( total_dat$night ) ) ), cosit_all )
      
      # we don't need to trim any nights if we are using the aggregated co-sitting network. So just make total_trim the same as total_dat
      total_trim <- total_dat
      
      # name the dimensions of the cositting array
      dimnames( cosit_net ) <- list( tgs, tgs, as.character( unique( total_dat$night ) ) ) 
      
    } 
    
  }
  
  ###### Prepare the dominance network for analysis, if it should be used ######
  
  
  if( is.null( dom_net_to_use_network ) ){
    
    # we don't need to trim any nights based on the dominance data if we aren't going to use the dominance data
    total_trim <- total_trim
    
  }else{
    
    if( dom_net_to_use_network == 'daily' ){ # if we are using the daily networks...
      
      stop( 'daily dominance networks are not yet supported' )
      # 
      # # set the daily networks to be the array that we will use for the NBDA analysis
      # cosit_net <- cosit_daily 
      # 
      # # if we are using daily networks, we can only analyze nights following days on which we have a cositting network. Find the nights on which we have both sleep-wake data and a co-sitting network from the day before
      # nights <- intersect( as.character( unique( total_trim$night ) ), as.character( days ) )
      # 
      # # further trim the sleep-wake data to only include these nights (namely, this will cut out the first night, as we have no data on the day prior to this night, and thus no co-sitting network) 
      # total_trim <- total_trim[ total_trim$night %in% nights, ]
      
      # name the dimensions of the dominance array
      dimnames( dom_net ) <- list( tgs, tgs, as.character( as.Date( days ) ) )
      
    }else{
      
      if( dom_net_to_use_network != 'aggregated' ){ # if we aren't using daily networks, then we must be using the aggregated network. If so...
        
        stop( 'dom_net_to_use_network must be set to daily, aggregated, or NULL' )
      }
      
      # replicate the dominance network for the whole study by the number of days in the study, so we have network for each day (all networks will be the same)
      dom_net <- replicate( length( as.character( unique( total_trim$night ) ) ), displace_dat )
      
      # name the dimensions of the dominance array
      dimnames( dom_net ) <- list( tgs, tgs, as.character( unique( total_trim$night ) ) )
      
      # we don't need to trim any nights if we are using the aggregated co-sitting network. So just make total_trim the same as total_dat
      total_trim <- total_trim
      
    } 
    
  }
  
  
  # save a final vector of the nights that we will include in the analysis 
  nights_temp <- as.character( unique( total_trim$night ) )
  
  # trim this vector to the nights that we actually want to include for analysis
  nights <- nights_temp[ nights_to_use ]
  
  
  
  ###### Create individual-level variables dataframe ######
  
  # extract the age and sex of the individuals from the study meta data
  meta_trim <- meta_df[ , c( 'collar_id', 'sex', 'age_class_vet') ]
  
  # pull out the relevant individuals
  node_attribute_df <- merge( x = data.frame( tag = tag_names ), y = meta_trim, by.x = 'tag', by.y = 'collar_id', all.x = T, all.y = F, sort = F )
  
  # order the dataframe by tag_name (this is important, as it needs to match the order of tag_names in the social network and contagion data)
  node_attribute_df <- node_attribute_df[ order( node_attribute_df$tag ), ]
  
  ###### Extract the contagion information ######
  
  ## use the diff function on the sleep-wake data to find when individuals switch their sleep state, where 1 implies a switch from sleep to awake and -1 implies a switch from awake to sleep. Store this information in a new dataframe, state_change_df. Only difference between the expressions in the different ifelse clauses here is that if set to waking, we won't not the first index as a change to wake if the individual started awake, and if set to sleep, we won't consider the first index as a change to sleep if the individual started asleep. THIS DOESN'T DO THIS FOR THE BEGINNING OF EVERY NIGHT WHICH IT TECHNICALLY SHOULD, BUT BECAUSE WE AREN'T INCLUDING THE BEGINNING OF NIGHTS IN THE DIFFUSIONS, IT SHOULDN'T BE A PROBLEM
  if( contagion_type == 'waking' ){
    
    state_change_df <- as.data.frame( apply( total_trim[ ,  names( total_trim )[ startsWith( names( total_trim ), '2' ) | startsWith( names( total_trim ), '6' ) ] ], 2, FUN = function( x ) diff( c( 1, x ) ) ) )
    
  }else{
    
    if( contagion_type != 'sleep' ){
      
      stop( 'contagion type must be set to sleep or waking' )
      
    }
    
    state_change_df <- as.data.frame( apply( total_trim[ ,  names( total_trim )[ startsWith( names( total_trim ), '2' ) | startsWith( names( total_trim ), '6' ) ] ], 2, FUN = function( x ) diff( c( 0, x ) ) ) )
  }
  
  # order the columns by the individual identities extracted above
  state_change_df <- state_change_df[ , tag_names ] # the columns should already be in this order, but we just need to make sure because we are going to use the index of the change to tell us who woke up (or went to sleep) and when
  
  # add the timestamp and night column to state_change_df (we could just leave it off and reference it in total_trim when we need it, but I like to have everything stick together in the same dataframe, just in case)
  state_change_df[ , c( 'local_timestamp', 'night', 'local_time' ) ] <- total_trim[ , c( 'local_timestamp', 'night', 'local_time' ) ]
  
  # make an empty list that we will fill with one vector for each night, indicating the order of 'acquisition', i.e. the order in which individuals woke up during the night
  order_of_acq <- vector( 'list' )
  
  # make an empty list that we will fill with one vector for each night, indicating the time of 'acquisition', i.e. the time, in minutes from the beginning of the night, that individuals woke up during the night. Each element of each vector will correspond to the same indexed element of the same indexed vector in the order_of_acq list
  time_of_acq <- vector( 'list' )
  
  # make an empty list that we will fill with one vector for each night, indicating the duration for which individuals remained awake after waking. Each element of each vector will correspond to the same indexed element of the same indexed vector in the order_of_acq and time_of_acq lists
  durations <- vector( 'list' )
  
  # make an empty list that we will fill with one matrix per night indicating whether each individual is present (1) or not present (0) (indicating an NA in its data at that time) during the times of acquisition). The matrix will have a row for each individual that has any data on that night, and a column for each time of acquisition
  presence_matrices <- vector( 'list' )
  
  # make an empty list that we will fill with the dataframes of node attributes (individual tag name, age, and sex) for each night for the individuals that have data during that night 
  node_attribute_list <- vector( 'list' )
  
  # make an empty list that we will fill with one matrix per night indicating the cositting network from the previous day (if cosit_net_to_use_network is set to daily, otherwise each matrix within the list will represent the same aggregated cositting network for the sutdy period). These matrices will have individuals removed who do not have data on the given night
  diffuse_net_list <- vector( 'list' )
  
  # make an empty list that we will fill with a vector for each night, indicating whether each individual in the nodes attribute list for that night was already awake at the start of the diffusion (1) or not (0)
  early_demonstrators_list <- vector( 'list' )
  
  # make an empty list that we will fill with a vector for each night, indicating the durations for which individuals were awake, if they were already awake at the beginning of the diffusion. If they were not already awake, thy will have a 0 here
  early_demo_durations_list <- vector( 'list' )
  
  counter <- 1 # set a counter to 1. This counter will advance every time we go through the loop below and will help us insert the information into the right index of the lists above
  
  ## loop through the nights and extract the necessary information
  for( ni in 1:length( nights ) ){ # for each night
    
    night_change_df <- state_change_df[ state_change_df$night == nights[ ni ], ]
    
    # trim the sleep state change data to only the times between night_start and night_end. Also trim the sleep-wake data and location data to only this and between the specificed times (we will use this to find the durations of behaviors below)
    if( ( night_start > "12:00:00" & night_end > '12:00:00' ) | ( night_start < "12:00:00" & night_end < '12:00:00') ){
      
      # trim sleep state change data
      night_change_trim <- night_change_df[ night_change_df$local_time > night_start & night_change_df$local_time < night_end, ]
      
      # trim sleep-wake data
      trim_sub <- total_trim[ total_trim$night == nights[ ni ] & ( total_trim$local_time > night_start & total_trim$local_time < night_end ) , ]
      
      # trim x_location data
      x_sub <-  x_dat[  x_dat$night == nights[ ni ] & (  x_dat$local_time > night_start &  x_dat$local_time < night_end ) , ]
      # trim sleep-wake data
      y_sub <-  y_dat[  y_dat$night == nights[ ni ] & (  y_dat$local_time > night_start &  y_dat$local_time < night_end ) , ]
      
    }else{
      
      # trim sleep state change data
      night_change_trim <- night_change_df[ night_change_df$local_time > night_start | night_change_df$local_time < night_end, ]
      
      # trim sleep-wake data
      trim_sub <- total_trim[ total_trim$night == nights[ ni ] & ( total_trim$local_time > night_start | total_trim$local_time < night_end ) , ]
      
      # trim x_location data
      x_sub <-  x_dat[  x_dat$night == nights[ ni ] & (  x_dat$local_time > night_start |  x_dat$local_time < night_end ) , ]
      # trim sleep-wake data
      y_sub <-  y_dat[  y_dat$night == nights[ ni ] & (  y_dat$local_time > night_start |  y_dat$local_time < night_end ) , ]
      
    }
    
    
    
    trim_sub$members_located <- apply( x_sub[ , tag_names ], 1, function( x ) paste( tag_names[ sort( which( !is.na( x ) ) ) ], collapse = '_' ) ) # the which function should automatically sort, but just in case...
    # 
    # inds_to_remove <- which( members_located != '' )
    # 
    # 
    # members_located <- members_located[ - inds_to_remove ]
    # 
    # trim_subber <- trim_sub[ - inds_to_remove, ]
    # x_subber <- x_sub[ - inds_to_remove, ]
    # y_subber <- y_sub[ - inds_to_remove, ]
    # 
    # 
    ### FOR NOW I AM GOING TO BREAK DIFFUSIONS ANYTIME THERE IS A CHANGE IN WHO HAS LOCATION DATA AVAILABLE. BUT THIS WILL ALSO START A NEW DIFFUSION IF ONE INDIVIDUAL IS MISSING LOCATION DATA FOR A  SINGLE MINUTE. ADD A SOLUTION FOR THIS ABOVE; OR MERGE WITH ALL.Y = T WHEN MAKING FULL_SPAT_DAT
    
    if( sum( trim_sub$members_located != '' ) == 0 ){
      
      next
      
    }else{
      
      ##### this night will be included in analysis, so add node attribute data as appropriate
      
      ## add co-sitting data to the node attribute dataframe if approrpiate
      # save the appropriate network from which to extract the eigenvector centralities for the node attributes. And trim the network to only include the individuals who have data on this night
      
      node_attribute_night <- node_attribute_df 
      
      if( !is.null( cosit_net_to_use_node_attr ) ){
        
        if( cosit_net_to_use_node_attr == 'daily' ){
          
          node_att_cosit_net <- cosit_daily[ , , nights[ ni ] ]
          
        }else{
          
          if( cosit_net_to_use_node_attr != 'aggregated' ){
            
            stop( 'cosit_net_to_use_node_attr must be set to daily, aggregated, or NULL' )
            
          }
          
          node_att_cosit_net <- cosit_all
          
        }
        
        # turn the network into a graph object in igraph
        g <- graph_from_adjacency_matrix( node_att_cosit_net, mode = 'undirected', weighted = T )
        
        # calculate the eigenvector centralities and save them in a dataframe
        eigen_cent_vec <- eigen_centrality( g, weights = E(g)$weight )$vector
        
        if( !identical( names( eigen_cent_vec ), node_attribute_trim$tag ) ) stop( '0')
        
        # add the eigenvector centrality data to the node attribute dataframe
        node_attribute_night$eigen_cent <- eigen_cent_vec
        
      }
      
      
      ## add dominance data to the node attribute dataframe if approrpiate
      # save the appropriate network from which to extract the dominance data for the node attributes. And trim the network to only include the individuals who have data on this night
      if( !is.null( dom_net_to_use_node_attr ) ){
        
        if( dom_net_to_use_node_attr == 'daily' ){
          
          stop( 'daily dominance networks are not yet supported' )
          
          #node_att_dom_net <- dom_daily[  , , nights[ ni ] ]
          
        }else{
          
          if( dom_net_to_use_node_attr != 'aggregated' ){
            
            stop( 'dom_net_to_use_node_attr must be set to daily, aggregated, or NULL' )
            
          }
          
          node_att_dom_net <- displace_dat
          
        }
        
        # calculate the total displacements that an individual gives (on a given day or across the whole study period, according to the input above). This will be a proxy of their dominance rank
        total_displacements <- rowSums( node_att_dom_net, na.rm = T )
        
        if( !identical( names( total_displacements ), node_attribute_trim$tag ) ) stop( '0' )
        
        # add the dominance data to the node attribute dataframe
        node_attribute_night$dominance <- total_displacements
        
      }
    
    
      #####
      
      presence_change <- as.numeric( c( 1 , diff( as.numeric( as.factor( trim_sub$members_located ) ) ) ) != 0 )
      
      split_vec <- cumsum( presence_change )
      
      trim_sub_list <- split( trim_sub, f = split_vec )      
      night_change_trim_list <- split( night_change_trim, f = split_vec )  
        
      x_sub_list <- split( x_sub, f = split_vec )      
      y_sub_list <- split( y_sub, f = split_vec )      
      
      
      for( di in 1:length( trim_sub_list ) ){
        
        trim_sub_dif <- trim_sub_list[[ di ]]
        
        night_change_trim_dif <- night_change_trim_list[[ di ]]
        
        x_sub_dif <- x_sub_list[[ di ]]
        y_sub_dif <- y_sub_list[[ di ]]
        
        if( all( trim_sub_dif$members_located == '' | nrow( trim_sub_dif ) < min_diffusion_duration ) ){
          
          next
          
        }else{
          
          
          # make a vector of the indices of the tags that we will remove from the nights diffusion analysis because they don't have location data (or sleep state change data)
          
          tags_to_remove <- which( apply( night_change_trim_dif[ , tag_names ], 2, function( x ) sum( !is.na( x ) ) ) == 0 )
          
          tags_to_remove <- sort( c( tags_to_remove, which( apply( x_sub_dif[ , tag_names ], 2, function( x ) sum( !is.na( x ) ) ) == 0 ) ) )
          
          
          # remove these tags from the vector of identities so we just have a vector of identities of individuals who actually have data on this night. Sort them as well, although they are already sorted, but just an extra precaution because the order is important
          if( length( tags_to_remove ) == 0 ){
            
            dif_tags <- tag_names
            
          }else{
            
            dif_tags <- sort( tag_names[ - tags_to_remove ] )
            
          }
          
          if( length( dif_tags ) < min_tags_per_diffusion ){
            
            next
            
          }else{
            
            # trim the night_change_trim data (the contagion data about when individuals went to sleep and woke) to only include these individuals that have data on this night
            night_change_trimmer_dif <- night_change_trim_dif[ , dif_tags, drop = F ]
            
            # remove rows from the node attribute data that are associated with tags who have no data during this diffusion
            node_attribute_trim <- node_attribute_night[ node_attribute_night$tag %in% dif_tags, , drop = F ]

            # trim the sleep-wake data to only the individuals that had data on this night
            trim_subber_dif <- trim_sub_dif[ , dif_tags, drop = F ]
            
            # make a dataframe of the durations of each sleep state for the individuals that have data on this night. When we index this dataframe to the start time of a behavior for a particular individual (or really anytime within the behavior), it will return the duration of the behavior indexed
            durations_df <- as.data.frame( apply( trim_subber_dif, 2, function( x ) rep( rle( x )$lengths, times = rle( x )$lengths ) ) ) 
            
            x_subber_dif <- x_sub_dif[ , dif_tags, drop = F ]
            y_subber_dif <- y_sub_dif[ , dif_tags, drop = F ]
            
          
            # find the indices of a sleep state change (either sleep to wake or wake to sleep, depending on the contagion type specified above)
            if( contagion_type == 'waking' ){
              
              change_inds <- which( night_change_trimmer_dif == 1, arr.ind = T )
              
            }else{
              
              if( contagion_type != 'sleep' ){
                
                stop( 'contagion type must be set to sleep or waking' )
              }
              
              change_inds <- which( night_change_trimmer_dif == -1, arr.ind = T )
              
            }
            
            if( nrow( change_inds ) < min_events_per_diffusion ){
              
              next
              
            }else{
              
              # order the indices of the sleep state change by row (i.e. by timestamp)
              change_inds_ordered <- change_inds[ order( change_inds[ , 1 ] ), , drop = F ]
              
              for( rand in 1:night_rerun_number ){ # repeat the next insertions in the final lists (and randomization of the order of acquisition of true ties) as many times as input above
                
                # just a check to make sure that we have the same tags in the node attribute dataframe and the vector that we will use to subset our contagion data, and that they are in the same order
                if( !identical( node_attribute_trim$tag, names( night_change_trimmer_dif ) ) ) stop( '1' )
                
                # add the trimmed node attribute data to the final list of node attribute information
                node_attribute_list[[ counter ]] <- node_attribute_trim
                
                # The second column of change_inds_ordered indicates the order of the individuals falling asleep. However, when individuals falls asleep at the same time, the individual with lower index in tag_names will always be treated as if it fell asleep first (and the individual with the highest index will always be treated as if it fell asleep last, and so on). So, we need to randomize the order of the individuals who change their sleep state at the same time. The next several lines will do this randomization
                
                if( randomize_ties ){
                  
                  # first save the unadulterated indices that indicate the order of changing sleep state
                  inds <- 1: nrow( change_inds_ordered )
                  
                  # split this indices into a list of vectors, with numbers in the same list element (i.e. same vector) indicated indices corresponding to individuals that changed their sleep state at the same time
                  list_of_inds <- split( inds, change_inds_ordered[ , 1 ] )
                  
                  # for individuals that change their sleep state at the same time, randomize the order of their corresponding indices (this function will actually randomize all list elements, but there is only one index in each list element when there was only one individual that changed its sleep state at that time)
                  new_list_of_inds <- lapply( list_of_inds, FUN = function( x ) as.numeric( sample( as.character( x ) ) ) )
                  
                  # unlist the new list of indices to create on complete vector with the new, shuffled (where appropriate) indices
                  new_inds <- as.numeric( unlist( new_list_of_inds ) )
                  
                  # reorder the sleep state change order and time of acquisition according to the new indices
                  final_change_inds_ordered <- change_inds_ordered[ new_inds, , drop = F ]
                  
                }else{
                  
                  # if we don't need to randomize the order of the true ties, simply save the unadulterated changed inds as the final change inds 
                  final_change_inds_ordered <- change_inds_ordered
                }
                
                # save the order of acquisition (i.e. order of waking) for this night in the appropriate list
                order_of_acq[[ counter ]] <- final_change_inds_ordered[ , 2 ]
                
                if( dup_times_allowable ){ # if duplicate times are allowable in the times of acquisition list (this is a user-input set at the top of the script)
                  
                  # save the time of acquisition (i.e. time of waking) for this night in the appropriate list
                  time_of_acq[[ counter ]] <- final_change_inds_ordered[ , 1 ]
                  
                }else{ # if duplicate times are not allowable
                  
                  # save a temporary vector with the times of acquisition
                  temp_time_of_acq <- final_change_inds_ordered[ , 1 ]
                  
                  if( max( table( temp_time_of_acq ) ) > 100 ){ # if there are more than 100 wake up events that happen at the same time (obviously not possible for this dataset), the following solution to the duplicates won't work, so stop the code and print an error
                    
                    stop( "more than 100 wake up events occur at the same time. The current solution to dealing with duplicate times of acquisition will not work" )
                    
                  }
                  
                  while( sum( duplicated( temp_time_of_acq ) ) > 0 ){ # as long as there are duplicate times of acquisition
                    
                    temp_time_of_acq[ duplicated( temp_time_of_acq ) ] <- temp_time_of_acq[ duplicated( temp_time_of_acq ) ] + 0.01 # add 0.01 to the duplicated times. This needs to be done recursively in the while loop because if there is more than one duplicate of a specific time, there will still be 2 (or more) elements with the same time of acquisition after adding 0.01 to the duplicates
                    
                  }
                  
                  # save the time of acquisition (i.e. time of waking), without duplicates, for this night in the appropriate list
                  time_of_acq[[ counter ]] <- temp_time_of_acq
                  
                }
                
                # save the duration of waking for this night in the appropriate list
                durations[[ counter ]] <- as.numeric( durations_df[ final_change_inds_ordered ] ) - 1
                
                # make a presence absence matrix indicating whether each individual (indicated by row) had sleep state change data at each time of an acquisition (waking event) in the group, and add it directly to the appropriate list
                presence_matrices[[ counter ]] <- t( apply( night_change_trimmer_dif[ final_change_inds_ordered[ , 1 ], , drop = F ], MARGIN = c( 1, 2 ), function( x ) as.numeric( !is.na( x ) ) ) )
                
                # just another check to make sure the tag names line up
                if( !identical( rownames( presence_matrices[[ counter ]] ), node_attribute_trim$tag ) ) stop( '2' )
                
                ## add the cositting and/or dominance networks to the list of networks that will be used for diffusions. Trim them to only include the individuals that have data on this particular night If they are both included, they will be inserted as a three dimensional array, and the first network will always be cositting and second will always be dominance
              
                
                ## add the networks. You need to add one for each time of acquisition
                
                
                spat_net_array <- array( NA, dim = c( length( dif_tags ), length( dif_tags ), nrow( final_change_inds_ordered ) ) )
                
                for( i in 1: length( time_of_acq[[ counter]] ) ){
                  
                  time_ind <- time_of_acq[[ counter ]][ i ]
                  
                  spat_net <-  1 / ( pointDistance( cbind( as.numeric( x_subber_dif[ time_ind, ] ), as.numeric( y_subber_dif[ time_ind,  ] ) ), lonlat = F ) ) # this is the inverse of the dyadic distances, so that individuals that are closer together have a greater edge weight
                  
                  ## this is where Inf was arising from. Remove infinity
                  diag( spat_net ) <- 0
                  
                  ## this is where Inf was arising from. Remove infinity
                  spat_net_array[ , , i ] <- spat_net
                  
                }
                
                dimnames(  spat_net_array ) <- list( dif_tags, dif_tags, time_of_acq[[ counter ]] )

                
                if( !is.null( cosit_net_to_use_network ) & !is.null( dom_net_to_use_network ) ){
                  
                  cosit_slice <- cosit_net[ dif_tags, dif_tags, nights[ ni ] ]
                  dom_slice <- dom_net[ dif_tags, dif_tags, nights[ ni ] ] 
                  

                  full_net_arr <- array( NA, dim = c( length( dif_tags ), length( dif_tags ), 3, length( time_of_acq[[ counter ]] ) ), dimnames = list( dif_tags, dif_tags, c( 'cosit', 'dominance', 'spatial' ), paste( nights[ ni ],  time_of_acq[[ counter ]], sep = '_' ) ) )
                  
                  full_net_arr[ , , 1, ] <- replicate( length( time_of_acq[[ counter ]] ), cosit_slice )
                  full_net_arr[ , , 2, ] <- replicate( length( time_of_acq[[ counter ]] ), dom_slice )
                  full_net_arr[ , , 3, ] <- spat_net_array
                  
                  diffuse_net_list[[ counter ]] <- full_net_arr
                  
                }else{
                  
                  if( !is.null( cosit_net_to_use_network ) ){
                    
                    cosit_slice <- cosit_net[ dif_tags, dif_tags, nights[ ni ] ]
                    
                    full_net_arr <- array( NA, dim = c( length( dif_tags ), length( dif_tags ), 2, length( time_of_acq[[ counter ]] ) ), dimnames = list( dif_tags, dif_tags, c( 'cosit', 'spatial' ), paste( nights[ ni ],  time_of_acq[[ counter ]], sep = '_' ) ) )
                    
                    full_net_arr[ , , 1, ] <- replicate( length( time_of_acq[[ counter ]] ), cosit_slice )
                    full_net_arr[ , , 2, ] <- spat_net_array
                    
                    diffuse_net_list[[ counter ]] <- full_net_arr
                    
                  }else{
                    
                    if( !is.null( dom_net_to_use_network ) ){
                      
                      dom_slice <- dom_net[ dif_tags, dif_tags, nights[ ni ] ] 
                      
                      full_net_arr <- array( NA, dim = c( length( dif_tags ), length( dif_tags ), 2, length( time_of_acq[[ counter ]] ) ), dimnames = list( dif_tags, dif_tags, c( 'dominance', 'spatial' ), paste( nights[ ni ],  time_of_acq[[ counter ]], sep = '_' ) ) )
                      
                      full_net_arr[ , , 1, ] <- replicate( length( time_of_acq[[ counter ]] ), dom_slice )
                      full_net_arr[ , , 2, ] <- spat_net_array
                      
                      diffuse_net_list[[ counter ]] <- full_net_arr
                      
                    }
                  }
                }
                
                # just another check to make sure the tag names line up
                if( !identical( rownames( diffuse_net_list[[ counter ]] ), node_attribute_trim$tag ) ) stop( '3' )
                
                ## we want to know who was already awake at the beginning of the diffusion (demonstrators) and how long they were awake for
                
                # make a binary vector indicating whether each individual woke up during the first minute of the diffusion
                waking_at_start <- night_change_trim_dif[ 1, dif_tags ]
                
                # make a binary vector indicating whether each individual was awake during the first minute of the diffusion
                awake_at_start <- trim_sub_dif[ 1, dif_tags ]
                
                # make a vector indicating whether each individual was already awake at the beginning of the diffusion
                demonstrators <- as.numeric( awake_at_start == 1 & waking_at_start != 1 )
                
                # turn NAs into 0's
                demonstrators[ is.na( demonstrators ) ] <- 0
                
                # save a vector indicating whether each individual was already awake at the beginning of the diffusion to the relevant list
                early_demonstrators_list[[ counter ]] <- demonstrators
                
                # extract a vector of the durations for which each individuals was in their particular sleep state at the start of the diffusion
                demonstrator_durations <- durations_df[ 1, dif_tags ] - 1 
                
                # set the durations to 0 for those that were not already awake at the beginning of the diffusion
                demonstrator_durations[ demonstrators != 1 ] <- 0
                
                # save these durations in the relevant list
                early_demo_durations_list[[ counter ]] <- demonstrator_durations
                
                if( !identical( dif_tags, node_attribute_trim$tag ) ) stop( '4' )
                
                counter <- counter + 1  #advance the counter 
              }
              
            }
                
          }
          
        }
          
      }
      
    }

  }
  
  
  # create a directory and save the lists as R objects within the directory
  dir.create( paste( getwd(), 'DATA/sleep_analysis_data/NBDA_data', sep = "/" ) )
  
  # read the R objects back in
  order_of_acq <<- order_of_acq
  time_of_acq <<- time_of_acq
  durations <<- durations
  diffuse_net_list <<- diffuse_net_list
  presence_matrices <<- presence_matrices
  node_attribute_list <<- node_attribute_list
  early_demonstrators_list <<- early_demonstrators_list
  early_demo_durations_list <<- early_demo_durations_list
  
  
  # save environment
  save.image( paste0( 'DATA/sleep_analysis_data/NBDA_data/NBDA_input_data_', cosit_net_to_use_network, '_', cosit_net_to_use_node_attr, '_', night_rerun_number, '.RData' ) )
  
}



