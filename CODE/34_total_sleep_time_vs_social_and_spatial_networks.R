
library( data.table )
library( lmerTest )
library( igraph )
library( sjPlot )
library( brms )

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}


class_meth <- 'percentile_thresh'


sep_thresh <- T

sleep_per <- fread( paste0( 'DATA/sleep_analysis_data/sleep_per_', class_meth, '_sep_thresh_', sep_thresh, '.csv' ) )

sleep_per <- as.data.frame( sleep_per )

meta_df <- read.csv( 'DATA/Collaring_data.csv' )

sleep_full <- merge( x = sleep_per, y = meta_df[ , c( 'collar_id', 'sex', 'age_class_vet', 'age_class_akiko' ) ], by.x = 'tag', by.y = 'collar_id', all.x = T, all.y = F, sort = F )

sleep_full$total_sleep_bouts[ is.na( sleep_full$TST ) ] <- NA  ## total sleep bouts didn't get cleaned out, so it has to be cleaned out now with the NAs in TST

sleep_full$total_pot_sleep[ is.na( sleep_full$TST ) ] <- NA ## this was not cleaned for the OG algorithm data, so we must do it here

sleep_full$dark_pot_sleep[ is.na( sleep_full$TST ) ] <- NA ## this was not cleaned for the OG algorithm data, so we must do it here


### what is the average and standard error number of sleep epoch per night?

mean( sleep_full$total_sleep_bouts, na.rm = T ) / 60
( sd( sleep_full$total_sleep_bouts, na.rm = T ) / 60 ) / sqrt( sum( !is.na( sleep_full$total_sleep_bouts ) ) ) 



load( 'DATA/social_data/cosit_2019.RData' )
# cosit_all - overall rate of co-sitting (26x26)
# cosit_daily - day by day rate of co-sitting (26x26x30)
# tgs - tag IDs (26)
# days - dates (30)

dimnames( cosit_all ) <- list( tgs, tgs )

dimnames( cosit_daily ) <- list( tgs, tgs, as.character( days ) )


hist( rowSums( cosit_all ) )

g <- graph_from_adjacency_matrix( cosit_all, mode = 'undirected', weighted = T )

E(g)$weight


displace_dat <- read.csv( 'DATA/social_data/Displace.csv', header = F )

displace_dat <- as.matrix( displace_dat )

rownames( displace_dat ) <- tgs
colnames( displace_dat ) <- tgs

displace_vec <- rowSums( displace_dat, na.rm = T )
displace_net_vec <- rowSums( displace_dat, na.rm = T ) - colSums( displace_dat, na.rm = T )


cosit_dat <- data.frame( tag = tgs, eigen_cent = eigen_centrality( g )$vector, strength = strength( g ), weighted_degree = rowSums( cosit_all ), betweenness = betweenness( g, weights = E(g)$weight ), displace = displace_vec, displace_net = displace_net_vec )


sleep_fuller <- merge( x = sleep_full, y = cosit_dat, by = 'tag', all.x = T, all.y = T, sort = F )

sleep_fuller$night <- as.factor( sleep_fuller$night )

sleep_fuller$tag_type <- as.factor( as.numeric( startsWith( as.character( sleep_fuller$tag ), '2' ) ) )


## total number of sleep epochs

m2_dat <- sleep_fuller[ !is.na( sleep_fuller$total_sleep_bouts ), ] ## total sleep bouts didn't get cleaned out, so it has to be cleaned out now with the NAs in TST


## function for normalizing a vector
normalize_func <- function( x ) return( (x - mean( x, na.rm = T ) )/ sd( x, na.rm = T ) )



m2_dat$total_sleep_bouts_std <- normalize_func( m2_dat$total_sleep_bouts )


m2_dat$eigen_cent_std <- normalize_func( m2_dat$eigen_cent )

m2_dat$displace_net_std <- normalize_func( m2_dat$displace_net )


priors <- c(prior(normal(0,2), class = "b", coef = eigen_cent_std),
            
            prior(normal(0,2), class = "b", coef = displace_net_std),
            
            prior(normal(0,2), class = "b", coef = sexmale),
            
            prior(normal(0,2), class = "b", coef = age_class_vetjuvenile),
            
            prior(normal(0,2), class = "b", coef = age_class_vetsubadult),
            
            prior(normal(0,2), class = "sd", group = night)
            
)


options(mc.cores = parallel::detectCores() )


total_sleep_bouts_mod <- brm( total_sleep_bouts_std ~ eigen_cent_std + displace_net_std + sex + age_class_vet + ( 1 | night ) + ( 1 | tag ), data = m2_dat, family= "gaussian", iter = 5000, control = list(max_treedepth = 30, adapt_delta = .999999999999999 ) )

dir.create( paste0( getwd(), "/RESULTS/models/" ) )

saveRDS( total_sleep_bouts_mod, "RESULTS/models/total_sleep_bouts_mod.rds")

total_sleep_bouts_mod <- readRDS( "RESULTS/models/total_sleep_bouts_mod.rds" )

summary( total_sleep_bouts_mod )

# tab_model( total_sleep_bouts_mod )

pp_check( total_sleep_bouts_mod )

most_cent_sleep <- ( ( max( m2_dat$eigen_cent_std )*-0.14 + 0.08 )*sd( m2_dat$total_sleep_bouts, na.rm = T ) ) + mean( m2_dat$total_sleep_bouts, na.rm = T )

least_cent_sleep <- ( ( min( m2_dat$eigen_cent_std )*-0.14 + 0.08 )*sd( m2_dat$total_sleep_bouts, na.rm = T ) ) + mean( m2_dat$total_sleep_bouts, na.rm = T )

least_cent_sleep - most_cent_sleep 

library( ggplot2 )



plot( conditional_effects( total_sleep_bouts_mod, "eigen_cent_std" ), theme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) )[[ 1 ]] + labs( x = 'Eigenvector centrality', y = 'Total time spent sleeping (min)')

plot( conditional_effects( total_sleep_bouts_mod, "displace_net_std" ), theme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) )[[ 1 ]] + labs( x = 'Scaled dominance', y = 'Total time spent sleeping (min)')



mod_dat_to_plot <- conditional_effects( total_sleep_bouts_mod, "eigen_cent_std" )[[1]]

mod_dat_to_plot$untrans_estimate__ <- ( mod_dat_to_plot$estimate__*sd( m2_dat$total_sleep_bouts, na.rm = T ) + mean( m2_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot$untrans_lower__ <- ( mod_dat_to_plot$lower__*sd( m2_dat$total_sleep_bouts, na.rm = T ) + mean( m2_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot$untrans_upper__ <- ( mod_dat_to_plot$upper__*sd( m2_dat$total_sleep_bouts, na.rm = T ) + mean( m2_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot$untrans_eigen_cent <- mod_dat_to_plot$eigen_cent_std * sd( m2_dat$eigen_cent, na.rm = T ) + mean( m2_dat$eigen_cent, na.rm = T )


mod_dat_to_plot_2 <- conditional_effects( total_sleep_bouts_mod, "displace_net_std" )[[1]]

mod_dat_to_plot_2$untrans_estimate__ <- ( mod_dat_to_plot_2$estimate__*sd( m2_dat$total_sleep_bouts, na.rm = T ) + mean( m2_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot_2$untrans_lower__ <- ( mod_dat_to_plot_2$lower__*sd( m2_dat$total_sleep_bouts, na.rm = T ) + mean( m2_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot_2$untrans_upper__ <- ( mod_dat_to_plot_2$upper__*sd( m2_dat$total_sleep_bouts, na.rm = T ) + mean( m2_dat$total_sleep_bouts, na.rm = T ) ) / 60


y_lims <- range( c( mod_dat_to_plot$untrans_lower__, mod_dat_to_plot$untrans_upper__, mod_dat_to_plot_2$untrans_lower__, mod_dat_to_plot_2$untrans_upper__ ) )




plot( mod_dat_to_plot$untrans_eigen_cent, mod_dat_to_plot$untrans_estimate__, type = 'l', xlim = range( mod_dat_to_plot$untrans_eigen_cent ), ylim = y_lims, bty = 'l', ylab = 'Total time spent sleeping (hours)', xlab = "Eigenvector centrality in affiliative network" ) 

polygon( x = c( mod_dat_to_plot$untrans_eigen_cent, rev( mod_dat_to_plot$untrans_eigen_cent ) ), y = c( mod_dat_to_plot$untrans_lower__, rev( mod_dat_to_plot$untrans_upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot$untrans_eigen_cent, mod_dat_to_plot$untrans_estimate__, type = 'l', lwd = 2, col = 'blue' )


## black for plotting
par( bg = 'black' )

plot( mod_dat_to_plot$untrans_eigen_cent, mod_dat_to_plot$untrans_estimate__, type = 'l', xlim = range( mod_dat_to_plot$untrans_eigen_cent ), ylim = y_lims, bty = 'l', ylab = 'Total time spent sleeping (hours)', xlab = "Eigenvector centrality in affiliative network", col = 'white', col.axis = 'white', col.lab = 'white', col.main = 'white' )  

axis( 1, labels = F, col = 'white' )
axis( 2, labels = F, col = 'white' )


polygon( x = c( mod_dat_to_plot$untrans_eigen_cent, rev( mod_dat_to_plot$untrans_eigen_cent ) ), y = c( mod_dat_to_plot$untrans_lower__, rev( mod_dat_to_plot$untrans_upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot$untrans_eigen_cent, mod_dat_to_plot$untrans_estimate__, type = 'l', lwd = 2, col = 'white' )





plot( mod_dat_to_plot_2$displace_net_std, mod_dat_to_plot_2$untrans_estimate__, type = 'l', xlim = range( mod_dat_to_plot_2$displace_net_std ), ylim = y_lims, bty = 'l', ylab = 'Total time spent sleeping (hours)', xlab = "Dominance (standardized)" ) 

polygon( x = c( mod_dat_to_plot_2$displace_net_std, rev( mod_dat_to_plot_2$displace_net_std ) ), y = c( mod_dat_to_plot_2$untrans_lower__, rev( mod_dat_to_plot_2$untrans_upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot_2$displace_net_std, mod_dat_to_plot_2$untrans_estimate__, type = 'l', lwd = 2, col = 'blue' )


## black for plotting 

plot( mod_dat_to_plot_2$displace_net_std, mod_dat_to_plot_2$untrans_estimate__, type = 'l', xlim = range( mod_dat_to_plot_2$displace_net_std ), ylim = y_lims, bty = 'l', ylab = 'Total time spent sleeping (hours)', xlab = "Dominance (standardized)", col = 'white', col.axis = 'white', col.lab = 'white', col.main = 'white' ) 

axis( 1, labels = F, col = 'white' )
axis( 2, labels = F, col = 'white' )

polygon( x = c( mod_dat_to_plot_2$displace_net_std, rev( mod_dat_to_plot_2$displace_net_std ) ), y = c( mod_dat_to_plot_2$untrans_lower__, rev( mod_dat_to_plot_2$untrans_upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot_2$displace_net_std, mod_dat_to_plot_2$untrans_estimate__, type = 'l', lwd = 2, col = 'white' )




### Modeling the influence of the spatial network on total number of epochs of sleep


spat_net_time <- '22:00:00'


spatial_net_measures <- read.csv( paste0( 'DATA/thermal_tracks/spatial_net_measures_positions.csv' ) )

class( spatial_net_measures$night )
class( sleep_fuller$night_date )

# sleep_per$night_date has to be made into a character object so that it is mergeable
sleep_fuller$night_date <- as.character( sleep_fuller$night_date )


spatial_net_measures_sub <- spatial_net_measures[ spatial_net_measures$local_time == spat_net_time,  ]


net_sleep_dat <- merge( x = sleep_fuller, y = spatial_net_measures_sub, by.x = 'night_date', by.y = 'night', all.x = T, all.y = F, sort = F )


mod_dat <- net_sleep_dat[ complete.cases( net_sleep_dat$total_sleep_bouts, net_sleep_dat$density ), ]


mod_dat$total_sleep_bouts_std <- normalize_func( mod_dat$total_sleep_bouts )



mod_dat$density_std <- normalize_func( mod_dat$density )

mod_dat$modularity_std <- normalize_func( mod_dat$modularity )

priors <- c(prior(normal(0,2), class = "b", coef = density_std),
            
            prior(normal(0,2), class = "b", coef = modularity_std),
            
            prior(normal(0,2), class = "b", coef = sexmale),
            
            prior(normal(0,2), class = "b", coef = age_class_vetjuvenile),
            
            prior(normal(0,2), class = "b", coef = age_class_vetsubadult)
            
)


options(mc.cores = parallel::detectCores() )


spatial_total_sleep_bouts_mod <- brm( total_sleep_bouts_std ~ density_std + modularity_std + sex + age_class_vet + ( 1 | night ) + ( 1 | tag ), data = mod_dat, family= "gaussian", iter = 5000, control = list(max_treedepth = 30, adapt_delta = .999999999999999 ) )

dir.create( paste0( getwd(), "/RESULTS/models/" ) )

saveRDS( spatial_total_sleep_bouts_mod, "RESULTS/models/spatial_total_sleep_bouts_mod.rds")

spatial_total_sleep_bouts_mod <- readRDS( "RESULTS/models/spatial_total_sleep_bouts_mod.rds" )

summary( spatial_total_sleep_bouts_mod )

# tab_model( spatial_total_sleep_bouts_mod )

pp_check( spatial_total_sleep_bouts_mod )


library( ggplot2 )

plot( conditional_effects( spatial_total_sleep_bouts_mod, "density_std" ), theme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) )[[ 1 ]] + labs( x = 'Spatial network density', y = 'Total time spent sleeping (min)' )

plot( conditional_effects( spatial_total_sleep_bouts_mod, "modularity_std" ), theme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) )[[ 1 ]] + labs( x = 'Spatial network modularity', y = 'Total time spent sleeping (min)' )



## black for plotting 

plot( conditional_effects( spatial_total_sleep_bouts_mod, "density_std" ),line_args = list( colour = "white", size = 2 ), errorbar_args = list( colour = "white" ), cat_args = list( colour = "white" ), theme = theme( text = element_text( size = 20 ), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "black", colour = "black"), panel.background = element_rect(fill = "black", colour = "black"), axis.line = element_line(colour = "white"), axis.text = element_text(size = rel(0.8), colour = "white"), axis.ticks = element_line(colour = "white")  ) )[[ 1 ]] + labs( x = 'Spatial network density', y = 'Total time spent sleeping (min)' )

plot( conditional_effects( spatial_total_sleep_bouts_mod, "modularity_std" ), line_args = list( colour = "white", size = 2 ), errorbar_args = list( colour = "white" ), cat_args = list( colour = "white" ), theme = theme( text = element_text( size = 20 ), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "black", colour = "black"), panel.background = element_rect(fill = "black", colour = "black"), axis.line = element_line(colour = "white"), axis.text = element_text(size = rel(0.8), colour = "white"), axis.ticks = element_line(colour = "white")  ) )[[ 1 ]] + labs( x = 'Spatial network modularity', y = 'Total time spent sleeping (min)' )






mod_dat_to_plot <- conditional_effects( spatial_total_sleep_bouts_mod, "density_std" )[[1]]

mod_dat_to_plot$untrans_estimate__ <- ( mod_dat_to_plot$estimate__*sd( mod_dat$total_sleep_bouts, na.rm = T ) + mean( mod_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot$untrans_lower__ <- ( mod_dat_to_plot$lower__*sd( mod_dat$total_sleep_bouts, na.rm = T ) + mean( mod_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot$untrans_upper__ <- ( mod_dat_to_plot$upper__*sd( mod_dat$total_sleep_bouts, na.rm = T ) + mean( mod_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot$untrans_density <- mod_dat_to_plot$density_std * sd( mod_dat$density, na.rm = T ) + mean( mod_dat$density, na.rm = T )


mod_dat_to_plot_2 <- conditional_effects( spatial_total_sleep_bouts_mod, "modularity_std" )[[1]]

mod_dat_to_plot_2$untrans_estimate__ <- ( mod_dat_to_plot_2$estimate__*sd( mod_dat$total_sleep_bouts, na.rm = T ) + mean( mod_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot_2$untrans_lower__ <- ( mod_dat_to_plot_2$lower__*sd( mod_dat$total_sleep_bouts, na.rm = T ) + mean( mod_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot_2$untrans_upper__ <- ( mod_dat_to_plot_2$upper__*sd( mod_dat$total_sleep_bouts, na.rm = T ) + mean( mod_dat$total_sleep_bouts, na.rm = T ) ) / 60

mod_dat_to_plot_2$untrans_modularity <- mod_dat_to_plot_2$modularity_std * sd( mod_dat$modularity, na.rm = T ) + mean( mod_dat$modularity, na.rm = T )


y_lims <- range( c( mod_dat_to_plot$untrans_lower__, mod_dat_to_plot$untrans_upper__, mod_dat_to_plot_2$untrans_lower__, mod_dat_to_plot_2$untrans_upper__ ) )


dev.off()

plot( mod_dat_to_plot$untrans_density, mod_dat_to_plot$untrans_estimate__, type = 'l', xlim = range( mod_dat_to_plot$untrans_density ), ylim = y_lims, bty = 'l', ylab = 'Total time spent sleeping (hours)', xlab = "Density of spatial network" ) 

polygon( x = c( mod_dat_to_plot$untrans_density, rev( mod_dat_to_plot$untrans_density ) ), y = c( mod_dat_to_plot$untrans_lower__, rev( mod_dat_to_plot$untrans_upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot$untrans_density, mod_dat_to_plot$untrans_estimate__, type = 'l', lwd = 2, col = 'blue' )




plot( mod_dat_to_plot_2$untrans_modularity, mod_dat_to_plot_2$untrans_estimate__, type = 'l', xlim = range( mod_dat_to_plot_2$untrans_modularity ), ylim = y_lims, bty = 'l', ylab = 'Total time spent sleeping (hours)', xlab = "Modulartiy of spatial network" ) 

polygon( x = c( mod_dat_to_plot_2$untrans_modularity, rev( mod_dat_to_plot_2$untrans_modularity ) ), y = c( mod_dat_to_plot_2$untrans_lower__, rev( mod_dat_to_plot_2$untrans_upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot_2$untrans_modularity, mod_dat_to_plot_2$untrans_estimate__, type = 'l', lwd = 2, col = 'blue' )




