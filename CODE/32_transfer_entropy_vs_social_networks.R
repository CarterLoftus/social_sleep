
library( ape )
library( statnet )
library( plyr )
library( asnipe )
library( brms )
library( ggplot2 )


## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}


########### network statistics ################

#### aggregated ####

## read in the transfer entropy network 
aggr_te_net <- readRDS( 'DATA/social_data/agg_te_net.rds' ) ### we are going to save this as effective just to make the code below runnable, even through it is not

## read in the co-sitting networks
load( 'DATA/social_data/cosit_2019.RData' )
# cosit_all - overall rate of co-sitting (26x26)
# cosit_daily - day by day rate of co-sitting (26x26x30)
# tgs - tag IDs (26)
# days - dates (30)

## name the dimensions of the co-sitting networks
dimnames( cosit_all ) <- list( tgs, tgs )
dimnames( cosit_daily ) <- list( tgs, tgs, as.character( as.Date( days, tz = 'UTC' ) ) )

## read in the displacement data (this data is from a Keeper link that Roi sent me: https://keeper.mpdl.mpg.de/d/2e876e1c6f8d489a9d56/)
displace_dat <- read.csv( 'DATA/social_data/Displace.csv', header = F )

## turn the displacement data into a matrix
displace_dat <- as.matrix( displace_dat )

rownames( displace_dat ) <- tgs
colnames( displace_dat ) <- tgs


## check that the tags line up in all networks

identical( dimnames( aggr_te_net ), dimnames( cosit_all ) )
identical( dimnames( aggr_te_net ), dimnames( displace_dat ) )
# yup, they line up

###  test whether the displacement network and cositting network predict the transfer entropy network ###

aggr_te_net_std <- ( aggr_te_net - mean( aggr_te_net, na.rm = T ) ) / sd( aggr_te_net, na.rm = T )

cosit_all_std <- ( cosit_all - mean( cosit_all, na.rm = T ) ) / sd( cosit_all, na.rm = T )

displace_dat_std <- ( displace_dat - mean( displace_dat, na.rm = T ) ) / sd( displace_dat, na.rm = T )


set.seed( 111 )

mrqap.dsp( aggr_te_net_std ~ cosit_all_std + displace_dat_std, directed="directed")


#### show effect size with linear model

te_agg_df <- adply( aggr_te_net, c( 1, 2 ) )

names( te_agg_df ) <- c( 'tag_a', 'tag_b', 'eff_te' )

te_agg_df$dy_name <- apply( te_agg_df, 1, FUN = function( x ) paste( sort( c( x[ 'tag_a' ], x[ 'tag_b' ] ) ), collapse = '_' )  )



cosit_df <- adply( cosit_all, c( 1, 2 ) )

names( cosit_df ) <- c( 'tag_a', 'tag_b', 'cosit_score' )



displace_df <- adply( displace_dat, c( 1, 2 ) )

names( displace_df ) <- c( 'tag_a', 'tag_b', 'displace_score' )


te_cosit_df <- merge( x = te_agg_df, y = cosit_df, by = c( 'tag_a', 'tag_b' ), all.x = T, all.y = T, sort = F )

te_disp_cosit_df <- merge( x = te_cosit_df, y = displace_df, by = c( 'tag_a', 'tag_b' ), all.x = T, all.y = T, sort = F )


## function for normalizing a vector
normalize_func <- function( x ) return( (x - mean( x, na.rm = T ) )/ sd( x, na.rm = T ) )

te_disp_cosit_df$cosit_std <- normalize_func( te_disp_cosit_df$cosit_score )

te_disp_cosit_df$displace_std <- normalize_func( te_disp_cosit_df$displace_score )

te_disp_cosit_df$eff_te_std <- normalize_func( te_disp_cosit_df$eff_te )

te_disp_cosit_df

mod_dat <- te_disp_cosit_df[ complete.cases( te_disp_cosit_df ), ]

mod_dat$log_te <- log( mod_dat$eff_te )
mod_dat$log_te_std <- normalize_func( mod_dat$log_te )


options( mc.cores = parallel::detectCores() )



priors <- c(prior(normal(0,2), class = "b", coef = cosit_std),
            prior(normal(0,2), class = "b", coef = displace_std)
)

net_cor_mod <- brm( log_te_std ~ cosit_std + displace_std + ( 1 | tag_a ) + ( 1 | tag_b ) + ( 1 | dy_name ), data = mod_dat, family= "skew_normal", iter = 5000, prior = priors, control = list(max_treedepth = 30, adapt_delta = .999999999999999 ) )

dir.create( paste0( getwd(), '/RESULTS/models/' ) )

saveRDS( net_cor_mod, "RESULTS/models/net_cor_mod.rds" )

net_cor_mod <- readRDS( "RESULTS/models/net_cor_mod.rds" )

summary( net_cor_mod )

pp_check( net_cor_mod )


plot( conditional_effects( net_cor_mod, "cosit_std" ), theme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) )[[ 1 ]] + labs( x = 'Time dyad spent cositting during the day (standardized)', y = "Transfer entropy between dyad's sleep states at night (standardized)") + ylim( c( -.3, 1.7) )




plot( conditional_effects( net_cor_mod, "displace_std" ), theme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) )[[ 1 ]] + labs( x = 'Number of times id A displaced id B during the day', y = "Transfer entropy between dyad's sleep states (id A -> id B) at night\n(standardized)") + ylim( c( -.35, 1.7 ) ) 



## black background

plot( conditional_effects( net_cor_mod, "cosit_std" ), line_args = list( colour = "white", size = 2 ), errorbar_args = list( colour = "white" ), cat_args = list( colour = "white" ), theme = theme( text = element_text( size = 25 ), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "black", colour = "black"), panel.background = element_rect(fill = "black", colour = "black"), axis.line = element_line(colour = "white"), axis.text = element_text(size = rel(0.8), colour = "white"), axis.ticks = element_line(colour = "white")  ) )[[ 1 ]] + labs( x = 'Time dyad spent cositting during the day (standardized)', y = "Transfer entropy between dyad's sleep states at night (standardized)") + ylim( c( -.35, 1.8) )




plot( conditional_effects( net_cor_mod, "displace_std" ), line_args = list( colour = "white", size = 2 ), errorbar_args = list( colour = "white" ), cat_args = list( colour = "white" ), theme = theme( text = element_text( size = 25 ), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "black", colour = "black"), panel.background = element_rect(fill = "black", colour = "black"), axis.line = element_line(colour = "white"), axis.text = element_text(size = rel(0.8), colour = "white"), axis.ticks = element_line(colour = "white")  ) )[[ 1 ]] + labs( x = 'Number of times id A displaced id B during the day', y = "Transfer entropy between dyad's sleep states (id A -> id B) at night\n(standardized)") + ylim( c( -.35, 1.8 ) ) 


# tab_model( net_cor_mod )


mod_dat_to_plot <- conditional_effects( net_cor_mod, "cosit_std" )[[1]]

mod_dat_to_plot$untrans_estimate__ <- exp( mod_dat_to_plot$estimate__*sd( mod_dat$log_te, na.rm = T ) + mean( mod_dat$log_te, na.rm = T ) )

mod_dat_to_plot$untrans_lower__ <- exp( mod_dat_to_plot$lower__*sd( mod_dat$log_te, na.rm = T ) + mean( mod_dat$log_te, na.rm = T ) )

mod_dat_to_plot$untrans_upper__ <- exp( mod_dat_to_plot$upper__*sd( mod_dat$log_te, na.rm = T ) + mean( mod_dat$log_te, na.rm = T ) )



mod_dat_to_plot_2 <- conditional_effects( net_cor_mod, "displace_std" )[[1]]

mod_dat_to_plot_2$untrans_estimate__ <- exp( mod_dat_to_plot_2$estimate__*sd( mod_dat$log_te, na.rm = T ) + mean( mod_dat$log_te, na.rm = T ) )

mod_dat_to_plot_2$untrans_lower__ <- exp( mod_dat_to_plot_2$lower__*sd( mod_dat$log_te, na.rm = T ) + mean( mod_dat$log_te, na.rm = T ) )

mod_dat_to_plot_2$untrans_upper__ <- exp( mod_dat_to_plot_2$upper__*sd( mod_dat$log_te, na.rm = T ) + mean( mod_dat$log_te, na.rm = T ) )


y_lims <- range( c( mod_dat_to_plot$lower__, mod_dat_to_plot$upper__, mod_dat_to_plot_2$lower__, mod_dat_to_plot_2$upper__ ) )


plot( mod_dat_to_plot$cosit_std, mod_dat_to_plot$estimate__, type = 'l', xlim = range( mod_dat_to_plot$cosit_std ), ylim = y_lims, bty = 'l', xlab = 'Dyad affiliative score (standardized)', ylab = "Log transfer entropy between dyad's sleep states at night (standardized)" ) 

polygon( x = c( mod_dat_to_plot$cosit_std, rev( mod_dat_to_plot$cosit_std ) ), y = c( mod_dat_to_plot$lower__, rev( mod_dat_to_plot$upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot$cosit_std, mod_dat_to_plot$estimate__, type = 'l', lwd = 2, col = 'blue' )




plot( mod_dat_to_plot_2$displace_std, mod_dat_to_plot_2$estimate__, type = 'l', xlim = range( mod_dat_to_plot_2$displace_std ), ylim = y_lims, bty = 'l', xlab = 'Dyad dominance score (directed; standardized)', ylab = "Log transfer entropy between dyad's sleep states at night (standardized)" ) 

polygon( x = c( mod_dat_to_plot_2$displace_std, rev( mod_dat_to_plot_2$displace_std ) ), y = c( mod_dat_to_plot_2$lower__, rev( mod_dat_to_plot_2$upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot_2$displace_std, mod_dat_to_plot_2$estimate__, type = 'l', lwd = 2, col = 'blue' )


summary( net_cor_mod )

TE_least_affil <- exp( ( ( min( mod_dat$cosit_std ) * 0.11 + ( -0.04 ) ) * sd( mod_dat$log_te, na.rm = T ) ) + mean( mod_dat$log_te, na.rm = T ) )

TE_most_affil <- exp( ( ( max( mod_dat$cosit_std ) * 0.11 + ( -0.04 ) ) * sd( mod_dat$log_te, na.rm = T ) ) + mean( mod_dat$log_te, na.rm = T ) )

TE_most_affil / TE_least_affil


TE_least_dom <- exp( ( ( min( mod_dat$displace_std ) * 0.07 + ( -0.04 ) ) * sd( mod_dat$log_te, na.rm = T ) ) + mean( mod_dat$log_te, na.rm = T ) )

TE_most_dom <- exp( ( ( max( mod_dat$displace_std ) * 0.07 + ( -0.04 ) ) * sd( mod_dat$log_te, na.rm = T ) ) + mean( mod_dat$log_te, na.rm = T ) )

TE_most_dom / TE_least_dom


### if you want to plot untransformed:

plot( mod_dat_to_plot$cosit_std, mod_dat_to_plot$untrans_estimate__, type = 'l', xlim = range( mod_dat_to_plot$cosit_std ), ylim = range( c( mod_dat_to_plot$untrans_lower__, mod_dat_to_plot$untrans_upper__  ) ), bty = 'l', xlab = 'Proportion of time dyad spent co-sitting (standardized)', ylab = "Transfer entropy between dyad's sleep states at night (standardized)" ) 

polygon( x = c( mod_dat_to_plot$cosit_std, rev( mod_dat_to_plot$cosit_std ) ), y = c( mod_dat_to_plot$untrans_lower__, rev( mod_dat_to_plot$untrans_upper__ ) ), border = F, col = transp( 'grey' ) )

lines( mod_dat_to_plot$cosit_std, mod_dat_to_plot$untrans_estimate__, type = 'l', lwd = 2, col = 'blue' )


