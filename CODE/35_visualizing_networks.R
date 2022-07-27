########### Visualizing networks ################

library( stringr )

#### aggregated ####

## read in the transfer entropy network 
agg_eff_te_net <- readRDS( 'DATA/social_data/agg_te_net.rds' ) ### we are going to save this as effective just to make the code below runnable, even through it is not

## agg_eff_te_net <- readRDS( 'DATA/social_data/agg_eff_te_net.rds' ) ### this would be the effective transfer entropy network, if you choose to use it


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

displace_net <- displace_dat - t( displace_dat )

displace_net[ displace_net < 0 ] <- 0

## check that the tags line up in all networks
identical( dimnames( agg_eff_te_net ), dimnames( cosit_all ) )
identical( dimnames( agg_eff_te_net ), dimnames( displace_dat ) )
# yup, they line up


night_to_plot <- "2019-08-01"

total_dat <- readRDS( "DATA/sleep_analysis_data/total_dat.rds")

night_dat <- total_dat[ total_dat$night == night_to_plot & ( total_dat$local_time > "21:00:00" | total_dat$local_time < "05:00:00" ), rownames( cosit_all ) ]

tags_to_keep <- names( night_dat )[ apply( night_dat, 2, function( x ) sum( !is.na( x ) ) > 0 ) ]


night_dat_trim <- night_dat[ , tags_to_keep ]


# extract the indices of the NAs
na_inds <- which( is.na( night_dat_trim ), arr.ind = T )

# this is pathetic, but I can't really think of a better way than going through fixing these one at a time
for( i in 1:nrow( na_inds ) ){ # for each NA...
  
  # replace the NA with the previously measured sleep state (because we are going through this in order, if the previous timestamp also had an NA, it will be have been filled in with the sleep state prior to that timestamp before we get to the current timestamp; so we don't have to worry about this situation)
  
  if( na_inds[ i, 1 ] == 1 ){
    
    
    night_dat_trim[ na_inds[ i , 1 ], na_inds[ i, 2 ] ] <- 0
    
  }else{
    
    night_dat_trim[ na_inds[ i, 1 ], na_inds[ i, 2 ] ] <- night_dat_trim[ na_inds[ i, 1 ] - 1, na_inds[ i, 2 ] ]
    
  }
}

sum( is.na( night_dat_trim ) ) # confirming that there are now no NA's


agg_eff_te_net <- agg_eff_te_net[ tags_to_keep, tags_to_keep ]
cosit_all <- cosit_all[ tags_to_keep, tags_to_keep ]
displace_net <- displace_net[ tags_to_keep, tags_to_keep ]

quantile_cutoff <- 0.8

agg_eff_te_net_trimmed <- agg_eff_te_net

agg_eff_te_net_trimmed[ agg_eff_te_net_trimmed < quantile( agg_eff_te_net_trimmed, quantile_cutoff, na.rm = T ) ] <- 0

displace_net_trimmed <- displace_net

displace_net_trimmed[ displace_net_trimmed < quantile( displace_net_trimmed, quantile_cutoff, na.rm = T ) ] <- 0

cosit_all_trimmed <- cosit_all

cosit_all_trimmed[ cosit_all_trimmed < quantile( cosit_all_trimmed, quantile_cutoff, na.rm = T ) ] <- 0


library( igraph )


transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}


g_te <- graph_from_adjacency_matrix( agg_eff_te_net_trimmed, mode = 'directed', weighted = T, diag = F, add.rownames = T )

g_dom <- graph_from_adjacency_matrix( displace_net_trimmed, mode = 'directed', weighted = T, diag = F, add.rownames = T )

g_cosit <- graph_from_adjacency_matrix( cosit_all_trimmed, mode = 'undirected', weighted = T, diag = F, add.rownames = T )


l <- layout_with_fr( g_dom )


sleep_state_vec <- rbinom( nrow( displace_net ) , 1, 0.2 )

library( animation )


saveGIF(
  {

    par( mfrow = c( 1, 2 ) )

    par(mar = c( 0, 0, 1, 0), bg = "white" )

    for( i in 1: nrow( night_dat_trim ) ){

      sleep_state_vec <- night_dat_trim[ i, ]

      plot( g_cosit, layout = l, vertex.color = ifelse( as.logical( sleep_state_vec ), "white", transp( 'white', 0.5 ) ), vertex.label = "", edge.color = "#e99588", edge.width = ( E(g_cosit)$weight )*6, main = paste( 'Cositting network', i )  )

      plot( g_dom - E( g_dom )[ E( g_dom )$weight == 0 ], layout = l, vertex.color = ifelse( as.logical( sleep_state_vec ), "white", transp( 'white', 0.5 ) ), vertex.label = "", edge.color = "#bd92de", edge.width = ( E(g_cosit)$weight )*6, edge.arrow.size = 1.4, edge.curved = T, main = paste( "Dominance network", i ) )


    }
  },

  movie.name = "dynamic_nets.gif",
  interval = 0.2,
  ani.width = 1000,
  ani.height = 1000,
  outdir = paste0( getwd(), "/RESULTS/" )


)



### black background


saveGIF(
  {
    
    par( mfrow = c( 1, 2 ) )
    
    par(mar = c( 0, 0, 1, 0), bg = "black" )
    
    for( i in 1: nrow( night_dat_trim ) ){
      
      print( i / nrow( night_dat_trim ) )
      sleep_state_vec <- night_dat_trim[ i, ]
      
      plot( g_cosit, layout = l, vertex.color = 'black', vertex.label = "", edge.color = "#e99588", edge.width = ( E(g_cosit)$weight )*6, main = paste( 'Cositting network', i )  )
      
      plot( g_cosit - E( g_cosit ), layout = l, vertex.color = ifelse( as.logical( sleep_state_vec ), "white", transp( 'white', 0.5 ) ), vertex.label = "", edge.width = -1, main = paste( 'Cositting network', i ), add = T  )
      
      plot( g_dom - E( g_dom )[ E( g_dom )$weight == 0 ], layout = l, vertex.color = 'black', vertex.label = "", edge.color = "#bd92de", edge.width = ( E(g_cosit)$weight )*6, edge.arrow.size = 1.4, edge.curved = T, main = paste( "Dominance network", i ) )
      
      plot( g_dom - E( g_dom ), layout = l, vertex.color = ifelse( as.logical( sleep_state_vec ), "white", transp( 'white', 0.5 ) ), vertex.label = "", edge.width = -1, edge.curved = T, main = paste( "Dominance network", i ), add = T )
      
    }
  },
  
  movie.name = "dynamic_nets_black_bg.gif",
  interval = 0.2,
  ani.width = 1000,
  ani.height = 1000,
  outdir = paste0( getwd(), "/RESULTS/" )
  
  
)




### make a supradjacency matrix

########### multi-plex network visualization ################

library(igraph)
library(graphlayouts) 
library(ggraph)
library(threejs)

#### aggregated ####

## read in the transfer entropy network 
agg_eff_te_net <- readRDS( 'DATA/social_data/agg_te_net.rds' ) ### we are going to save this as effective just to make the code below runnable, even through it is not## agg_eff_te_net <- readRDS( 'DATA/social_data/agg_eff_te_net.rds' ) ### this would be the effective transfer entropy network, if you choose to use it


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

displace_net <- displace_dat - t( displace_dat )

displace_net[ displace_net < 0 ] <- 0


## check that the tags line up in all networks
identical( dimnames( agg_eff_te_net ), dimnames( cosit_all ) )
identical( dimnames( agg_eff_te_net ), dimnames( displace_dat ) )
# yup, they line up


supra_mat <- matrix( NA, nrow = nrow( agg_eff_te_net ) * 3, ncol = ncol( agg_eff_te_net ) * 3 ) 

rownames( supra_mat ) <- paste( rep( c( 'cosit', 'TE', 'dom' ), each = nrow( agg_eff_te_net ) ), rep( rownames( agg_eff_te_net ), times = 3 ), sep = '_' )

colnames( supra_mat ) <- paste( rep( c( 'cosit', 'TE', 'dom' ), each = nrow( agg_eff_te_net ) ), rep( rownames( agg_eff_te_net ), times = 3 ), sep = '_' )


net_agg_eff_te_net <- agg_eff_te_net - t( agg_eff_te_net )

net_agg_eff_te_net[ net_agg_eff_te_net < 0 ] <- 0


#### now going to trim #####


quantile_cutoff <- 0.85

agg_eff_te_net_trimmed <- agg_eff_te_net

agg_eff_te_net_trimmed[ agg_eff_te_net_trimmed < quantile( agg_eff_te_net_trimmed, quantile_cutoff, na.rm = T ) ] <- 0

displace_dat_trimmed <- displace_dat

displace_dat_trimmed[ displace_dat_trimmed < quantile( displace_dat_trimmed, quantile_cutoff, na.rm = T ) ] <- 0

cosit_all_trimmed <- cosit_all

cosit_all_trimmed[ cosit_all_trimmed < quantile( cosit_all_trimmed, quantile_cutoff, na.rm = T ) ] <- 0





supra_mat[ grepl( 'cosit', rownames( supra_mat ) ), grepl( 'cosit', colnames( supra_mat ) ) ] <- cosit_all_trimmed


supra_mat[ grepl( 'TE', rownames( supra_mat ) ), grepl( 'TE', colnames( supra_mat ) ) ] <- agg_eff_te_net_trimmed


supra_mat[ grepl( 'dom', rownames( supra_mat ) ), grepl( 'dom', colnames( supra_mat ) ) ] <- displace_dat_trimmed

all_tag_ids <- str_split_fixed( colnames( supra_mat ), '_', 2 )[ , 2 ]

for( i in 1:ncol( supra_mat ) ){
  
  tag_id <- str_split_fixed( colnames( supra_mat )[ i ], '_', 2 )[ , 2 ]
  
  row_inds <- which( all_tag_ids %in% tag_id )
  
  supra_mat[ row_inds, i ] <- 1
}

supra_mat[ is.na( supra_mat ) ] <- 0

g <- graph_from_adjacency_matrix( supra_mat, mode = 'directed', weighted = T, diag = F, add.colnames = T )


nets <- str_split_fixed( rownames( supra_mat ), '_', 2 )[ , 1 ]

V( g )$lvl <- as.numeric( factor( nets, levels = c( 'cosit', 'TE', 'dom' ) ) ) 

V( g )$name <- rownames( supra_mat )

V( g )$layer <- nets

#V( g )$lvl[ V(g )$lvl == 3 ] <- 2

xyz <- layout_as_multilevel(g,type = "separate",
                            FUN1 = layout_with_stress,
                            FUN2 = layout_with_stress,
                            project2D = FALSE)

ignore_iso <- T

lvl1 <- which(igraph::V(g)$lvl == 1)
lvl2 <- which(igraph::V(g)$lvl == 2)
lvl3 <- which(igraph::V(g)$lvl == 3)

g2 <- igraph::induced_subgraph(g, lvl2)

if (ignore_iso) {
  iso2 <- which(igraph::degree(g2) == 0)
  g2 <- igraph::delete.vertices(g2, iso2)
}

xy2 <- layout_with_stress( g2, weights = NA )




xyz <- cbind(0, igraph::V(g)$lvl , 0)

xy2[, 1] <- graphlayouts:::normalise(xy2[, 1], to = c(1, 2))
xy2[, 2] <- graphlayouts:::normalise(xy2[, 2], to = c(1, 2))

#xy1 <- graphlayouts:::optim_level(g, 2, xy2)


xyz[lvl1, c(1, 3)] <- xy2
xyz[lvl2, c(1, 3)] <- xy2
xyz[lvl3, c(1, 3)] <- xy2



g$layout <- xyz
V(g)$color <- c("#00BFFF", "#FF69B4", "#00BFFF" )[V(g)$lvl]
V(g)$vertex.label <- V(g)$name

graphjs(g, bg="black", vertex.shape="sphere" )

## give edges and nodes color attributes


target <- V(g)$name[ V(g)$layer == 'cosit' ]
target <- as_edgelist(induced.subgraph(g, target))
cosit_edges <- apply(target, 1, function(z) paste(z, collapse = "|"))

target <- V(g)$name[ V(g)$layer == 'TE' ]
target <- as_edgelist(induced.subgraph(g, target))
TE_edges <- apply(target, 1, function(z) paste(z, collapse = "|"))


target <- V(g)$name[ V(g)$layer == 'dom' ]
target <- as_edgelist(induced.subgraph(g, target))
dom_edges <- apply(target, 1, function(z) paste(z, collapse = "|"))



#### now in 2D with edge weights

cols2 <- c("#3A5FCD", "#CD00CD", "#EE30A7", "#EE6363", 
           "#CD2626", "#458B00", "#EEB422", "#EE7600")




alpha <- 40
beta <- 225

xy <- graphlayouts:::iso_project(xyz, a = alpha, b = beta)


ggraph(g, "manual", x = xy[, 1], y = xy[, 2]) +
  
  geom_edge_link0(
    aes(
      filter = (node1.lvl == 1 & node2.lvl == 1),
      edge_colour = '1'
    ),
    edge_colour = 'white',
    alpha = 0.5, 
    edge_width = E( g )[ cosit_edges ]$weight * 2 
  ) +
  
  geom_edge_link0(
    aes(
      filter = (node1.lvl != node2.lvl)
    ),
    edge_width = 1,
    edge_colour = 'white',
    alpha = 0.3,
  ) +
  
  geom_edge_fan0(
    aes(
      filter = (node1.lvl == 2 & node2.lvl == 2),
      edge_colour = '2'
    ),
    arrow = arrow( length = unit(8, 'mm') ),
    edge_width = rep( E( g )[ TE_edges ]$weight*800, 4 ), 
    alpha = 0.5 ) +
  
  geom_edge_fan0(
    aes(
      filter = (node1.lvl == 3 & node2.lvl == 3),
      edge_colour = '3'
    ),
    arrow = arrow(length = unit(8, 'mm') ),
    edge_width = rep( E( g )[ dom_edges ]$weight*120, 4 ), 
    alpha = 0.5 ) +
  
  # geom_node_point(aes(
  #   fill = as.factor(lvl),
  #   size = 1,
  # ),
  # alpha = 1,
  # color = 'black'
  # ) +
  # 
  geom_node_point(aes(
    fill = as.factor(lvl),
    size = 1,
  ),
  alpha = 
    .7,
  color = 'white'
  ) +
  #scale_fill_manual(values = c("#3A5FCD", "#CD00CD", "#EE30A7" ) )+
  theme_graph(
    background = 'black'
  ) +
  #coord_cartesian(clip = "off", expand = TRUE) +
  theme( legend.position = "none" )





library( animation )


saveGIF(
  {
    
    alpha <- 40
    
    betas <- seq( 1, 360, by = 1 )
    
    for( beta in betas ){
      
      print( beta )
      xy <- graphlayouts:::iso_project(xyz, a = alpha, b = beta)
      
      
      p <- ggraph(g, "manual", x = xy[, 1], y = xy[, 2]) +
        
        geom_edge_link0(
          aes(
            filter = (node1.lvl == 1 & node2.lvl == 1)
          ),
          edge_colour = "#e99588",
          alpha = 0.5, 
          edge_width = E( g )[ cosit_edges ]$weight * 2 
        ) +
        
        geom_edge_link0(
          aes(
            filter = (node1.lvl != node2.lvl)
          ),
          edge_width = 0.8,
          edge_colour = 'white',
        ) +
        
        geom_edge_fan0(
          aes(
            filter = (node1.lvl == 2 & node2.lvl == 2),
          ),
          edge_colour = "#adc857",
          arrow = arrow( length = unit(10, 'mm') ),
          edge_width = rep( E( g )[ TE_edges ]$weight*800, 4 ), 
          alpha = 0.5 ) +
        
        geom_edge_fan0(
          aes(
            filter = (node1.lvl == 3 & node2.lvl == 3),
          ),
          edge_colour = "#bd92de",
          arrow = arrow(length = unit(10, 'mm') ),
          edge_width = rep( E( g )[ dom_edges ]$weight*120, 4 ), 
          alpha = 0.5 ) +
        
        geom_node_point(aes(
          fill = as.factor(lvl),
        ),
        size = 12,
        colour = 'white',
        alpha = 0.7
        ) +
        #scale_fill_manual(values = c("#3A5FCD", "#CD00CD", "#EE30A7" ) )+
        theme_graph(
          background = 'black'
        ) +
        #coord_cartesian(clip = "off", expand = TRUE) +
        theme( legend.position = "none" )
      
      plot( p )
    }
  }
  ,
  
  movie.name = "rotating_multiplex_large.gif",
  interval = 0.05,
  ani.width = 2000,
  ani.height = 2000,
  outdir = paste0( getwd(), "/RESULTS/" )
)



library( animation )


saveGIF(
  {
    
    alpha <- 40
    
    betas <- seq( 1, 360, by = 1 )
    
    for( beta in betas ){
      
      print( beta )
      xy <- graphlayouts:::iso_project(xyz, a = alpha, b = beta)
      
      
      p <- ggraph(g, "manual", x = xy[, 1], y = xy[, 2]) +
        
        geom_edge_link0(
          aes(
            filter = (node1.lvl == 1 & node2.lvl == 1)
          ),
          edge_colour = "#e99588",
          alpha = 0.5, 
          edge_width = E( g )[ cosit_edges ]$weight * 1
        ) +
        
        geom_edge_link0(
          aes(
            filter = (node1.lvl != node2.lvl)
          ),
          edge_width = ( 0.8/3 ),
          edge_colour = 'white',
        ) +
        
        geom_edge_fan0(
          aes(
            filter = (node1.lvl == 2 & node2.lvl == 2),
          ),
          edge_colour = "#adc857",
          arrow = arrow( length = unit(3.5, 'mm') ),
          edge_width = rep( E( g )[ TE_edges ]$weight*400, 4 ), 
          alpha = 0.5 ) +
        
        geom_edge_fan0(
          aes(
            filter = (node1.lvl == 3 & node2.lvl == 3),
          ),
          edge_colour = "#bd92de",
          arrow = arrow(length = unit(3.5, 'mm') ),
          edge_width = rep( E( g )[ dom_edges ]$weight*60, 4 ), 
          alpha = 0.5 ) +
        
        geom_node_point(aes(
          fill = as.factor(lvl),
        ),
        size = 4,
        colour = 'white',
        alpha = 0.7
        ) +
        #scale_fill_manual(values = c("#3A5FCD", "#CD00CD", "#EE30A7" ) )+
        theme_graph(
          background = 'black'
        ) +
        #coord_cartesian(clip = "off", expand = TRUE) +
        theme( legend.position = "none" )
      
      plot( p )
    }
  }
  ,
  
  movie.name = "rotating_multiplex_small.gif",
  interval = 0.05,
  ani.width = 500,
  ani.height = 500,
  outdir = paste0( getwd(), "/RESULTS/" )
)



