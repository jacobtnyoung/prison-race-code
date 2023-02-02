# ############################################################################################# #
# This code uses the corclass package to create plots of the schema for the inmate code and race items of the NIJ project.
# ############################################################################################# #

# ================================================================== #
# Clear the workspace and load the libraries.

rm( list = ls() )

set.seed( 12345 )

# ================================================================== #
# Load the data.

load( here::here( "prisoner_racial_code_corclass_REVISION.R" ) )


# ================================================================== #
# Plotting the modules.

library( corclass )
library( sna )
library( network )


# ================================================================== #
# Create the functions and objects to plot.

# ++++++++++++++++++++++++++++++ # 
# Create the list of the modules.
cor.list <- NULL
cutoff <- 0.20 # cutoff value for correlations in the graph.
n.modules <- length( c.code.cor$modules )
for( i in 1:n.modules ){
  cor.list[[i]] <- c.code.cor$modules[[i]]$cormat
  cor.list[[i]][abs(cor.list[[i]]) < cutoff] <- 0
}


# ++++++++++++++++++++++++++++++ # 
# Create a function to create the network objects. 
cor.to.net.create <- function( cormat ){
  cor.net <- as.network( cormat, directed = FALSE, 
                         ignore.eval = FALSE, 
                         names.eval = "cor" ) # create the network.
  return( cor.net )
}

# Create the list of networks.
net.list <- NULL
for(i in 1:n.modules ){
  net.list[[i]] <- cor.to.net.create( cor.list[[i]] )
}


# ++++++++++++++++++++++++++++++ # 
# Create a function to rescale the width of the edges.
edge.rescale <- function( edges, low, high ){
  min_w <- min( edges )
  max_w <- max( edges )
  rscl <- ( ( high-low )  * ( edges - min_w ) ) / ( max_w - min_w ) + low
  rscl
}


# ++++++++++++++++++++++++++++++ # 
# Create a function to shade the edges with darker being larger correlations.
edge.col.create <- function( net.edges ){
  vec.to.color <- as.vector( abs( net.edges ) )
  vec.to.color <- 1 - vec.to.color # subtract 1 to flip the grey function scale.
  edge.cols <- grey( vec.to.color )
  return( edge.cols )
}

# Create a list of edges for edge shadings.
edge.col.list <- NULL
n.modules <- length( c.code.cor$modules )
for( i in 1:n.modules ){
  edge.col.list[[i]] <- edge.col.create( 
    as.edgelist(net.list[[i]], attrname = "cor")[,3] 
  ) 
}


# ++++++++++++++++++++++++++++++ # 
# Create a function to create the edge types for the plots.
edge.type.create <- function( vect, type1 = 1, type2 = 3 ){
  edge.type <- vect
  edge.type[ vect > 0 ]   <- type1
  edge.type[ vect < 0 ]   <- type2
  return( edge.type )
}

# Create a list of edge types.
edge.type.list <- NULL
n.modules <- length( c.code.cor$modules )
for( i in 1:n.modules ){
  edge.type.list[[i]] <- edge.type.create( as.edgelist(net.list[[i]], attrname = "cor")[,3] )
}



# ++++++++++++++++++++++++++++++ # 
# Function to make edges less than a set value nearly white.

edge.col.r.create <- function( net.edges, cutoff ){
  vec.to.color <- as.vector( abs( net.edges ) )
  vec.to.color[vec.to.color < cutoff] <- 
    min( abs( net.edges[net.edges != 0] ) ) # those less than the cutoff set to the smallest value.
  vec.to.color <- 1 - vec.to.color # subtract 1 to flip the grey function scale.
  edge.cols <- grey( vec.to.color )
  edge.cols <- matrix( edge.cols, nrow = dim( net.edges )[1] )
  return( edge.cols )
}

# Create a list of edge shadings.
edge.col.r.list <- NULL
n.modules <- length( c.code.cor$modules )
for (i in 1:n.modules ){
  edge.col.r.list[[i]] <- edge.col.r.create( 
    as.sociomatrix( net.list[[i]], attrname = "cor" ),
    cutoff = 0.3
  )
}


# ++++++++++++++++++++++++++++++ # 
# Create a function to create the edge types for the plots.
edge.type.create <- function( cormat, type1 = 1, type2 = 3 ){
  edge.type <- cormat
  edge.type[ cormat > 0 ]   <- type1
  edge.type[ cormat < 0 ]   <- type2
  return( edge.type )
}

# Create a list of edge types.
edge.type.list <- NULL
n.modules <- length( c.code.cor$modules )
for (i in 1:n.modules ){
  edge.type.list[[i]] <- edge.type.create( cor.list[[i]] )
}


# ================================================================== #
# Plot the networks.

net.cords <- gplot( net.list[[1]], mode = "circle", gmode = "graph" ) 

op <- par( mai = c( 0.01,0.01,0.01,0.01 ), omi = c( 0.1,0.1,0.1,0.1 ), mfrow=c( 2,2 ) )

for( i in 1:n.modules ){
  gplot( net.list[[i]],
         # Edges.
         edge.lwd = edge.rescale( cor.list[[i]], 0.1, 5),
         edge.lty = edge.type.list[[i]],
         edge.col = edge.col.list[[i]],
         
         # Vertices.
         vertex.col = c(
           rep( rgb( 0.2, 0.4, 0.2, 0.6 ), length.out = length( p.names ) ),
           rep( rgb( 0.9, 0.2, 0.2, 0.6 ), length.out = length( r.names ) ) 
         ),
         vertex.cex = rowMeans( abs( as.sociomatrix( net.list[[i]], attrname = "cor" ) ) )*6,
         
         # Labels.
         label = network.vertex.names( net.list[[1]] ),
         label.pos = 5,
         label.cex = 1,
         
         # Misc stuff.
         gmode = "graph"
  )
}

par( op )

# Report the average correlation for each network.
mean( abs( as.sociomatrix( net.list[[1]], attrname = "cor" ) ) )
mean( abs( as.sociomatrix( net.list[[2]], attrname = "cor" ) ) )
mean( abs( as.sociomatrix( net.list[[3]], attrname = "cor" ) ) )
mean( abs( as.sociomatrix( net.list[[4]], attrname = "cor" ) ) )

# Report the mean correlation for each item for the table.
round( apply( c.code.cor$modules[[1]]$cormat, 2, mean ), 2 )
round( apply( c.code.cor$modules[[2]]$cormat, 2, mean ), 2 )
round( apply( c.code.cor$modules[[3]]$cormat, 2, mean ), 2 )
round( apply( c.code.cor$modules[[4]]$cormat, 2, mean ), 2 )



# ================================================================== #
# Plotting Correlations for the full plot.

library( reshape2 )
library( ggplot2 )
library( ggraph )
library( gridExtra )
library( RColorBrewer )
library( ggcorrplot )

# Write a function to convert the correlation matrix.
dat.for.plot <- function( cormat ){
  dd <- as.dist( ( 1-cormat )/2 ) # Use correlation between variables as distance.
  hc <- hclust( dd )
  cormat <-cormat[hc$order, hc$order]
  cormat[lower.tri( cormat )] <- NA # set the lower part of the triangle as missing (redundant information).
  cor.dat <- melt( cormat, na.rm = TRUE ) # create the dataframe.
  return( cor.dat )
}

# assign the agreement matrix to the object to plot.
code.cor.dat <- dat.for.plot( c.mat )


# ++++++++++++++++++++++++++++++ # 
# All items.
code.heatmap <- ggplot( data = code.cor.dat, aes( Var2, Var1, fill = value ) ) +
  geom_tile( color = "white" ) +
  scale_fill_gradient2( high = "red", mid = "white", 
                        midpoint = 0, limit = c( 0,1 ), space = "Lab", 
                        name="Correlation" ) +
  theme_minimal() + 
  coord_fixed() + 
  theme(
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar( barwidth = 7, barheight = 1,
                                title.position = "top", title.hjust = 0.5) ) #+
  #ggtitle( "Correlations among Respondents\n for Prison & Racial Code Items" )
print( code.heatmap )


# ================================================================== #
# Group plot for all items.

# ++++++++++++++++++++++++++++++ # 
# assign colors to the groups.
library( RColorBrewer )
c.colors  <- brewer.pal( length( unique( c.code.cor$membership ) ), "Set3" )[c.code.cor$membership]
c.net %v% "member.color" <- c.colors  # assign the colors as a network object.

# color the edges.
# Create a list of matrices of edge shadings.
edge.cols <- edge.col.create( c.mat )
edge.cols.r <- edge.col.r.create( c.mat, cutoff = 0.75 )

#delete.vertices( c.net, c( 3, 48, 18, 109 ) ) # several nodes are pushed out and get removed here.
op <- par( mai = c( 0.2,0.2,0.8,0.2 ), omi = c( 0.1,0.1,0.1,0.1 ) )
set.seed( 081922 )
gplot( c.net,
       edge.col = edge.cols,
       #edge.col = edge.cols.r,
       edge.lwd = edge.rescale( c.mat, 0.01, 2.5 ),
       vertex.col = c.net %v% "member.color",
       gmode = "graph"
       
)
par( op )

