# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !  #
# This is the REVISION syntax based on reviewer comments.
# ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !  #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ############################################################################################# #
# This code creates the agreement matrices for the inmate code and race items of the NIJ project.
# ############################################################################################# #

# This is the REVISED syntax based on reviewer comments.

# ================================================================== #
# Clear the workspace and load the libraries.

rm( list = ls() )

# ================================================================== #
# Load the data.

setwd( "/Users/jyoung20/Dropbox (ASU)/Living and Working in Max. (NIJ 2017)/NIJ_LWMAX_Data_Syntax/NIJ_LWMAX_CCA_PROJECT/NIJ_LWMAX_CCA_PROJECT_data/" )
load( "prisoner_racial_code_recoded_REVISION.R" )


# ================================================================== #
# Function to create the agreement matrix.
# The equation comes from pg. 65 of Purzycki, B. G., & Jamieson-Lane, A. (2016). AnthroTools. Cross-Cultural Research, 51(1), 51-74. doi:10.1177/1069397116680352.

DiscountedAgreementMatrix <-
  function( response.mat,n.responses ){
    discounted.matrix= matrix( 0, nrow( response.mat ),nrow( response.mat ) )
    for( i in 1:nrow( discounted.matrix ) ){
      for( j in 1:ncol( discounted.matrix ) ){
        discounted.matrix[i,j] <- mean( response.mat[i,] == response.mat[j,], na.rm = TRUE ) # Create the average number of items i and j agree on.
      }             
    }
    discounted.matrix <- ( discounted.matrix*n.responses -1 ) / (n.responses-1 ) # Correct the average agreement for guessing.
    diag( discounted.matrix ) <- 0 # set the diagonal to zero.
    discounted.matrix[is.finite(discounted.matrix) == FALSE] <- 0 # adjust Inf values to zero (if any).
    return( discounted.matrix ) 
  }

codes.d.agreement   <- DiscountedAgreementMatrix( p.code[,-1], 3 )
race.d.agreement    <- DiscountedAgreementMatrix( r.code[,-1], 3 )

mean( codes.d.agreement )
mean( race.d.agreement )


# ================================================================== #
# Plotting results.

library( reshape2 )
library( ggplot2 )

dat.for.plot <- function( cormat ){
  dd <- as.dist((1-cormat)/2) # Use correlation between variables as distance.
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
  cormat[lower.tri(cormat)] <- NA # set the lower part of the triangle as missing (redundant information).
  cor.dat <- melt( cormat, na.rm = TRUE ) # create the dataframe.
  return( cor.dat )
}

# assign the agreement matrix to the object to plot.
codes.agreement.dat <- dat.for.plot( codes.d.agreement )
race.agreement.dat <- dat.for.plot( race.d.agreement )


# Plot for prisoner code.
ggheatmap.PC <- ggplot( data = codes.agreement.dat, aes( Var2, Var1, fill = value ) ) +
  geom_tile( color = "white" ) +
  scale_fill_gradient2( low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c( -1,1 ), space = "Lab", 
                        name="Agreement" ) +
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
                               title.position = "top", title.hjust = 0.5) ) +
  ggtitle( "Panel A: Plot of Agreement for Prisoner Code Items" )
print( ggheatmap.PC )

# Plot for racial code.
ggheatmap.RC <- ggplot( data = race.agreement.dat, aes( Var2, Var1, fill = value ) ) +
  geom_tile( color = "white" ) +
  scale_fill_gradient2( low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c( -1,1 ), space = "Lab", 
                        name="Agreement" ) +
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
                                title.position = "top", title.hjust = 0.5) ) +
  ggtitle( "Panel B: Plot of Agreement for Racial Code Items" )
print( ggheatmap.RC )


# ================================================================== #
# Save the file.

setwd( "/Users/jyoung20/Dropbox (ASU)/Living and Working in Max. (NIJ 2017)/NIJ_LWMAX_Data_Syntax/NIJ_LWMAX_CCA_PROJECT/NIJ_LWMAX_CCA_PROJECT_data/" )
save.image( "prisoner_racial_code_recoded_mats_REVISION.R" )


# ############################################################################################# #
# END OF SYNTAX FILE.
# ############################################################################################# #