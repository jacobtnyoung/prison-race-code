# ############################################################################################# #
# This creates an example data set for the NIJ culture paper.
# ############################################################################################# #

# ================================================================== #
# Clear the workspace and load the libraries.

rm( list = ls() )
set.seed( 12345 )


# ================================================================== #
# Create the example data.

n <- 5
items <- 7

ex.1 <- matrix( 
  c(
    3,3,1,2,1,3,1,
    3,3,1,2,1,3,1,
    3,3,1,2,1,3,1,
    3,3,1,2,1,3,1,
    3,3,1,2,1,3,1 ),
  nrow = n, byrow = TRUE )
colnames( ex.1 ) <- c( "I1","I2","I3","I4","I5","I6","I7" )

ex.2 <- matrix( 
  c(
    3,3,1,1,1,3,2,
    3,3,1,1,1,3,2,
    3,3,1,1,1,3,2,
    2,1,3,3,3,1,1,
    2,1,3,3,3,1,1 ),
  nrow = n, byrow = TRUE )
colnames( ex.2 ) <- c( "I1","I2","I3","I4","I5","I6","I7" )

ex.3 <- matrix( 
  c(
    3,3,1,1,3,3,3,
    3,3,1,2,3,3,2,
    3,2,2,1,2,3,2,
    3,2,2,1,1,3,2,
    1,2,2,3,1,1,3 ),
  nrow = n, byrow = TRUE )
colnames( ex.3 ) <- c( "I1","I2","I3","I4","I5","I6","I7" )


# ================================================================== #
# Create the agreement matrices.

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

ex.1.agree <- DiscountedAgreementMatrix( ex.1, 3 )
ex.2.agree <- DiscountedAgreementMatrix( ex.2, 3 )
ex.3.agree <- DiscountedAgreementMatrix( ex.3, 3 )

ex.1.agree
ex.2.agree
ex.3.agree

# ================================================================== #
# Correlational Class Analysis.

library( corclass )

# Run the corclass model.
ex.1.cor <- cca( ex.1, filter.value = 0.05 )
ex.2.cor <- cca( ex.2, filter.value = 0.05 )
ex.3.cor <- cca( ex.3, filter.value = 0.05 )




# ################# #
# END OF SYNTAX FILE.
# ################# #