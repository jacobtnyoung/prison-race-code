# ############################################################################################# #
# This code uses the corclass package for the inmate code and race items of the NIJ project.
# ############################################################################################# #

# ================================================================== #
# Clear the workspace and load the libraries.

rm( list = ls() )

set.seed( 12345 )

# ================================================================== #
# Load the data.

load( here::here(  "prisoner_racial_code_recoded_mats_REVISION.R" ) )

# Define the variable names.
p.names <- c( "PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9" )
r.names <- c( "RC1","RC2","RC3","RC4","RC5","RC6","RC7" )


# ================================================================== #
# Correlational Class Analysis.

library( corclass )

# Put the objects together.
p.code.r <- p.code[,-1]
colnames( p.code.r ) <- p.names
r.code.r <- r.code[,-1]
colnames( r.code.r ) <- r.names
c.code.r <- cbind( p.code.r, r.code.r )

# Run the corclass model.
c.code.cor <- cca( c.code.r, filter.value = 0.05 )
p.code.cor <- cca( p.code.r, filter.value = 0.05 )
r.code.cor <- cca( r.code.r, filter.value = 0.05 )

# Get the correlation among individuals.
c.mat <- c.code.cor$cormat[1:dim( c.code.cor$cormat )[1], 1:dim( c.code.cor$cormat )[1]]

# Set the diagonal to zero.
diag( c.mat ) <- 0

# Information on group membership/size.
table( c.code.cor$membership )
sum( table( c.code.cor$membership ) )
sum( table( c.code.cor$membership ) ) - 266

# group proportions.
table( c.code.cor$membership ) / 266

# Export the schema assignments for analysis in Stata.
miss.membership <- rep( 0, length.out = length( p.code[,1] ) )
miss.membership[ attr( c.code.cor$cormat, "zeros" ) ] <- 1
df1 <- data.frame( id = p.code[,1], miss.m = miss.membership )
df2 <- data.frame( id = df1$id[df1$miss.m != 1], member = c.code.cor$membership )
data.out <- merge( df1, df2, by = "id", all = TRUE)
data.out$member[is.na( data.out$member ) == TRUE] <- 999
write.csv(data.out,"/Users/jyoung20/Dropbox (ASU)/Living and Working in Max. (NIJ 2017)/NIJ_LWMAX_Data_Syntax/NIJ_LWMAX_CCA_PROJECT/NIJ_LWMAX_CCA_PROJECT_data/prison_code_memberships_REVISION.csv")


# ================================================================== #
# Modularity.

library( network )

c.net <- as.network( c.mat, directed = FALSE )
c.net %v% "member" <- c.code.cor$membership

library( intergraph )
library( igraph )

i.c.net <- asIgraph( c.net )
modularity( i.c.net, c.code.cor$membership, weights = get.edge.attribute(c.net$mel,"cor")  )


# ================================================================== #
# Save the file before plotting.

save.image( here::here( "prisoner_racial_code_corclass_REVISION.R" ) )
