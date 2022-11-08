# This syntax file takes the .csv file for the prisoner code and racial code items and preps them for analysis.

# ================================================================== #
# Clear the workspace and load the libraries.

rm( list = ls() )

# ================================================================== #
# Load the data.

dat <- as.matrix( read.csv( here::here( "prisoner_racial_code_items.csv", as.is=TRUE, header=TRUE ) ) 0

# Create the separate data files.
p.code <- dat[,c( 2:10 )]
r.code <- dat[,c( 11:17 )]

# Check missing values.
table( is.na( p.code ) )
table( is.na( p.code ) ) / dim( p.code )[2]

table( is.na( r.code ) )
table( is.na( r.code ) ) / dim( r.code )[2]

# Summary stats before the recode.
for( i in 1:dim( p.code )[2] ){
  print( mean( p.code[,i], na.rm = TRUE ) )
}
for( i in 1:dim( p.code )[2] ){
  print( sd( p.code[,i], na.rm = TRUE ) )
}
for( i in 1:dim( r.code )[2] ){
  print( mean( r.code[,i], na.rm = TRUE ) )
}
for( i in 1:dim( r.code )[2] ){
  print( sd( r.code[,i], na.rm = TRUE ) )
}

# Write the function to replace the missing values.
NA.replace <- function( input.dat, pdist ){
  set.seed( 12345 )
  for( i in 1:nrow( input.dat ) ){
    for( j in 1:ncol( input.dat ) ){
      if( is.na( input.dat[i,j] == TRUE ) )
        input.dat[i,j] <- pdist
    }
  }
  return( input.dat )
}

# Write the function to replace the values.
change.values <- function( input.dat ){
  new.vals <- input.dat
  # recode "Disagree" to be the same as "Strongly Disagree".
  new.vals[input.dat == 1] <- 1
  new.vals[input.dat == 2] <- 1
  # recode "Neither Agree or Disagree" to be between these two.
  new.vals[input.dat == 3] <- 2
  # recode "Agree" to be the same as "Strongly Disagree".
  new.vals[input.dat == 4] <- 3
  new.vals[input.dat == 5] <- 3
  
  # Replace missing values.
  new.vals.no.NA <- NA.replace( new.vals, round( runif( 1,1,3 ), 0 ) )
  return( new.vals.no.NA )
}

# Run the functions.
p.code <- change.values( p.code )
r.code <- change.values( r.code )

# Summary stats after the recode.
for( i in 1:dim( p.code )[2] ){
  print( mean( p.code[,i], na.rm = TRUE ) )
}
for( i in 1:dim( p.code )[2] ){
  print( sd( p.code[,i], na.rm = TRUE ) )
}
for( i in 1:dim( r.code )[2] ){
  print( mean( r.code[,i], na.rm = TRUE ) )
}
for( i in 1:dim( r.code )[2] ){
  print( sd( r.code[,i], na.rm = TRUE ) )
}

# add back on the ids.
p.code   <- cbind( dat[,1],p.code )
r.code   <- cbind( dat[,1],r.code )


# ================================================================== #
# Save the file.

save.image( here::here( "prisoner_racial_code_recoded_REVISION.R" ) )


# ################################################################## #
# END OF SYNTAX FILE.
# ################################################################## #
