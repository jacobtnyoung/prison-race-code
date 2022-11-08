# ############################################################################################# #
# This code uses ANTHROPAC for the inmate code and race items of the NIJ project.
# ############################################################################################# #

# This is the REVISED syntax based on reviewer comments.

# ================================================================== #
# Clear the workspace and load the libraries.

rm( list = ls() )

# ================================================================== #
# Load the data.

load( here::here( "prisoner_racial_code_recoded_mats_REVISION.R" ) )

# ================================================================== #
# CCA Models.

#library("devtools")
#install_github("alastair-JL/AnthroTools") 
# https://github.com/alastair-JL/AnthroTools

library( AnthroTools )

codes.model <- ConsensusPipeline( SurveyResults = p.code[,-1], numQ = 3 )
codes.model
race.model <- ConsensusPipeline( SurveyResults = r.code[,-1], numQ = 3 )
race.model

summary( codes.model$origCompetence )
summary( race.model$origCompetence )

length( which( codes.model$origCompetence > 0.7 ) ) / 266
length( which( race.model$origCompetence > 0.7 ) ) / 266
length( which( codes.model$origCompetence > 0.7 & race.model$origCompetence > 0.7 ) ) / 266


# ================================================================== #
# First factor Competencies.

# Plot the competencies.
library( dplyr )
library( pander )
library( gridExtra )
library( ggplot2 )

codes.comp <- cbind( p.code[,1],codes.model$origCompetence )
race.comp  <- cbind( r.code[,1], race.model$origCompetence )

colnames( codes.comp ) <- c( "id", "orig.comp.code")
colnames( race.comp ) <- c( "id", "orig.comp.race")

df <- merge( codes.comp, race.comp, by="id", all= TRUE)

a <- ggplot(df, aes( x=orig.comp.code ) ) + 
  geom_histogram( color="black", fill="grey", bins = 70 ) +
  geom_vline(xintercept=mean(df$orig.comp.code,na.rm = TRUE), size=1.5, color="grey40") +
  xlab("Competency") + ylab("Frequency") +
  theme_minimal() + 
  ggtitle( "Panel C: Plot of Competencies for\n Prison Code Items" )
a

b <- ggplot(df, aes( x=orig.comp.race ) ) + 
  geom_histogram( color="black", fill="grey", bins = 70 ) + 
  geom_vline(xintercept=mean(df$orig.comp.race,na.rm = TRUE), size=1.5, color="grey40") +
  xlab("Competency") + ylab("Frequency") + 
  theme_minimal() + 
  ggtitle( "Panel D: Plot of Competencies for\n Racial Code Items" )
b

c <- ggplot( df, aes( x=orig.comp.code, y=orig.comp.race ) ) + 
  xlab("Competency (Prisoner Code)") + ylab("Competency (Racial Code)") +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, col = "grey40") +
  theme_minimal() + 
  ggtitle( "Panel C: Scatterplot of Competencies for\n Prison Code and Racial Code Items" )
c

grid.arrange( a, b, c, nrow = 2, ncol = 2 )

cor( codes.model$origCompetence, race.model$origCompetence )


# ================================================================== #
# Plot w/ Agreement and Distribution Plots.

grid.arrange( ggheatmap.PC, ggheatmap.RC, a, b, nrow = 2, ncol = 2 )


# ================================================================== #
# Second factor Competencies.

# Get the competencies for the second factor.
codes.d.agreement.factors <- ComreySolve( codes.d.agreement )
race.d.agreement.factors  <- ComreySolve( race.d.agreement  )
codes.comp2               <- cbind( p.code[,1], codes.d.agreement.factors$second )
race.comp2                <- cbind( r.code[,1], race.d.agreement.factors$second  )

# Plot the competencies for the second factor.
colnames( codes.comp2 )   <- c( "id", "second.comp.code" )
colnames( race.comp2 )    <- c( "id", "second.comp.race" )

df2 <- merge( codes.comp2, race.comp2, by="id", all= TRUE )

a2 <- ggplot( df2, aes( x=second.comp.code ) ) + 
  geom_histogram( color="black", fill="grey", bins = 70 ) +
  geom_vline( xintercept=mean( df$second.comp.code,na.rm = TRUE ), size=1.5, color="grey40" ) +
  xlab("Competency for Second Factor") + ylab("Frequency") +
  theme_minimal() + 
  ggtitle( "Panel A: Plot of Competencies (Second Factor) for\n Prisoner Code Items" )
a2

b2 <- ggplot(df2, aes( x=second.comp.race ) ) + 
  geom_histogram( color="black", fill="grey", bins = 70 ) + 
  geom_vline(xintercept=mean(df$second.comp.race,na.rm = TRUE), size=1.5, color="grey40") +
  xlab("Competency for Second Factor") + ylab("Frequency") + 
  theme_minimal() + 
  ggtitle( "Panel B: Plot of Competencies (Second Factor) for\n Racial Code Items" )
b2

c2 <- ggplot( df2, aes( x=second.comp.code, y=second.comp.race ) ) + 
  xlab("Second Factor Competency (Prisoner Code)") + ylab("Second Factor Competency (Racial Code)") +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, col = "grey40") +
  theme_minimal() + 
  ggtitle( "Panel C: Scatterplot of Competencies (Second Factor) for\n Prisoner Code and Racial Code Items" )
c2

grid.arrange( a2, b2, c2, nrow = 2, ncol = 2 )

cor(codes.d.agreement.factors$second, race.d.agreement.factors$second)

## Plot them all.
grid.arrange( a, b, a2, b2, c, c2, nrow = 3, ncol = 2 )


# ================================================================== #
# Export the competencies.

# Export these results to an outsheet for stata.
data.out <- merge( df, df2, by = "id")
write.csv(data.out, here::here( "prison_code_competencies_REVISION.csv" ) )
