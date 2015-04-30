\name{BMLsimulation-package}
\alias{BMLsimulation-package}
\alias{BMLsimulation}
\docType{package}
\title{
\packageTitle{BMLsimulation}
}
\description{
\packageDescription{BMLsimulation}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{BMLsimulation}
\packageIndices{BMLsimulation}

The Biham-Middleton-Levine model, a.k.a. BML model, is a simple traffic flow model. In this model,each intersection of a square grid of streets contains either an East-facing car, a North-facing car, or an empty space.All the North-facing cars simultaneously attempt to move one unit North; a car succeeds if there is already an empty space for it to move into.The East-facing cars attempt to move East in the same way.
Initially, cars are distributed at random. Each is car is independently equally likely to be East-facing or North-facing.
}
\author{
\packageAuthor{BMLsimulation}

Maintainer: \packageMaintainer{BMLsimulation}
}
\references{
\url{https://www.jasondavies.com/bml/#0.50/128/128}
}

\keyword{ package }

\examples{
library("BMLsimulation")
#Create 20*20 Grid, with 50 red cars and 100 blue cars 
g = createBMLGrid(20, 20, c(red = 50, blue = 100)) 
#Run Simulation, 1000 iteration
g = rumBMLGrid(1000, g)
## plot the grid
plot(g)
#Summarize the output
summary(g)
## average velocity of 10*10 grid with density 0.3
velocity_density(10,10,0.3,100)
}
