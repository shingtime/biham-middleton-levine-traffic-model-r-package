\name{rumBMLGrid}
\alias{rumBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run the BML traffic model simulation
}
\description{
This function provides a  way to run the simulaiton.
}
\usage{
rumBMLGrid(numSteps, grid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid }{
  The inital grid matrix
}
  \item{numSteps }{
  Number of iterations in the simulation process.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{matrix}{a matrix that represents the result after the given times of iterations}
}
\references{
}
\author{
Junxiao Bu
}
\note{
}


\seealso{
\code{\link{createBMLGrid}}
}
\examples{
library(BMLsimulation)
g = createBMLGrid(100, 100, c(red = 50, blue = 100))
g = rumBMLGrid(100,g)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
