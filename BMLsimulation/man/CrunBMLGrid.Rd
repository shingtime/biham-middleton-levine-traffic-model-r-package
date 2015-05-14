\name{CrunBMLGrid}
\alias{CrunBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
C-version to move cars and get the final grid.
}
\description{
This function calls the function implemented in C and returns the final grid using
same algorithm as the R-version.
}
\usage{
CrunBMLGrid(grid,numsteps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
The BML grid generate.
}
  \item{numsteps}{
How many iterations we want to implement.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{matrix}{a BML grid generate after given time steps.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Junxiao Bu
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

library(BMLsimulation)
g = createBMLGrid(10, 10, c(red = 20, blue = 20))
grid = CrunBMLGrid(g,100)

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
