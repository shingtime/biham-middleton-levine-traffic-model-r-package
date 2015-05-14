\name{car_coordinate}
\alias{car_coordinate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the matrix's coordinates
}
\description{
this function returns the coordinates of all the cars.
}
\usage{
car_coordinate(matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
The matrix that we want to compute.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{dataframe}{a dataframe that represents the coordinates}
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
car_coordinate(g)

}
