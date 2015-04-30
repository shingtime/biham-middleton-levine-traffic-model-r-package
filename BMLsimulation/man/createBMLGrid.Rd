\name{createBMLGrid}
\alias{createBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
construct a BML Matrix
}
\description{
This function is a constructor of BML grid class. It constructs the matrix using the corresponding number of rows, number of columns, number of red cars and number of blue cars. 
}
\usage{
createBMLGrid(r, c, ncars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
  row number of the grid

}
 \item{c}{column number of the grid}
 \item{ncars}{A two elements vector, which contains an element named "red" and an element named "blue".}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{matrix}{The initial matrix with element 1, 0, -1. 1 represents red cars. 0 represents white space. -1 represents blue cars.}
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

g = createBMLGrid (100,100,c(red=2000,blue=2000)) 

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

