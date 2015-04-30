\name{summary.BMLGrid}
\alias{summary.BMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summary the current state of a given BML grid
}
\description{
This summary function gives the total numbers of cars, numbers of moving cars,numbers of blocked cars and average velocity for each color's car.
}
\usage{
summary.BMLGrid = function(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a BML grid with S3 class}
  \item{...}{additional arguments affecting the summary produced.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{output}{The output vector returns six values: total numbers of red cars, numbers of moving red cars, numbers of blocked red cars, average velocity of red cars, total numbers of blue cars,numbers of moving blue cars, numbers of blocked blue cars, average velocity of blue cars}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

## create the initial grid
g = createBMLGrid(10,10,c(red=20,blue=20))
## run 500 iterations
x = rumBMLGrid(500,g)
## summary the grid
summary.BMLGrid(x)
## since the BML grid has S3 class, the equivalent way is:
summary(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
