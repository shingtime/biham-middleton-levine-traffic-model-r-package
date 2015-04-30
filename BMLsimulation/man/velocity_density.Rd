\name{velocity_density}
\alias{velocity_density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the average velocity for BML grid given different density
}
\description{
This function returns the result the total average velocity of a BML grid when passing
different density.
}
\usage{
velocity_density(r,c,density,numSteps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{row number of the grid}
  \item{c}{column number of the grid}
  \item{density}{ratio of total numbers of cars to the grid size}
  \item{numSteps}{Number of iterations in the simulation process.}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{velocity_summary}{The function returns a two column dataframe. The first column is the corresponding average velocity of the grid. The second column is the corresponding density.}
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
density = seq(from=0.2,to=0.7,length.out=50)
velocity_density(r=100,c=100,density,numSteps=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
