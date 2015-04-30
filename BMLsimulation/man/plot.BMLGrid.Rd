\name{plot.BMLGrid}
\alias{plot.BMLGrid}

\title{
Plot the BML model grid
}
\description{
This function uses S3 method to plot a BML grid
}
\usage{
plot.BMLGrid(x,...)
}
\arguments{
  \item{x}{an BML grid with S3 class}
  \item{...}{Arguments to be passed to methods, such as graphical parameters.}
}
\details{

}
\value{

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Juxiao Bu
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
## plot the final grid
plot.BMLGrid(x)
## Since the grid includes S3 class, the following function can get the same result
plot(x)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
