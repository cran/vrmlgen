\name{vrmlgen-package}
\alias{vrmlgen-package}
\alias{vrmlgen}
\docType{package}
\title{
Create 3D data plots, charts and graphs as VRML files
}
\description{
vrmlgen translates 3D data, i.e. 3D point clouds, meshes or surfaces, into a visual representation in the Virtual Reality Markup Language (VRML, file type .wrl) or alternatively, in the LiveGraphics3D format.
Apart from providing access to low-level plotting functions (points, lines, text, etc.) through a unified interface, several higher-level visualization methods are supported, including 3D scatter plots, meshes, bar charts, graphs and density estimation contour surfaces.
}
\details{
\tabular{ll}{
Package: \tab vrmlgen\cr
Type: \tab Package\cr
Version: \tab 1.4.8\cr
Date: \tab Date: 2012-03-23\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
URL: \tab http://www.cs.nott.ac.uk/~egg/vrmlgen\cr
}
}
\author{
Enrico Glaab

e.glaab@cs.nott.ac.uk
}

\references{
	
	Enrico Glaab, Jonathan M. Garibaldi, Natalio Krasnogor (2010).  vrmlgen: An R Package for
	3D Data Visualization on the Web. \emph{Journal of Statistical Software, 36(8)}, p. 1-18.
	URL: \url{http://www.jstatsoft.org/v36/i08/}
	
}

\keyword{ package }
\examples{

curdir <- getwd()
outdir <- tempdir()
setwd(outdir)

# create 33x3 matrix with random 3D input data
mat <- matrix(runif(99, 0, 3), ncol = 3)

# create random class assignment vector with three classes
y <- round(runif(33, 0, 2))
y <- ifelse(y == 0, "class 1", ifelse(y == 1, "class 2", "class 3"))

# create ouput using numbers from 1 to length(y) as metalabels
cloud3d(mat, labels = y, metalabels = 1:length(y), col.axis = "black", col.lab = "blue",
col.bg = "white")

setwd(curdir)

}
