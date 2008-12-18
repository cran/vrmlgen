\name{vrmlgen-package}
\alias{vrmlgen-package}
\alias{vrmlgen}
\docType{package}
\title{
Create 3D data plots, charts and graphs as VRML files
}
\description{
vrmlgen translates numerical data into 3-dimensional representations in the Virtual Reality Markup Language (VRML). 
Supported visualization methods are 3D scatter plots, meshes, bar charts, graphs and density estimation contour surfaces.  
}
\details{
\tabular{ll}{
Package: \tab vrmlgen\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-04-04\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Enrico Glaab

e.glaab@cs.nott.ac.uk
}
\references{
}
\keyword{ package }
\seealso{
}
\examples{
# create 33x3 matix with random 3D input data
mat <- matrix(runif(99, 0, 3), ncol=3)

# create random class assignment vector with three classes
y <- round(runif(33, 0, 2))
y <- ifelse(y == 0, "class 1", ifelse(y == 1, "class 2", "class 3"))

# create ouput using numbers from 1 to length(y) as metalabels
vcloud(mat, y, metalabels=1:length(y), col.axis="black", col.lab="blue", col.bg="white")
}
