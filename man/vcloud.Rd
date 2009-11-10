\name{vcloud}
\alias{vcloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create scatter plots for 3D data matrices in VRML}
\description{
vcloud translates 3D point data, i.e. numerical matrices with 3 columns and n rows, 
into visual representations in the Virtual Reality Markup Language (VRML).  
}
\usage{
vcloud(data, labels = rownames(data), metalabels = NULL,
filename = "out.wrl", pointstyle = c("s", "b", "c"), cols
= rainbow(length(unique(labels))), showdensity = FALSE,
scalefac = 4, lab.axis = c("X-axis", "Y-axis", "Z-axis"),
col.axis = "white", showaxis = TRUE, col.lab = "white",
col.bg = "black", cex.lab = 1, navigation = "EXAMINE",
transparency = 0, fov = 0.785, pos = rep(scalefac + 4, 3),
dir = c(-0.59, 0.77, 0.24, 0.99), htmlout = NULL, hwidth
= 1200, hheight = 800, showlegend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a numerical matrix with 3 columns and n rows}
  \item{labels}{ a vector of size n containing the group labels
   (strings or numbers) of the data points }
  \item{metalabels}{ a vector of strings or numbers containing
    optional metalabels for the rows which will appear as 
    popup-text when moving the mouse over a data point } 
  \item{filename}{ filename of the generated VRML output file }
  \item{pointstyle}{ a vector of point style types, possible types
   are "s" for spheres, "b" for boxes and "c" for cones. Use a
   vector of size 1 to draw all points in the same type, otherwise
   the number of used point styles must be at least as large as the
   number of different labels (i.e. length(pointstyles) >=
   length(unique(labels)))}
  \item{cols}{ a vector of colors to visualize different classes
  among the data points. The number of colors should be at least
  as large as the number of different labels (i.e. length(col)
  >= length(unique(labels)) }
  \item{showdensity}{ if TRUE, semi-transparent density estimation
   contour surfaces will be drawn to identify regions of high data
   density (requires misc3d-package)}
  \item{scalefac}{ a scaling factor for the size of the 3D box in
   which the data will be visualized }
  \item{lab.axis}{ a vector of size 3 containing the axis labels }
  \item{col.axis}{ color of the axis }
  \item{showaxis}{ if FALSE, the coordinate axes are hidden in
   the output}  
  \item{col.lab}{ a vector of colors specifying the axis labels }
  \item{col.bg}{ background color }  
  \item{cex.lab}{ scaling factor for axis label font size }
  \item{navigation}{ type of mouse navigation in the VRML file, can
   be "EXAMINE", "WALK", "SLIDE", "FLY" or "PAN"}  
  \item{transparency}{ a value between 0 and 1 specifying the degree
   of transparency for the objects to be drawn}    
  \item{fov}{ a scalar defining the field of view angle in the VRML
   file in radians }
  \item{pos}{ a vector of size 3 for the position of the viewpoint
   in the VRML coordinate system }
  \item{dir}{ a vector of size 4 specifying the viewing direction
   (first 3 components) and the rotation of the camera around the
   direction vector (last component in radians)}     
  \item{htmlout}{ a filename for generating an HTML-file to embed the VRML-output }
  \item{hwidth}{ width of the embedded VRML-display in the HTML-output }
  \item{hheight}{ height of the embedded VRML-display in the HTML-output }
  \item{showlegend}{ a logical value indicating whether a legend should
  be drawn on the plot}  
}
\details{
}
\value{
  The function is used for its side-effect (output of a VRML-file in the
  specified directory) and has no return value. The htmlout-parameter can
  be used to embed the resulting VRML-object in a HTML-file.
}
\references{
Bowman, A. W. and Azzalini, A. (2007). R package 'sm': \emph{Nonparametric smoothing methods} (version 2.2) URL: \url{http://www.stats.gla.ac.uk/~adrian/sm}, \url{http://azzalini.stat.unipd.it/Book_sm}

Feng, D. and Tierney, L. (2009). R package 'misc3d": \emph{Miscellaneous 3D plots} (version 0.6-1) URL: \url{http://cran.r-project.org/web/packages/misc3d/index.htm}
}
\author{ Enrico Glaab }
\note{ 
}
\seealso{ \link{vbar}, \link{lcloud}, \link{lbar} }
\examples{


# example 1: visualize random 3D input data
mat <- matrix(runif(99, 0, 3), ncol=3)

# create random class assignment vector with three classes
y <- round(runif(33, 0, 2))
y <- ifelse(y == 0, "class 1", ifelse(y == 1, "class 2", "class 3"))

# create ouput using numbers from 1 to length(y) as metalabels
vcloud(mat, y, metalabels=1:length(y), col.axis="black", col.lab="blue",
col.bg="white", htmlout="result.html")


# example 2: scaled parametric plot of a sphere
p <- seq(-pi,pi, length=30)
x <- c(rep(1, 30) \%*\% t(cos(p)))
y <- c(cos(p) \%*\% t(sin(p)))
z <- c(sin(p) \%*\% t(sin(p)))

vcloud(cbind(x,y,z), scalefac=7)


# example 3: visualization of Edgar Anderson's Iris data
# using density estimation contour surfaces (requires misc3-package!)
irismat <- iris[,1:3]
vcloud(irismat, labels=iris[,5], showdensity=TRUE,
col.bg="white", col.axis="blue", col.lab="black",
lab.axis=colnames(irismat))
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
