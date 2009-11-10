\name{lcloud}
\alias{lcloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create scatter plots for 3D data matrices in LiveGraphics3D-format}
\description{
lcloud translates 3D point data, i.e. numerical matrices with 3 columns and n rows, 
into visual representations in the LiveGraphics3D-format. Please make sure that the file "live.jar", which can be obtained
from http://www.fktech.eu/fr/enligne/doc-fr/LG3Dfr/LG3DInstallation.htm, is saved in the same
folder as the generated output files.
}
\usage{
lcloud(data, labels = rownames(data), metalabels = NULL, 
urllabels = NULL, filename = "out.m", pointstyle = c("s", "b",
"t"), cols = rainbow(length(unique(labels))), scalefac = 4,
lab.axis = c("X-axis", "Y-axis", "Z-axis"), col.axis = "black",
showaxis = TRUE, col.lab = "black", col.metalab = "black",
col.bg = "white", cex.lab = 1, ambientlight = 0.5,
htmlout = NULL, hwidth = 1200, hheight = 800, mode2d = FALSE,
showlegend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a numerical matrix with 3 columns and n rows}
  \item{labels}{ a vector of size n containing the group labels
   (strings or numbers) of the data points }
  \item{metalabels}{ a vector of strings or numbers containing
    optional metalabels for the rows which can be accessed in the
    plot by clicking on a data point with the mouse)}
  \item{urllabels}{  a vector of strings containing hyperlinks
    which can be accessed from the plot by clicking on a data point
    with the mouse}   
  \item{filename}{ filename of the generated LiveGraphics3D output file }
  \item{pointstyle}{ a vector of point style types, possible types
   are "s" for spheres, "b" for boxes and "t" for tetrahedron. Use a
   vector of size 1 to draw all points in the same type, otherwise
   the number of used point styles must be at least as high as the
   number of different labels (i.e. length(pointstyles) >=
   length(unique(labels)))}
  \item{cols}{ a vector of colors to visualize different classes
  among the data points. The number of colors should be at least
  as large as the number of different labels (i.e. length(col)
  >= length(unique(labels)) }
  \item{scalefac}{ a scaling factor for the size of the 3D box in
   which the data will be visualized }
  \item{lab.axis}{ a vector of size 3 containing the axis labels }
  \item{col.axis}{ color of the axis }
  \item{showaxis}{ if FALSE, the coordinate axes are hidden in
   the output}  
  \item{col.lab}{ a vector of colors specifying the axis labels }
  \item{col.metalab}{ a vector of colors specifying the meta labels }  
  \item{col.bg}{ background color }  
  \item{cex.lab}{ scaling factor for axis label font size }
  \item{ambientlight}{ ambient light gray level (between 0 and 1) }
  \item{htmlout}{ a filename for generating an HTML-file to embed
  the VRML-output }
  \item{hwidth}{ width of the embedded VRML-display in the HTML-output }
  \item{hheight}{ height of the embedded VRML-display in the HTML-output }
  \item{mode2d}{ generate a zoomable 2D plot (the z-coordinates of the input
   matrix are set to zero)}
  \item{showlegend}{ a logical value indicating whether a legend should
  be drawn on the plot}  
}
\details{
}
\value{
  The function is used for its side-effect (output of a LiveGraphics3D-file
  and optionally an HTML-page in the specified directory) and has no
  return value.  The htmlout-parameter can be used to embed
  the resulting LiveGraphics3D object in a HTML-file.
}
\references{
Martin Kraus. LiveGraphics3D - a non-commercial Java 1.1 applet. URL: \url{http://wwwvis.informatik.uni-stuttgart.de/~kraus/index.html}
}
\author{ Enrico Glaab }
\note{ 
}
\seealso{ \link{vcloud}, \link{vbar}, \link{lbar} }
\examples{


# example 1: visualize random 3D input data
mat <- matrix(runif(99, 0, 3), ncol=3)

# create random class assignment vector with three classes
y <- round(runif(33, 0, 2))
y <- ifelse(y == 0, "class 1", ifelse(y == 1, "class 2", "class 3"))

# create ouput using numbers from 1 to length(y) as metalabels
lcloud(mat, y, col.axis="black", col.lab="blue", col.bg="white",
htmlout="result.html")


# example 2: scaled parametric plot of a sphere
p <- seq(-pi,pi, length=30)
x <- c(rep(1, 30) \%*\% t(cos(p)))
y <- c(cos(p) \%*\% t(sin(p)))
z <- c(sin(p) \%*\% t(sin(p)))

lcloud(cbind(x,y,z), cols="blue", showaxis=FALSE)


# example 3: visualization of Edgar Anderson's Iris data
# using density estimation contour surfaces (requires misc3-package!)
irismat <- iris[,1:3]
lcloud(irismat, labels=iris[,5], col.bg="white", col.axis="blue",
col.lab="black", lab.axis=colnames(irismat))
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
