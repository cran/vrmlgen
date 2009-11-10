\name{lmesh}
\alias{lmesh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create parametric function visualizations in LiveGraphics3D-format}
\description{
lmesh visualizes parametric functions in the LiveGraphics3D-format.
}
\usage{
lmesh(xfun = "sin(v)*cos(u)", yfun = "sin(v)*sin(u)",
zfun = "cos(v)", param1 = "u", param2 = "v",
range1 = c(0,2*pi), range2 = c(0,pi), size1 = 30,
size2 = 30, filename = "out.m", cols = NULL,
scalefac = 4, lab.axis = c("X-axis", "Y-axis", "Z-axis"),
col.axis = "black", showaxis = TRUE, col.lab = "black",
col.bg = "white", cex.lab = 1, ambientlight = 0.5,
htmlout = NULL, hwidth = 1200, hheight = 800)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xfun}{ parametric function to define curves and surfaces
   (1. dimension)}
  \item{yfun}{ parametric function to define curves and surfaces
   (2. dimension)}
  \item{zfun}{ parametric function to define curves and surfaces
   (3. dimension)}
  \item{param1}{ name of the first parameter used in parametric
  functions}
  \item{param2}{ name of the second parameter used in parametric
  functions}
  \item{range1}{ value range for the first parameter for
  parametric functions}
  \item{range2}{ value range for the second parameter for
  parametric functions}
  \item{size1}{ mesh size for the first parameter used in
  parametric functions}
  \item{size2}{ mesh size for the second parameter used in
  parametric functions}
  \item{filename}{ filename of the generated LiveGraphics3D
  output file}
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
  \item{col.bg}{ background color }  
  \item{cex.lab}{ scaling factor for axis label font size }
  \item{ambientlight}{ ambient light gray level (between 0 and 1) }
  \item{htmlout}{ a filename for generating an HTML-file to embed }
  \item{hwidth}{ width of the embedded VRML-display in the HTML-output }
  \item{hheight}{ height of the embedded VRML-display in the HTML-output }
}
\details{
  A parametric function can be defined with the xfun, yfun and 
  zfun parameters. Additionally, the parameter names and ranges 
  have to be specified using the param1, param2, range1 and
  range2 parameters. The htmlout-parameter can be used to embed
  the resulting LiveGraphics3D object in a HTML-file.
}
\value{
  The function is used for its side-effect (output of a VRML-file 
  in the specified directory) and has no return value.
}
\references{ }
\author{ Enrico Glaab }
\note{ 
}
\seealso{ \link{vmesh}}
\examples{

# Visualization of a parametric function
# defining the surface of 3D spiral
lmesh(xfun="s*cos(s)*(4 + cos(t+s))",
yfun="s*sin(s)*(4 + cos(t+s))",
zfun="s*sin(t+s)",
param1="s", param2="t",
range1=c(0,4*pi), range2=c(0,2*pi))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
