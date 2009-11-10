\name{vmesh}
\alias{vmesh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create 3D mesh and parametric function visualizations in VRML-format}
\description{
vmesh visualizes 3D meshes and parametric functions in the VRML-format.
}
\usage{
vmesh(infile = NULL, data = NULL, edges = NULL,
xfun = "sin(v)*cos(u)", yfun = "sin(v)*sin(u)",
 zfun = "cos(v)", param1 = "u", param2 = "v",
range1 = c(0,2*pi), range2 = c(0,pi), size1 = 30,
size2 = 30, filename = "out.wrl", cols = "red",
scalefac = 4, autoscale=ifelse(is.null(infile),
"independent","equicenter"), lab.axis = c("X-axis",
"Y-axis", "Z-axis"), col.axis = "white", showaxis = TRUE,
col.lab = "white", col.bg = "black", cex.lab = 1,
navigation = "EXAMINE", transparency = 0, fov = 0.785,
pos = rep(scalefac + 4, 3), dir = c(0.19, 0.45, 0.87, 2.45),
htmlout = NULL, hwidth = 1200, hheight = 800)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{ filename of 3D mesh input file in obj-format
  (not required if data and edges or the parameters for
   parametric functions are specified)}
  \item{data}{ a numerical matrix with 3 columns and n rows
  specifying the coordinates of mesh vertices (not used if
  parameter infile or xfun is specified}
  \item{edges}{ a numerical matrix with each row corresponding
  to the vertex numbers which form a polygon (first vertex has
  number 0)}
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
  \item{filename}{ filename of the generated VRML output file }
  \item{cols}{ a vector of colors to visualize different classes
  among the data points. The number of colors should be at least
  as large as the number of different labels (i.e. length(col)
  >= length(unique(labels)) }
  \item{scalefac}{ a scaling factor for the size of the 3D box in
   which the data will be visualized }
  \item{autoscale}{ the scaling type. "independent" means all axes
   are auto-scaled independently, "equidist" means all axes are
   scaled by the same factor, "equicenter" additionally positions
   the drawn object in the center, "none" means no scaling is
   applied}
  \item{lab.axis}{ a vector of size 3 containing the axis labels }
  \item{col.axis}{ color of the axis }
  \item{showaxis}{ if FALSE, the coordinate axes are hidden in
   the output}    
  \item{col.lab}{ a vector of colors specifying the axis labels }
  \item{col.bg}{ background color }  
  \item{cex.lab}{ scaling factor for axis label font size }
  \item{navigation}{ type of mouse navigation in the VRML file,
   can be "EXAMINE", "WALK", "SLIDE", "FLY" or "PAN"}  
  \item{fov}{ a scalar defining the field of view angle in the
   VRML file in radians }
  \item{pos}{ a vector of size 3 for the position of the viewpoint
   in the VRML coordinate system }
  \item{transparency}{ a value between 0 and 1 specifying the degree
   of transparency for the objects to be drawn}
  \item{dir}{ a vector of size 4 specifying the viewing direction
   (first 3 components) and the rotation of the camera around the
    direction vector (last component in radians)}
  \item{htmlout}{ a filename for generating an HTML-file to embed the VRML-output }
  \item{hwidth}{ width of the embedded VRML-display in the HTML-output }
  \item{hheight}{ height of the embedded VRML-display in the HTML-output }
}
\details{
  There are three ways to use the function: An obj-file in the 
  current directory can be specified as input with the infile-parameter,
  a 3D mesh can be specified using the data and edges parameter (in both
  cases, no other parameters are required), or a parametric function
  can be defined using the xfun, yfun and zfun parameters (in this case,
  param1, param2, range1 and range2 parameters have to be set additionally).
  The htmlout-parameter can be used to embed the resulting VRML-object
  in a HTML-file.
}
\value{
  The function is used for its side-effect (output of a VRML-file 
  in the specified directory) and has no return value. The
  htmlout-parameter can be used to embed the resulting VRML-object
  in a HTML-file.
}
\references{ }
\author{ Enrico Glaab }
\note{ 
}
\seealso{ \link{lmesh}  }
\examples{

# Visualization of a parametric function
# defining the surface of 3D spiral
vmesh(xfun="s*cos(s)*(4 + cos(t+s))",
yfun="s*sin(s)*(4 + cos(t+s))",
zfun="s*sin(t+s)",
param1="s", param2="t",
range1=c(0,4*pi), range2=c(0,2*pi))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
