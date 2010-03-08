\name{mesh3d}
\alias{mesh3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw a 3D-mesh }
\description{
  mesh3d visualizes 3D meshes and parametric functions in the VRML- or Livegraphics3D-format.
}
\usage{
mesh3d(xfun = "sin(v)*cos(u)", yfun = "sin(v)*sin(u)",
       zfun = "cos(v)", param1 = "u", param2 = "v",
       range1 = c(0, 2 * pi), range2 = c(0, pi),
       size1 = 30, size2 = 30, type = "vrml", x = NULL,
       y = NULL, z = NULL, edges = NULL, obj_infile = NULL,
       filename = "out.wrl", write_obj = FALSE, cols = "red",
       scalefac = 4, autoscale = ifelse(is.null(obj_infile),
       "independent", "equicenter"), 
       lab.axis = c("X-axis", "Y-axis", "Z-axis"),
       col.axis = "black", showaxis = TRUE, col.lab = "black",
       col.bg = "white", cex.lab = 1, htmlout = NULL,
       hwidth = 1200, hheight = 800,
       vrml_navigation = "EXAMINE", vrml_transparency = 0,
       vrml_fov = 0.785, vrml_pos = rep(scalefac + 4, 3),
       vrml_dir = c(0.19, 0.45, 0.87, 2.45),
       lg3d_ambientlight = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xfun}{parametric function to define curves and surfaces
   (1. dimension)}
  \item{yfun}{ parametric function to define curves and surfaces
   (2. dimension)}
  \item{zfun}{ parametric function to define curves and surfaces
   (3. dimension)}
  \item{param1}{ name of the first parameter used in parametric
  functions}
  \item{param2}{ name of the second parameter used in parametric
  functions}
  \item{range1}{ value range for parameter "param1"}
  \item{range2}{ value range for parameter "param2"}
  \item{size1}{ mesh size for the first parameter used in
  parametric functions}
  \item{size2}{ mesh size for the second parameter used in
  parametric functions}
  \item{type}{ the output type ("vrml" or "lg3d"), this will be
    set automatically if mesh3d is called after vrml.open() or
    lg3d.open()}
  \item{x}{a 3-column numeric matrix of coordinates or a numeric
   vector of x-coordinates (not used if parameter obj\_infile or
   xfun is specified)}
  \item{y}{a numeric vector of y-coordinates (only needed if x is
   a vector)}
  \item{z}{a numeric vector of z-coordinates (only needed if x is
   a vector)}
  \item{edges}{ a numerical matrix in which each row contains a
   sequence of vertex identifiers representing the edges of a
   polygonal face (vertex identifiers are integer numbers,
   starting with 0)}
  \item{obj_infile}{filename of a 3D mesh input file in obj-format
  (not required if edges and x,y,z or the parameters for
   parametric functions are specified)}
  \item{filename}{ filename of the generated output file }
  \item{write_obj}{ TRUE, if the output is to be created in the
   obj-format (requires parameters x,y,z and edges to be set)}
  \item{cols}{the color of the output}
  \item{scalefac}{ a numerical scaling factor to increase/decrease
   the size of the plotted 3D objects}
  \item{autoscale}{ the scaling type. "independent" means all axes
   are auto-scaled independently. "equidist" means all axes are
   scaled by the same factor and "equicenter" additionally positions
   the drawn object in the center. "none" means no scaling is
   applied}
  \item{lab.axis}{ a vector of size 3 containing the axis labels }
  \item{col.axis}{ color of the axis }
  \item{showaxis}{ if FALSE, the coordinate axes are hidden in
   the output}    
  \item{col.lab}{ a vector of colors specifying the axis labels }
  \item{col.bg}{ background color }  
  \item{cex.lab}{ scaling factor for axis label font size }
  \item{htmlout}{ a filename for generating an HTML-file to embed
   the output }
  \item{hwidth}{ width of the embedded visualization in the
   HTML-output }
  \item{hheight}{ height of the embedded visualization in the
   HTML-output }
  \item{vrml_navigation}{type of mouse navigation in the VRML file,
   can be "EXAMINE", "WALK", "SLIDE", "FLY" or "PAN" (VRML only)}
  \item{vrml_transparency}{a number between 0 and 1 specifying the
   transparency level of plotted objects (VRML only)}
  \item{vrml_fov}{ a scalar defining the field of view angle in the
   VRML file in radians (VRML only)}
 \item{vrml_pos}{ a vector of size 3 corresponding to the position
   of the viewpoint (VRML only)}
  \item{vrml_dir}{a vector of size 4 specifying the viewing direction
   (first 3 components) and the rotation of the camera around the
    direction vector (last component in radians, VRML only)}
  \item{lg3d_ambientlight}{ambient light gray level (value between
   0 and 1, LG3D only)}
}
\details{
  
  mesh3d visualizes 3D-curves and surfaces specified as parametric 
  functions as well 3D-meshes defined by vertex-coordinates and
  edges between them. The output can be generated in the VRML- or
  Livegraphics3D-format, or in the obj-format, if the x,y,z- and
  edges-parameters are specified. 
  There are three ways to provide 3D data as input: An obj-file in the 
  current directory can be used as by setting the obj\_infile-parameter,
  a 3D mesh can be specified using the x,y,z and edges parameter (in both
  cases, no other parameters are required), or a parametric function
  can be defined using the xfun, yfun and zfun parameters (in this case,
  param1, param2, range1 and range2 parameters have to be set additionally).
  
  To add additional shapes and objects to the final 3D scene, this plotting 
  function can also be called within a VRML- or Livegraphics3D-environment 
  created by calling the vrml.open()- or lg3d.open()-function. In this case, 
  mesh3d will inherit all global parameters set in the vrml.open- or lg3d.open-
  function (e.g. filename, type, htmlout, etc.) and the user does not need to 
  specify these options anymore.
  
}
\value{
  The function is used for its side-effect (creating a VRML-,
  Livegraphics3D-, or obj-file) and has no return value.
}
\references{}
\author{Enrico Glaab}
\note{
}
\seealso{ \link{cloud3d}, \link{bar3d}}
\examples{

# Visualization of a parametric function
# defining the surface of 3D spiral

mesh3d(xfun="s*cos(s)*(4 + cos(t+s))",
       yfun="s*sin(s)*(4 + cos(t+s))",
       zfun="s*sin(t+s)",param1="s",
       param2="t",range1=c(0,4*pi),
       range2=c(0,2*pi), type="vrml",
       filename="spiral.wrl",
       htmlout="spiral.html")
       
# show the output in a web-browser 
# (VRML-plugin must be installed!)
browseURL(paste("file://",file.path(getwd(),
                "spiral.html"), sep=""))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }

