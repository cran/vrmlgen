\name{vrml.open}
\alias{vrml.open}
\title{VRML output device system}
\description{
vrml.open creates a new VRML-environment in which primitive and higher-level 
plotting functions can be combined together to create a 3D-VRML scene.
}
\usage{
vrml.open(filename = "out.wrl", col = "white", navigation = NULL,
          scale = 1, fov = 0.785, pos = rep(scale + 8, 3),
          dir = c(0.19, 0.45, 0.87, 2.45),
          html.embed = "out.html", hwidth = 1200, hheight = 800)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ filename of the generated VRML output file }
  \item{col}{ background color }
  \item{navigation}{ type of mouse navigation in the VRML file,
   can be "EXAMINE", "WALK", "SLIDE", "FLY" or "PAN" }
  \item{scale}{ a scaling factor for the size of the entire
   3D scene }
  \item{fov}{ a scalar defining the field of view angle in the
   VRML file in radians }
  \item{pos}{ a vector of size 3 corresponding to the position
   of the viewpoint }
  \item{dir}{ a vector of size 4 specifying the viewing direction
   (first 3 components) and the rotation of the camera around the
    direction vector (last component in radians)}
  \item{html.embed}{ a filename for generating an HTML-file to embed the
   VRML-output }
  \item{hwidth}{ width of the embedded visualization in the HTML-output }
  \item{hheight}{ height of the embedded visualization in the HTML-output }
}
\details{  
  
  This function creates a new VRML-environment to provide access to primitive 
  plotting functions like points3d, lines3d and text3d and to combine multiple 
  plotting functions together (both primitive and higher-level functions). After 
  creating the VRML-environment, all primitive and higher-level functions called 
  afterwards will automatically inherit the higher-level settings (background 
  color, navigation-type, etc.) specified in the vrml.open-parameters. In order 
  to close the VRML-environment and create the plot, the vrml.close() function 
  has to be called.  
  
}
\value{
  The function is used for its side-effect (output of a VRML-file 
  in the specified directory) and has no return value. The
  htmlout-parameter can be used to embed the resulting VRML-object
  in a template HTML-file.
}
\author{ Enrico Glaab }
\references{
	
	Enrico Glaab, Jonathan M. Garibaldi, Natalio Krasnogor (2010).  vrmlgen: An R Package for
	3D Data Visualization on the Web. \emph{Journal of Statistical Software, 36(8)}, p. 1-18.
	URL: \url{http://www.jstatsoft.org/v36/i08/}
	
}
\seealso{ \link{vrml.close} }
\examples{

curdir <- getwd()
outdir <- tempdir()
setwd(outdir)

# This example loads the atom coordinates of a molecule
# (C60, fullerene) and visualizes the molecule in 3D
# using points for the atoms and lines for the atom bonds
# (atom pairs within a given distance threshold).

vrml.open(file = "c60.wrl", navigation = "EXAMINE",
          html.embed = "c60example.html")

# load dataset
data(c60coords)

# plot the atoms as black spheres
points3d(c60coords, col = "black")

# plot the atom bonds as gray lines
# (for all atom pairs with a Euclidean distance < 0.66)
for(j in 1:(nrow(c60coords)-1))
{
  for(k in (j+1):nrow(c60coords))
  {
  	if(sqrt(sum((c60coords[j,]-c60coords[k,])^2)) < 0.66)
  	  lines3d(c60coords[c(j,k),], col = "gray", lwd = 1)
  }
}

vrml.close()

# show the output in a web-browser 
# (VRML-plugin must be installed!)
if(file.exists(paste("file://",file.path(outdir,
                "c60example.html"), sep = "")))
{                
  browseURL(paste("file://",file.path(outdir,
                  "c60example.html"), sep = ""))
}

setwd(curdir)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
