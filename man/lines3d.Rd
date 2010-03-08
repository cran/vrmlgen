\name{lines3d}
\alias{lines3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw lines in a 3D-scene}
\description{
lines3d draws lines in a 3D-scene in the VRML- or Livegraphics3D-format. Must be called after vrml.open() or lg3d.open()
and before vrml.close() or lg3d.close().
}
\usage{
lines3d(x, y = NULL, z = NULL, col = "black", lwd = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 3-column numeric matrix of coordinates or a numeric vector
   of x-coordinates}
  \item{y}{a numeric vector of y-coordinates (only needed if x is a vector)}
  \item{z}{a numeric vector of z-coordinates (only needed if x is a vector)}
  \item{col}{the color of the text}
  \item{lwd}{a numerical scaling factor specifying the line width}
}
\details{
  
  lines3d adds lines at specified start- and end-points to a 3D-scene in the
  VRML- or Livegraphics3D-format. In the x,y and z coordinate parameters,
  the odd-numbered entries represent the line start-points and the subsequent
  even-numbered entries contain the corresponding end-points.  
  lines3d can only be applied within a VRML- or Livegraphics3D-environment 
  created by calling the vrml.open()- or lg3d.open()-function and closed using 
  the vrml.close()- or lg3d.close()-function.
}
\value{
  The function is used for its side-effect (writing lines to a VRML-
  or Livegraphics3D-file) and has no return value.
}
\author{ Enrico Glaab }
\seealso{ \link{text3d}, \link{points3d}}
\examples{

curdir <- getwd()
outdir <- tempdir()
setwd(outdir)

# This example loads the atom coordinates of a molecule
# (C60, fullerene) and visualizes the molecule in 3D
# using points for the atoms and lines for the atom bonds
# (atom pairs within a given distance threshold).


vrml.open(file="c60.wrl", navigation="EXAMINE",
          html.embed="c60example.html")

# load dataset
data(c60coords)

# plot the atoms as black spheres
points3d(c60coords, col="black")

# plot the atom bonds as gray lines
# (for all atom pairs with a Euclidean distance < 0.66)
for(j in 1:(nrow(c60coords)-1))
{
  for(k in (j+1):nrow(c60coords))
  {
  	if(sqrt(sum((c60coords[j,]-c60coords[k,])^2)) < 0.66)
  	  lines3d(c60coords[c(j,k),], col="gray", lwd=1)
  }
}

vrml.close()

# show the output in a web-browser 
# (VRML-plugin must be installed!)
if(file.exists(paste("file://",file.path(outdir,
                "c60example.html"), sep="")))
{
  browseURL(paste("file://",file.path(outdir,
                  "c60example.html"), sep=""))
}                
                
setwd(curdir)
                
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
