\name{points3d}
\alias{points3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw points in a 3D-scene }
\description{
points3d plots data points in a 3D-scene in the VRML- or
Livegraphics3D-format. Must be called after vrml.open()
or lg3d.open() and before vrml.close() or lg3d.close().
}
\usage{
points3d(x, y = NULL, z = NULL, col = "black",
         pointstyle = "s", transparency = 0,
         hyperlinks = NULL, scale = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 3-column numeric matrix of coordinates or a numeric vector
   of x-coordinates}
  \item{y}{a numeric vector of y-coordinates (only needed if x is a vector)}
  \item{z}{a numeric vector of z-coordinates (only needed if x is a vector)}
  \item{col}{the color of the text}
  \item{pointstyle}{"s" for sphere, "b" for box, "c" for cone}
  \item{transparency}{ a number between 0 and 1 specifying the transparency
   level of plotted objects }
  \item{hyperlinks}{ a vector of strings specifying hyperlinks that will be
   triggered, when the user clicks on the corresponding datapoint }
  \item{scale}{ a numerical scaling factor to increase/decrease the point size }
}
\details{
 
  points3d plots data points in different point-styles (sphere, box, cone) in a 
  3D-scene in the VRML- or Livegraphics3D-format. This function can be used to 
  add single data point representations to an already existing 3D scene, or to 
  flexibly combine different plotting styles like points, lines (see lines3d) 
  and text-strings (see text3d) in a 3D environment. To create a standard 
  scatter plot, bar plot or 3D mesh visualization, the higher-level plotting 
  functions cloud3d, bar3d and mesh3d provide more convenient alternatives. 
  points3d can only be applied within a VRML- or Livegraphics3D-environment 
  created by calling the vrml.open()- or lg3d.open()-function and closed using 
  the vrml.\-close()- or lg3d.close()-function.
}
\value{
  The function is used for its side-effect (writing text in a VRML-
  or Livegraphics3D-file) and has no return value.
}
\author{ Enrico Glaab }
\seealso{ \link{text3d}, \link{lines3d}}
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
