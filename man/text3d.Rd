\name{text3d}
\alias{text3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw text in a 3D-scene }
\description{
text3d plots text in a 3D-scene in the VRML- or Livegraphics3D-format. Must be called after vrml.open() or lg3d.open()
and before vrml.close() or lg3d.close().
}
\usage{
text3d(x, y = NULL, z = NULL, text, col = "black",
       fontweight = "normal", fontfamily = "sans",
       hyperlink = NULL, rot = c(0, 1, 0, 0), scale = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 3-column numeric matrix of coordinates or a
   numeric vector of x-coordinates}
  \item{y}{a numeric vector of y-coordinates (only needed if
   x is a vector)}
  \item{z}{a numeric vector of z-coordinates (only needed if x
   is a vector)}
  \item{text}{ a vector of strings which are to be plotted (size
   must correspond to the number of coordinates specified in the
   x-, y-, and z-parameters}
  \item{col}{ the color of the text}
  \item{fontweight}{ "normal" or "bold" }
  \item{fontfamily}{ "sans", "serif" or "typewriter"}
  \item{hyperlink}{ a string containing a hyperlink to a website}
  \item{rot}{ a numeric vector of length 4, the first 3
   components specify a rotation axis and the last component is
   the rotation angle in radians}
  \item{scale}{ a numerical scaling factor to increase/decrease
   the text size }
}
\details{

 text3d draws one or multiple text strings in a 3D-scene in the VRML- or 
 Livegraphics3D-format. This function can be used to add additional labels to 
 plots or to attach textual information to a 3D-shape or object. text3d can only 
 be applied within a VRML- or Livegraphics3D-environment created by calling the 
 vrml.open()- or lg3d.open()-function and closed using the vrml.close()- or 
 lg3d.close()-function.
 
}
\value{
  The function is used for its side-effect (writing text in a VRML-
  or Livegraphics3D-file) and has no return value.
}
\author{ Enrico Glaab }
\seealso{ \link{points3d}, \link{lines3d}  }
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
