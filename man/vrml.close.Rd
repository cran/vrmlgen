\name{vrml.close}
\alias{vrml.close}
\title{VRML output device system}
\description{
vrml.close terminates a VRML-environment and writes the corresponding 3D-scene 
to a VRML file.
}
\usage{
vrml.close()
}
\details{  
  
  This function closes a VRML-environment created with vrml.open() and writes 
  the VRML-plot to the file and directory specified in the vrml.open-
  parameters. For more details, see the description of the vrml.open-function.
  
}
\value{
  The function is used for its side-effect (output of a VRML-file 
  in the specified directory) and has no return value.
}
\author{ Enrico Glaab }
\seealso{ \link{vrml.open} }
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
