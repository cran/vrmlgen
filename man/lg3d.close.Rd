\name{lg3d.close}
\alias{lg3d.close}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Livegraphics3D output device system}
\description{
lg3d.close terminates a Livegraphics3D-environment and writes the corresponding
3D-scene to a Livegraphics3D-file.
}
\usage{
lg3d.close()
}
%- maybe also 'usage' for other objects documented here.
\details{  
  This function closes a Livegraphics3D-environment created with lg3d.open() and 
  writes the Livegraphics3D-plot to the file and directory specified in the 
  lg3d.open- parameters. For more details, see the description of the lg3d.open-
  function.  
}
\value{
  The function is used for its side-effect (output of a Livegraphics3D-file 
  in the specified directory) and has no return value.
}
\author{ Enrico Glaab }
\references{
	
	Enrico Glaab, Jonathan M. Garibaldi, Natalio Krasnogor (2010).  vrmlgen: An R Package for
	3D Data Visualization on the Web. \emph{Journal of Statistical Software, 36(8)}, p. 1-18.
	URL: \url{http://www.jstatsoft.org/v36/i08/}
		
}
\seealso{ \link{vrml.open} }
\examples{


curdir <- getwd()
outdir <- tempdir()
setwd(outdir)


# This example loads the atom coordinates of a molecule
# (C60, fullerene) and visualizes the molecule in 3D
# using points for the atoms and lines for the atom bonds
# (atom pairs within a given distance threshold).

lg3d.open(file = "c60.mat", html.embed = "c60example.html")

# load dataset
data(c60coords)

# plot the atoms as black spheres
points3d(c60coords, col = "black", scale = 2)

# plot the atom bonds as gray lines
# (for all atom pairs with a Euclidean distance < 0.66)
for(j in 1:(nrow(c60coords)-1))
{
  for(k in (j+1):nrow(c60coords))
  {
  	if(sqrt(sum((c60coords[j,]-c60coords[k,])^2)) < 0.66)
  	  lines3d(c60coords[c(j,k),], col = "gray", lwd = 2)
  }
}

lg3d.close()


# show the output in a web-browser 
# (Java must be enabled!)
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
