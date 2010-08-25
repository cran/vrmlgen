\name{lg3d.open}
\alias{lg3d.open}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Livegraphics3D output device system}
\description{
lg3d.open creates a new Livegraphics3D-environment in which primitive and 
higher-level plotting functions can be combined together to create a 
scene in the Livegraphics3D-format.
}
\usage{
lg3d.open(filename = "out.m", col = "white", scale = 1,
          html.embed = "out.html", hwidth = 1200,
          hheight = 800, ambientlight = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{filename}{ filename of the generated Livegraphics3D
	 output file }
  \item{col}{ background color }
  \item{scale}{ a scaling factor for the size of the entire
   3D scene }
  \item{html.embed}{ a filename for generating an HTML-file
   to embed the VRML-output }
  \item{hwidth}{ width of the embedded visualization in the
   HTML-output }
  \item{hheight}{ height of the embedded visualization in the
   HTML-output }  
  \item{ambientlight}{ambient light gray level (value between
   0 and 1, LG3D only)}
}
\details{
  
  This function creates a new Livegraphics3D-environment to provide access to 
  primitive plotting functions like points3d, lines3d and text3d and to combine 
  multiple plotting functions together (both primitive and higher-level 
  functions). After creating the environment, all primitive and higher-level 
  functions called afterwards will automatically inherit the higher-level 
  settings (background color, navigation-type, etc.) specified in the lg3d.open-
  parameters. In order to close the environment and create the plot, the 
  lg3d.close() function has to be called.  
  
}
\value{  
   The function is used for its side-effect (output of a Livegraphics3D-file in 
   the specified directory) and has no return value. The htmlout-parameter can 
   be used to embed the resulting Livegraphics3D-object in a template HTML-file.  
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

