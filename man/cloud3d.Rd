\name{cloud3d}
\alias{cloud3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw a 3D scatter plot}
\description{
 cloud3d creates dynamic 3D scatter plot visualizations in the VRML- or Livegraphics3D-format.
}
\usage{
cloud3d(x, y = NULL, z = NULL, labels = rownames(data),
        filename = "out.wrl", type = "vrml",
        pointstyle = c("s", "b", "c"), metalabels = NULL,
        hyperlinks = NULL, cols = rainbow(length(unique(labels))),
        scalefac = 4, autoscale = "independent",
        lab.axis = c("X-axis", "Y-axis", "Z-axis"),
        col.axis = "black", showaxis = TRUE, col.lab = "black",
        col.bg = "white", cex.lab = 1, htmlout = NULL,
        hwidth = 1200, hheight = 800, showlegend = TRUE,
        vrml_navigation = "EXAMINE", vrml_showdensity = FALSE, 
        vrml_fov = 0.785, vrml_pos = rep(scalefac + 4, 3),
        vrml_dir = c(0.19, 0.45, 0.87, 2.45),
        vrml_transparency = 0, lg3d_ambientlight = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a 3-column numeric matrix of coordinates or a numeric
   vector of x-coordinates (not used if parameter obj\_infile or
   xfun is specified)}
  \item{y}{a numeric vector of y-coordinates (only needed if x is
   a vector)}
  \item{z}{a numeric vector of z-coordinates (only needed if x is
   a vector)}
  \item{labels}{a vector of size n containing optional class labels
   (strings or numbers) for the data points}
  \item{filename}{filename of the generated output file }
  \item{type}{the output type ("vrml" or "lg3d"), this will be
    set automatically if mesh3d is called after vrml.open() or
    lg3d.open()}
  \item{pointstyle}{a vector of point style types. Possible types
   are "s" for spheres, "b" for boxes and "c" for cone. To draw
   all points in the same style, use a vector of size 1, otherwise
   the number of used point styles must be greater or equal to the
   number of different labels (i.e. length(pointstyles) >=
   length(unique(labels)))}
  \item{metalabels}{ a vector of strings or numbers containing
    optional metalabels for the rows which can be accessed by
    hovering the mouse over a data point in the plot (in VRML)}
  \item{hyperlinks}{a vector of strings specifying hyperlinks that
   will be triggered, when the user clicks on the corresponding
   datapoint}
  \item{cols}{a vector of colors to visualize different classes
  among the data points. The number of colors should either be
  one or at least as large as the number of different labels
  (i.e. length(col) >= length(unique(labels))}
  \item{scalefac}{a numerical scaling factor to increase/decrease
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
   the VRML-output }
  \item{hwidth}{ width of the embedded visualization in the
   HTML-output }
  \item{hheight}{ height of the embedded visualization in the
   HTML-output }
  \item{showlegend}{if TRUE, a data legend will be drawn based on the
   group labels}
  \item{vrml_navigation}{type of mouse navigation in the VRML file,
   can be "EXAMINE", "WALK", "SLIDE", "FLY" or "PAN" (VRML only)}
  \item{vrml_showdensity}{if TRUE, semi-transparent density estimation
   contour surfaces will be drawn to identify regions of high data
   density (requires misc3d-package)}
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
  
  cloud3d creates dynamic 3D-scatterplots with optional features
  like density estimation contour surfaces and hyperlinks for each
  datapoint in the VRML- or Livegraphics3D-format. The plots can
  automatically be embedded in a HTML-file to allow users to
  inspect the data interactively and from different 3D-perspectives
  and scalings on a webpage.
  
  To add additional shapes and objects to the final 3D scene, this plotting 
  function can also be called within a VRML- or Livegraphics3D-environment 
  created by calling the vrml.open() or lg3d.open() function. In this case, 
  cloud3d will inherit all global parameters set in the vrml.open- or lg3d.open-
  function (e.g. filename, type, htmlout, etc.) and the user does not need to 
  specify these options anymore.
  
}
\value{
  The function is used for its side-effect (creating a VRML-
  or Livegraphics3D-file) and has no return value.
}
\references{
Bowman, A. W. and Azzalini, A. (2007). R package 'sm': \emph{Nonparametric smoothing methods} (version 2.2) URL: \url{http://www.stats.gla.ac.uk/~adrian/sm}, \url{http://azzalini.stat.unipd.it/Book_sm}

Feng, D. and Tierney, L. (2009). R package 'misc3d": \emph{Miscellaneous 3D plots} (version 0.6-1) URL: \url{http://cran.r-project.org/web/packages/misc3d/index.htm}
	
Enrico Glaab, Jonathan M. Garibaldi, Natalio Krasnogor (2010).  vrmlgen: An R Package for 3D Data Visualization on the Web. \emph{Journal of Statistical Software, 36(8)}, p. 1-18. URL: \url{http://www.jstatsoft.org/v36/i08/}
	
}

\author{Enrico Glaab}
\seealso{ \link{mesh3d}, \link{bar3d}}
\examples{


curdir <- getwd()
outdir <- tempdir()
setwd(outdir)


# example 1: visualize random 3D input data
mat <- matrix(runif(99, 0, 3), ncol = 3)

# create random class assignment vector with three classes
y <- round(runif(33, 0, 2))
y <- ifelse(y == 0, "class 1", ifelse(y == 1, "class 2", "class 3"))

# create ouput using numbers from 1 to length(y) as metalabels
cloud3d(mat, labels = y, metalabels = 1:length(y), col.axis = "black",
        col.lab = "blue", col.bg = "white", type = "vrml",
        filename = "result.wrl", htmlout = "result.html")

# show the output in a web-browser 
# (VRML-plugin must be installed!)
if(file.exists(paste("file://",file.path(outdir,
                "result.html"), sep = "")))
{
  browseURL(paste("file://",file.path(outdir,
                  "result.html"), sep = ""))
}


# example 2: visualization of Edgar Anderson's Iris data
# using density estimation contour surfaces (requires misc3-package!)
irismat <- iris[,1:3]
cloud3d(irismat, labels = iris[,5], vrml_showdensity = TRUE,
        col.axis = "blue", col.lab = "black", lab.axis = colnames(irismat),
        type = "vrml", filename = "result2.wrl", htmlout = "iris.html")

# show the output in a web-browser 
# (VRML-plugin must be installed!)
if(file.exists(paste("file://",file.path(outdir,
                "iris.html"), sep = "")))
{
  browseURL(paste("file://",file.path(outdir,
                  "iris.html"), sep = ""))
}
 
setwd(curdir)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
