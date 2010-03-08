\name{axis3d}
\alias{axis3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw labeled coordinate axes in a 3D-scene }
\description{
axis3d draws 3 labeled, orthogonal coordinate axes in a 3D-scene
in the VRML- or Livegraphics3D-format. Must be called after
vrml.open() or lg3d.open() and before vrml.close() or lg3d.close().
}
\usage{
axis3d(lab.axis = c("X-axis", "Y-axis", "Z-axis"),
       filename = NULL, type = "vrml", col.lab = "black",
       col.axis = "black", cex.lab = 1, local_scale = 1,
       global_scale = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lab.axis}{a vector of size 3 containing the axis labels}
  \item{filename}{filename of the generated output file}
  \item{type}{the output type ("vrml" or "lg3d"), this will be
    set automatically if mesh3d is called after vrml.open() or
    lg3d.open()}
  \item{col.lab}{a vector of colors specifying the axis labels}
	\item{col.axis}{color of the axis}
  \item{cex.lab}{scaling factor for axis label font size}
  \item{local_scale}{a numerical scaling factor to increase/decrease
   the size of the plotted axes}
  \item{global_scale}{a numerical scaling factor applied to all
   objects in the 3D scene}
}
\details{
	
	axis3d adds colored and labeled coordinate axes to an already existing 3D-
	scene in	the VRML- or Livegraphics3D-format. To create a standard scatter 
	plot, bar plot or 3D mesh visualization, which already contains coordinate 
	axes, the higher-level plotting functions cloud3d, bar3d and mesh3d provide 
	more convenient alternatives. axis3d can only be applied within a VRML- or 
	Livegraphics3D-environment created by calling the vrml.open()- or lg3d.open()-
	function and closed using the vrml.close()- or lg3d.close()-function.
  
}
\value{
  The function is used for its side-effect (writing coordinate axes to a VRML-
  or Livegraphics3D-file) and has no return value.
}
\references{}
\author{ Enrico Glaab }
\note{
}
\seealso{ \link{text3d}, \link{lines3d}}
\examples{


# start a new VRML environment
vrml.open(file="axis_example.wrl", scale=5,
          html.embed="axis.html")

	# draw a coordinate system with dark blue axes
	# and dark gray axis labels
	axis3d(col.lab = "darkgray", col.axis = "darkblue")
	
	# plot random data points within the coordinate system
	# (increase point size by a factor of 2)
	mat <- matrix(runif(99, 0, 1), ncol=3)
	points3d(mat, col="red", scale = 2)

# close the VRML environment and write the output file
vrml.close()

# show the output in a web-browser 
# (VRML-plugin must be installed!)
browseURL(paste("file://",file.path(getwd(),
                "axis.html"), sep=""))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
