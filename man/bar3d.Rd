\name{bar3d}
\alias{bar3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw a 3D bar chart}
\description{
bar3d creates 3D barplots and height map visualizationsin the VRML- or 
Livegraphics3D-format.
}
\usage{
bar3d(data, row.labels = rownames(data),
      col.labels = colnames(data), metalabels = NULL,
      filename = "out.wrl", type = "vrml", space = 0.5,
      cols = rainbow(length(as.matrix(data))),
      rcols = NULL, ccols = NULL, origin = c(0, 0, 0),
      scalefac = 4, lab.axis = c("X-axis", "Y-axis", "Z-axis"),
      lab.vertical = FALSE, col.axis = "black",
      showaxis = TRUE, autoscale = TRUE, 
      ignore_zeros = TRUE, col.lab = "black",
      col.bg = "white", cex.lab = 1, cex.rowlab = 1,
      cex.collab = 1, htmlout = NULL, hwidth = 1200,
      hheight = 800, showlegend = TRUE,
      vrml_navigation = "EXAMINE", vrml_transparency = 0,
      vrml_fov = 0.785, vrml_pos = rep(scalefac + 4, 3),
      vrml_dir = c(0.19, 0.45, 0.87, 2.45),
      lg3d_ambientlight = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numerical matrix with 3 columns and n rows}
  \item{row.labels}{a vector containing the row labels
   (strings or numbers)}
  \item{col.labels}{a vector containing the column labels
   (strings or numbers)}
  \item{metalabels}{a vector of strings or numbers containing
    optional metalabels for the rows which can be accessed by
    hovering the mouse over a data point in the plot (in VRML)}
  \item{filename}{filename of the generated output file}
  \item{type}{the output type ("vrml" or "lg3d"), this will be
    set automatically if mesh3d is called after vrml.open() or
    lg3d.open()}
  \item{space}{space between single bars in a bar plot (as a
   fraction of the average bar width)}
  \item{cols}{a vector of colors to visualize different classes
   among the data points. The number of colors should either be
   one or at least as large as the number of different labels
   (i.e. length(col) >= length(unique(labels))}
  \item{rcols}{specifies the bar colors per row of the
   input matrix (the cols- and ccols-parameter can remain
   undefined). The number of colors must correspond to the
   numbers of rows of the data matrix.}
  \item{ccols}{specifies the bar colors per column of the
   input matrix (the cols- and rcols-parameter can remain
   undefined). The number of colors must correspond to the
   numbers of columns of the data matrix.}
  \item{origin}{a vector of length 3 specifying the coordinates
   of the plot origin (can be used to translate the plot)}
  \item{scalefac}{a scaling factor for the size of the entire
   plot}
  \item{lab.axis}{a vector of size 3 containing the axis 
   labels}
  \item{col.axis}{color of the axis}
	\item{lab.vertical}{if TRUE, the data labels will be plotted
	 in vertical instead of horizontal orientation}
  \item{showaxis}{ if FALSE, the coordinate axes are hidden in
   the output}  
	\item{autoscale}{if TRUE, the data is automatically scaled
	 to fill the volume between the axes}
   \item{ignore_zeros}{if TRUE, for height values equal to
   zero no bar will be plotted}    
  \item{col.lab}{ a vector of colors for the axis labels }
  \item{col.bg}{ background color }  
  \item{cex.lab}{ scaling factor for axis label font size }
  \item{cex.rowlab}{ scaling factor for matrix row labels font
   size }
  \item{cex.collab}{ scaling factor for matrix column labels
   font size }
  \item{htmlout}{a filename for generating an HTML-file to embed
   the output }
  \item{hwidth}{width of the embedded visualization in the
   HTML-output }
  \item{hheight}{height of the embedded visualization in the
   HTML-output }
  \item{showlegend}{if TRUE, a data legend will be added to
   the 3D plot}
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
  
  bar3d generates 3D barplots and height map visualizationsin the VRML- or 
  Livegraphics3D-format. The plots can automatically be embedded in a HTML-file 
  to allow users to inspect the data interactively and from different 3D-
  perspectives and scalings on a webpage.
  
  To add additional shapes and objects to the final 3D scene, this plotting 
  function can also be called within a VRML- or Livegraphics3D-environment 
  created by calling the vrml.open()- or lg3d.open()-function. In this case, 
  cloud3d will inherit all global parameters set in the vrml.open- or lg3d.open-
  function (e.g. filename, type, htmlout, etc.) and the user does not need to 
  specify these options anymore.
  
}
\value{
  The function is used for its side-effect (creating a VRML-
  or Livegraphics3D-file) and has no return value.
}
\author{Enrico Glaab}
\seealso{ \link{mesh3d}, \link{cloud3d}}
\examples{

curdir <- getwd()
outdir <- tempdir()
setwd(outdir)

# Height map visualization of geographical data
# with colors corresponding to different height levels
colpalette <- terrain.colors(max(volcano)-min(volcano)+1)
cols <- colpalette[as.vector(t(volcano))-min(volcano)+1]
bar3d(volcano, col.bg="white", cols=cols,
      type="vrml", space=0, showaxis=FALSE,
      filename="volcano.wrl", htmlout="volcano.html")

# show the output in a web-browser 
# (VRML-plugin must be installed!)
if(file.exists(paste("file://",file.path(outdir,
                "volcano.html"), sep="")))
{
  browseURL(paste("file://",file.path(outdir,
                  "volcano.html"), sep=""))
}     

# bar plot for a data matrix with numerical metalabels
mat <- matrix(9:1, nrow=3)
rownames(mat) <- paste('row',1:3)
colnames(mat) <- paste('col',1:3)
bar3d(mat, space=1, metalabels=1:9, col.bg="white",
      col.axis="blue", col.lab="black", type="vrml",
      filename="barplot.wrl", htmlout="barplot.html")
      
# show the output in a web-browser 
# (VRML-plugin must be installed!)
if(file.exists(paste("file://",file.path(outdir,
                "barplot.html"), sep="")))
{
  browseURL(paste("file://",file.path(outdir,
                  "barplot.html"), sep=""))
}

# Height map visualization of very rough and highly
# simplified topographic data for the United Kingdom

data(uk_topo)

bar3d(uk_topo, autoscale=FALSE, cols="blue",
     space=0, showaxis=FALSE, filename = "uk_topo.wrl",
     htmlout="uk_topo.html")

setwd(curdir)      

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
