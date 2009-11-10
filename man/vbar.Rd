\name{vbar}
\alias{vbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create bar plots and height maps in VRML-format}
\description{
  vbar visualizes numerical data as bar charts and terrain plots and parametric functions as 3D-graphs
  in VRML-files.  
}
\usage{
vbar(data, row.labels = rownames(data),
col.labels = colnames(data), metalabels = NULL,
filename = "out.wrl",  space = 0.5,
cols = rainbow(length(as.matrix(data))), rcols = NULL,
ccols = NULL, scalefac = 4, lab.axis = c("X-axis",
"Y-axis", "Z-axis"), col.axis = "white",
showaxis = TRUE, col.lab = "white", col.bg = "black",
cex.lab = 1, cex.rowlab= 1, cex.collab = 1,
navigation = "EXAMINE", fov = 0.785, 
pos = rep(scalefac + 4, 3), dir = c(0.19, 0.45, 0.87, 2.45),
htmlout = NULL, hwidth = 1200, hheight = 800,
showlegend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{ a numerical matrix with 3 columns and n rows}
  \item{row.labels}{ a vector containing the row labels
   (strings or numbers) }
  \item{col.labels}{ a vector containing the column labels
   (strings or numbers) }
  \item{metalabels}{ optional metalabels for the rows which
    will appear as popup-text when moving the mouse over a
    data point in the VRML-visualization
   (strings or numbers) }   
  \item{filename}{ filename of the generated VRML output file }
  \item{space}{ space between single bars in a bar plot (as a
   fraction of the average bar width) }
  \item{cols}{ a vector of colors to visualize different classes
   among the data points. The number of colors should be at least
   as large as the number of different labels (i.e. length(col)
   >= length(unique(labels)) }
  \item{rcols}{ specifies the bar colors per row of the
   input matrix. The number of colors must correspond to the
   numbers of rows of the data matrix. }
  \item{ccols}{ specifies the bar colors per column of the
   input matrix. The number of colors must correspond to the
   numbers of columns of the data matrix.}   
  \item{scalefac}{ a scaling factor for the size of the 3D box in
   which the data will be visualized }
  \item{lab.axis}{ a vector of size 3 containing the axis 
  labels }
  \item{col.axis}{ color of the axis }
  \item{showaxis}{ if FALSE, the coordinate axes are hidden in
   the output}  
  \item{col.lab}{ a vector of colors specifying the axis labels }
  \item{col.bg}{ background color }  
  \item{cex.lab}{ scaling factor for axis label font size }
  \item{cex.rowlab}{ scaling factor for matrix row labels font
   size }
  \item{cex.collab}{ scaling factor for matrix column labels
   font size }
  \item{navigation}{ type of mouse navigation in the VRML file,
   can be "EXAMINE", "WALK", "SLIDE", "FLY" or "PAN"}  
  \item{fov}{ a scalar defining the field of view angle in the
   VRML file in radians }
  \item{pos}{ a vector of size 3 for the position of the viewpoint
   in the VRML coordinate system }
  \item{dir}{ a vector of size 4 specifying the viewing direction
   (first 3 components) and the rotation of the camera around the
    direction vector (last component in radians)}
  \item{htmlout}{ a filename for generating an HTML-file to embed the VRML-output }
  \item{hwidth}{ width of the embedded VRML-display in the HTML-output }
  \item{hheight}{ height of the embedded VRML-display in the HTML-output }
  \item{showlegend}{ a logical value indicating whether a legend should
  be drawn on the plot}  
}
\details{
}
\value{
  The function is used for its side-effect (output of a VRML-file in
  the specified directory) and has no return value. The htmlout-parameter
  can be used to embed the resulting VRML-object in a HTML-file.
}
\references{ }
\author{ Enrico Glaab }
\note{
}
\seealso{ \link{vcloud}, \link{lcloud}, \link{lbar} }
\examples{
# visualize volcano data height-map
vbar(volcano)

# bar plot for a data matrix with numerical metalabels
mat <- matrix(9:1, nrow=3)
rownames(mat) <- paste('row',1:3)
colnames(mat) <- paste('col',1:3)
vbar(mat, space=1, metalabels=1:9, col.bg="white", col.axis="blue",
col.lab="black")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
