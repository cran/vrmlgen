\name{lbar}
\alias{lbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create bar plots and height maps in the LiveGraphics3D-format}
\description{
  vbar visualizes numerical data as bar charts and terrain plots and parametric functions as 3D-graphs
  in LiveGraphics3D-files. Please make sure that the file "live.jar", which can be obtained
from http://www.fktech.eu/fr/enligne/doc-fr/LG3Dfr/LG3DInstallation.htm, is saved in the same
folder as the generated output files.
}
\usage{
lbar(data, row.labels = rownames(data), 
col.labels = colnames(data), filename = "out.m", space = 0.5,
cols = rainbow(length(as.matrix(data))), rcols = NULL,
ccols = NULL, scalefac = 4, lab.axis = c("X-axis",
"Y-axis", "Z-axis"), col.axis = "black", showaxis = TRUE,
col.lab = "white", col.bg = "white", cex.lab = 1.0,
cex.rowlab = 1.0, cex.collab = 1.0, ambientlight = 0.5,
htmlout = NULL, hwidth = 1200, hheight = 800, showlegend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{ a numerical matrix with 3 columns and n rows}
  \item{row.labels}{ a vector containing the row labels
   (strings or numbers) }
  \item{col.labels}{ a vector containing the column labels
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
  \item{ambientlight}{ ambient light gray level (between 0 and 1) }
  \item{htmlout}{ a filename for generating an HTML-file to embed the VRML-output }
  \item{hwidth}{ width of the embedded VRML-display in the HTML-output }
  \item{hheight}{ height of the embedded VRML-display in the HTML-output }
  \item{showlegend}{ a logical value indicating whether a legend should
  be drawn on the plot}  
}
\details{
}
\value{
The function is used for its side-effect (output of a LiveGraphics3D-file
and optionally an HTML-page in the specified directory) and has no return
value. The htmlout-parameter can be used to embed the resulting
LiveGraphics3D object in a HTML-file.
}
\references{
Martin Kraus. LiveGraphics3D - a non-commercial Java 1.1 applet. URL: \url{http://wwwvis.informatik.uni-stuttgart.de/~kraus/index.html}
}
\author{ Enrico Glaab }
\note{
}
\seealso{ \link{vbar}, \link{vcloud}, \link{lcloud} }
\examples{
# visualize volcano data height-map
lbar(volcano, htmlout="result.html", showaxis=FALSE)

# bar plot for a data matrix with numerical metalabels
mat <- matrix(9:1, nrow=3)
rownames(mat) <- paste('row',1:3)
colnames(mat) <- paste('col',1:3)
lbar(mat, space=1, col.bg="white", col.axis="blue", col.lab="black",
htmlout="result.html")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
