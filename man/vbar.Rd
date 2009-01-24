\name{vbar}
\alias{vbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Visualize numerical data and parametric functions in VRML-files}
\description{
  vbar visualizes numerical data as bar charts and terrain plots and parametric functions as 3D-graphs
  in VRML-files.  
}
\usage{
% not more than 65 chars in usage and 
vbar(data, row.labels = rownames(data), col.labels = 
colnames(data), filename = "out.wrl", space = 0.5, cols =
rainbow(length(data)), scalefac = 4, lab.axis = c("X-axis",
"Y-axis", "Z-axis"), col.axis = "white", col.lab = "white",
col.bg = "black", cex.lab = 1, cex.rowlab = 1, cex.collab =
1, navigation = "EXAMINE", fov = 0.785, pos = rep(scalefac +
4, 3), dir = c(-0.59, 0.77, 0.24, 0.99))
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
   as high as to the number of different labels (i.e. length(col)
   >= length(unique(labels)) }
  \item{scalefac}{ a scaling factor for the size of the 3D box in
   which the data will be visualized }
  \item{lab.axis}{ a vector of size 3 containing the axis 
  labels }
  \item{col.axis}{ \code{col.axis} color of the axis }
  \item{col.lab}{ \code{col.lab} color of the axis labels }
  \item{col.bg}{ \code{col.bg} background color }  
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
}
\details{

}
\value{
  The function is used for its side-effect (output of a VRML-file in the specified directory) and has no return value.
}
\references{ }
\author{ Enrico Glaab }
\note{
}
\seealso{ \code{\link{vcloud}} }
\examples{
# visualize volcano data height-map
vbar(volcano)

# bar plot for a data matrix with numerical metalabels
mat <- matrix(9:1, nrow=3)
rownames(mat) <- paste('row',1:3)
colnames(mat) <- paste('col',1:3)
vbar(mat, space=1, col.bg="white", col.axis="blue", col.lab="black")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
