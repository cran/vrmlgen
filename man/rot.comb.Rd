\name{rot.comb}
\alias{rot.comb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Combine rotation vectors }
\description{
 rot.comb computes the combined rotation vector for two input rotation vectors.
}
\usage{
rot.comb(rotvec1 = c(1, 0, 0, pi/2), rotvec2 = c(0, 0, 1, pi))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rotvec1}{ a numeric vector of length 4, the first 3 components specify
    a rotation axis and the last component is the rotation angle in radians}
  \item{rotvec2}{ a numeric vector of length 4, the first 3 components specify
    a rotation axis and the last component is the rotation angle in radians }
}
\details{
 The rot.comb helper function computes the combined rotation vector for two
 input rotation vectors representing the sequential rotation of a 3D-object
 around two coordinate axis. In each rotation vector, the first 3 components
 represent the rotation axis (for the input vectors, this can be c(1,0,0)
 for the x-axis, c(0,1,0) for y-axis or c(0,0,1) for the z-axis), and the
 last component represent the rotation angle in radians.
 The computation is achieved by multypling the quaternion-representations
 of the input vectors. Please note that this multiplication is not commutative,
 i.e. the order of the input rotation vectors is important.
 
}
\value{
  Returns a numeric vector of length 4, where the first 3 components
  represent a rotation axis and the last component is rotation angle in radians.
}
\author{ Enrico Glaab }
\examples{

# compute the combined rotation vector for
# 1) a 90-degree rotation around the y-axis, and
# 2) a 180-degree rotation around the z-axis

rot.comb(c(0,1,0,pi/2), c(0,0,1,pi))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ misc }
