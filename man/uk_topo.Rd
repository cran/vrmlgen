\name{uk_topo}
\alias{uk_topo}
\docType{data}
\title{ Rough topographic height map for the United Kingdom }
\description{
  A very rough and highly simplified topographic example data set for the United Kingdom
}
% The format is:
% num [1:8966, 1:3] 0.904 0.895 0.882 0.865 0.847 ...
\usage{data(uk_topo)}
\details{
  This dataset contains very rough height map data for the islands
  of the United Kingdom as example input for the bar3d function.
}
\source{  
  The data was obtained from a gray scale topographic image of
  the United Kingdom obtained from \url{http://maps.simutrans.com/europe.html}
  after removing all height values below a threshold chosen by
  visual inspection of the outcome.
}
\keyword{datasets}
