\name{jackal}
\alias{jackal}
\docType{data}
\title{
  Mandible lengths of male and female golden jackals
}
\description{
  Mandible lengths (in mm) for male and female golden jackals
  (\emph{Canis aureus}) from a collection of specimens in the British
  Museum of Natural History, London, UK.
}
\usage{data(jackal)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Length}}{a numeric vector}
    \item{\code{Sex}}{a factor with levels \code{Male} \code{Female}}
  }
}
%\details{
%
%}
\source{
  The data were manually transcribed from Manly (2007).
}
\references{
  Higham, C.F.W., Kijngam, A., and Manly, B.F.J. (1980) An analysis of
  prehistoric canid remains from Thailand. \emph{Journal of
    Archaeological Science} \strong{7}:149-165.
  
  Manly, B.F.J. (2007) \emph{Randomization, bootstrap and Monte Carlo
    methods in biology. Third Edition}. Chapman & Hall/CRC, Boca Raton.
}
\examples{
data(jackal)
str(jackal)

## boxplot of mandible length vs sex
plot(Length ~ Sex, data = jackal)
}
\keyword{datasets}
