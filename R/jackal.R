##' Mandible lengths of male and female golden jackals
##'
##' Mandible lengths (in mm) for male and female golden jackals (\emph{Canis
##' aureus}) from a collection of specimens in the British Museum of Natural
##' History, London, UK.
##'
##' @name jackal
##' @docType data
##' @format A data frame with 20 observations on the following 2 variables.
##' \describe{ \item{list("Length")}{a numeric vector} \item{list("Sex")}{a
##' factor with levels \code{Male} \code{Female}} }
##' @references Higham, C.F.W., Kijngam, A., and Manly, B.F.J. (1980) An
##' analysis of prehistoric canid remains from Thailand. \emph{Journal of
##' Archaeological Science} \strong{7}:149-165.
##'
##' Manly, B.F.J. (2007) \emph{Randomization, bootstrap and Monte Carlo methods
##' in biology. Third Edition}. Chapman \& Hall/CRC, Boca Raton.
##' @source The data were manually transcribed from Manly (2007).
##' @keywords datasets
##' @examples
##'
##' data(jackal)
##' str(jackal)
##'
##' ## boxplot of mandible length vs sex
##' plot(Length ~ Sex, data = jackal)
##'
NULL
