\name{colSums}
\alias{colSums}
\alias{rowSums}
\alias{colMeans}
\alias{rowMeans}
\alias{colVars}
\alias{rowVars}
\alias{colStdevs}
\alias{rowStdevs}
\title{Column and Row Summaries}
\description{Sums, means, variances, or standard deviations by column
             (or row) of an array.}
\usage{
  colSums (x, na.rm=FALSE, dims=1)
  rowSums (x, na.rm=FALSE, dims=1)
  colMeans(x, na.rm=FALSE, dims=1)
  rowMeans(x, na.rm=FALSE, dims=1)
  colVars (x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
           twopass=FALSE)
  rowVars (x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
           twopass=FALSE)
  colStdevs(x, ...)
  rowStdevs(x, ...)
}
\arguments{
  \item{x}{A numeric array (or a dataframe to convert to a matrix).}
  \item{na.rm}{Logical: Remove NA's?}
  \item{dims}{Number of dimensions to sum over [colSums] or leave alone
              [rowSums].  Only useful when x is a multidimensional array.}
  \item{unbiased}{Logical: Use (N-1) in the denominator when calculating
                  variance?}
  \item{SumSquares}{Logical: If TRUE, colVars just returns sums of squares.}
  \item{twopass}{Logical: If TRUE, colVars uses the corrected two-pass
     algorithm of Chan Golub & LeVeque, which is slower but less subject
     to roundoff error.}
  \item{...}{colStdevs/rowStdevs take the same arguments as colVars/rowVars.}
}
\details{On a matrix:

  colSums (x, na.rm) == apply(x, 2, sum,  na.rm=na.rm)

  rowSums (x, na.rm) == apply(x, 1, sum,  na.rm=na.rm)

  colMeans(x, na.rm) == apply(x, 2, mean, na.rm=na.rm)

  rowMeans(x, na.rm) == apply(x, 1, mean, na.rm=na.rm)

  colVars (x, na.rm) == apply(x, 2, var,  na.rm=na.rm)

  rowVars (x, na.rm) == apply(x, 1, var,  na.rm=na.rm)
}
\value{
  A vector or array with dimensionality length(dim(x))-dims [colSums] or
  dims [rowSums].  Dimnames of the remaining dimensions are preserved.
  When the result is 1-dimensional, it is always demoted to a vector.
}
\author{Originally by Douglas Bates <bates@stat.wisc.edu> as package
       "MatUtils".  Modified, expanded, and renamed by David Brahm
       <brahm@alum.mit.edu>, with help of course from the R-help gurus.}
\note{Detection of NA's (and NaN's) in the C code uses a trick (x==x)
      that may not work on all platforms.

      This package will probably be superseded by built-in functions in
      R-1.5.0.}
\seealso{\code{\link{apply}}}
\examples{
x <- matrix(1:12, 3,4, dimnames=list(letters[1:3], LETTERS[1:4]))
x[2,2] <- NA
colSums(x)
colSums(x, na.rm=TRUE)
rowSums(x, na.rm=TRUE)
colVars(x)

y <- array(1:24, c(2,3,4), dimnames=list(1:2, letters[1:3], LETTERS[1:4]))
colSums(y, dims=1)
rowSums(y, dims=1)
colSums(y, dims=2)
rowSums(y, dims=2)
colMeans(y, dims=1)
}
\keyword{array}
\keyword{arith}
