colSums <- function(x, na.rm=FALSE, dims=1) {
  if (is.data.frame(x)) x <- data.matrix(x)
  if (is.complex(x))                                             # Complex case
    return(Recall(Re(x), na.rm, dims) + 1i*Recall(Im(x), na.rm, dims))
  if (is.matrix(x) && dims==1) return(.Call("colSums", x, na.rm)) # Simple case
  if (dims==0) return(x)                                   # Don't sum any dims
  dx <- dim(x)
  if (length(dx) <= dims) return(sum(x, na.rm=na.rm))            # Sum all dims
  dn <- dimnames(x)[-(1:dims)]
  dim(x) <- c(prod(dx[1:dims]), prod(dx[-(1:dims)]))      # Convert to a matrix
  res <- .Call("colSums", x, na.rm)                  # colSums returns a vector
  if (length(dx)-dims==1) names(res) <- dn[[1]] else {
    dim(res) <- dx[-(1:dims)]                            # Convert res to array
    dimnames(res) <- dn
  }
  res
}

rowSums <- function(x, na.rm=FALSE, dims=1) {
  if (is.data.frame(x)) x <- data.matrix(x)
  if (is.complex(x))                                             # Complex case
    return(Recall(Re(x), na.rm, dims) + 1i*Recall(Im(x), na.rm, dims))
  if (is.matrix(x) && dims==1) return(.Call("rowSums", x, na.rm)) # Simple case
  if (dims==0) return(sum(x, na.rm=na.rm))                       # Sum all dims
  dx <- dim(x)
  if (length(dx) <= dims) return(x)                        # Don't sum any dims
  dn <- dimnames(x)[1:dims]
  dim(x) <- c(prod(dx[1:dims]), prod(dx[-(1:dims)]))      # Convert to a matrix
  res <- .Call("rowSums", x, na.rm)                  # rowSums returns a vector
  if (dims==1) names(res) <- dn[[1]] else {
    dim(res) <- dx[1:dims]                               # Convert res to array
    dimnames(res) <- dn
  }
  res
}

colMeans <- function(x, na.rm=FALSE, dims=1)
  colSums(x, na.rm, dims) / colSums(!is.na(x), FALSE, dims)
rowMeans <- function(x, na.rm=FALSE, dims=1)
  rowSums(x, na.rm, dims) / rowSums(!is.na(x), FALSE, dims)

colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
                     sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}
rowVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(rowSums(x^2, na.rm, dims))
  N <- rowSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==0) x - mean(x, na.rm=na.rm) else
                     sweep(x, 1:dims, rowMeans(x,na.rm,dims))}
  (rowSums(x^2, na.rm, dims) - rowSums(x, na.rm, dims)^2/N) / Nm1
}
colStdevs <- function(x, ...) sqrt(colVars(x, ...))
rowStdevs <- function(x, ...) sqrt(rowVars(x, ...))
