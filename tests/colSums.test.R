require(colSums)

x <- matrix(1:12, 3,4, dimnames=list(letters[1:3], LETTERS[1:4]))
x[2,2] <- NA
x1 <- c(A=6, B=NA, C=24, D=33)
x2 <- c(A=6, B=10, C=24, D=33)
x3 <- c(a=22, b=21, c=30)
x4 <- c(A=1, B=NA, C=1, D=1)

y <- array(1:24, c(2,3,4), dimnames=list(1:2, letters[1:3], LETTERS[1:4]))
y1 <- c(A=21, B=57, C=93, D=129)

z <- matrix(1:15, 3,5, dimnames=list(letters[1:3], LETTERS[1:5]))
z[1,1] <- z[1,3] <- z[ ,4] <- NA
z[2,2] <- z[2,3] <- 0/0
z1 <- c(A=NA, B=NaN, C=NaN, D=NA, E=42)
z2 <- c(A=5, B=10, C=9, D=0, E=42)

stopifnot(all.equal(colSums(x), x1),
          all.equal(colSums(x, TRUE), x2),
          all.equal(rowSums(x, TRUE), x3),
          all.equal(colVars(x), x4),
          all.equal(colSums(y, dims=2), y1),
          all.equal(colSums(z), z1),
          all.equal(colSums(z, TRUE), z2))
