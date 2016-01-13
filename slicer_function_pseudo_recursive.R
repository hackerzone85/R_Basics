slicer <- function(v, t, d) {
  b <- 0
  for (i in 1:d) {
    frac <- t * v
    v <- v - frac
    b <- b + frac
    print(c(i, a, p))
  }
  return(b)
}

my <- slicer(1, 0.1, 50)
