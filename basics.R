edge <- seq(-3, 3, length.out = 25)
df <- expand.grid(x = edge, y = edge)
lst <- as.list(surface.df)

mtx <- outer(edge, edge)
mtx[1,1]
mtx[1,2] <- 10
mtx[1,3] <- 11
mtx[2,1] <- 8
mtx[3,1] <- 7

slice <- matrix(c(1,1
                  ,1,2
                  ,1,3)
                , nrow = 3
                , byrow = TRUE)
slice
mtx[slice]


arr <- outer(mtx, mtx)
vec <- as.vector(arr)
arr.slice <- 
arr[1,1,1,1 , drop = FALSE]
