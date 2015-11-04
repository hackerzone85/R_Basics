## Here are two functions. 
## The purpose is to cache the inverse of the matrix...
## ...so that it only needs to be calculated once and can be retrieved...
## ...until the matrix is changed, because setting a new value will
## ...NULLify the current value held for the inverse

## This first function takes a matrix and returns a list of functions.
## Among this list is a set function for replacing the value of the matrix...
## ...and a get function for returning the current value of the matrix.
## Simlilarly a setinv and getinv function that will accept or return...
## ...an additional value. This really could be anything, but here we use it to hold...
## ... the solved/inverse of the matrix value

makeCacheMatrix <- function(x = matrix()) {
  
  # initial value
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  
  get <- function() x
  
  setinv <- function(invert) inv <<- invert
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## This function will only accept an instance of one of our lists created by...
## ...the makeCacheMatrix function above. This is because....
## ...it calls the four functions of this specific construct.
## The first thing it does is to call getinv, to get the current value of inv.
## If this is not NULL, it simply returns it.
## Otherwise, it calls get(), runs the result through solve...
## ...then it sets the result using setinv before returning it.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
    matr <- x$get()
    inv <- solve(matr)
    x$setinv(inv)
    inv
}