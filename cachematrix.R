## This package contains functions that make objects to set and get different objects in the current environment and
## a "cache" environment to optimize computations. The contained operations are
## 3. makeVector: setting and getting a vector
## 3. cachemean: setting and getting the mean of a vector

## This function creates a special "matrix" object that can cache its inverse which is really a matrix containing
## functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix.

## The function returns a "matrix" object with row names of "matrix" and "inverse" and column names of "set" and "get".
## The "matrix" object has lists as its elements. Each list contains 1 element that is another function. An example
## syntax to call this function is x["matrix", "get"][[1]]().

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <- NULL
     }
     get <- function() x
     setInverse <- function(inverseMatrix) i <<- inverseMatrix
     getInverse <- function() i
     m <- matrix(c(set, setInverse, get, getInverse), 2, 2)
     dimnames(m) <- list(c("matrix", "inverse"), c("set", "get"))
     m
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above (you will need to call
## makeCacheMatrix first to properly use the cacheSolve function). If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     m <- x["inverse", "get"][[1]]()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x["matrix", "get"][[1]]()
     m <- solve(data)
     x["inverse", "set"][[1]](m)
     m
}

## This function creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

## This function calculates the mean of the special "vector" created with the above function. However, it first checks
## to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}