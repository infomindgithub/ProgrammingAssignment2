## Put comments here that give an overall description of what your
## functions do
## Caching the Inverse of a Matrix:
## Matrix inversion is usually computationally expensive.
## Caching the inverse of a matrix rather than computing it repeatedly has benefits.
## Write a short comment describing this function
## Two functions are used below to create a special object:
##
## a)The function "makeCacheMatrix" creates a matrix object 
## b)The function "CacheSolve" caches inverse of the matrix.
##
##   Consistent with use of Lexical Scoping in R, 
##     free variables are assigned values available 
##     in the environment in which the function is defined.
##   	
##   Used the <<- operator to assign the argument y to x where the matrix()object x 
##      is obtained from the user's environment; 
##      and also assigned a NULL value to the object inv in the user's environment.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
##
      x <<- y
      inv <<- NULL
    }
##    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
##
## Write a short comment describing this function
  ## The cacheSolve object is assigned a function that computes the inverse 
  ##   of the "matrix" created by makeCacheMatrix above.
  ## If the inverse has been calculated and the 
  ##   matrix is unchanged, then the inverse is retrieved from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv        
}
