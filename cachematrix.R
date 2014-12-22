## This is R code that demonstrates how lexical scoping works in R
## It also demonstrates the double arrow assignment operator (<<-). Unlike the usual 
## single arrow assignment (<-) that always assigns in the current environment, the 
## double arrow operator will keep looking up the chain of parent environments until 
## it finds a matching name.
## Together, a static parent environment and <<- make it possible to maintain state across function calls.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## cache the matrix inverse in matinv, initialize to NULL
  matinv <- NULL
  
  get <- function() x
  
  ## Double arrow assignment below ensures that the matinv variable in the parent is assigned 
  ## rather than creating a new local variable in this anonymous function, also named matinv
  setinv <- function(inv) matinv <<- inv
  
  getinv <- function() matinv
  
  list(get = get,
       setinv = setinv,
       getinv = getinv)

}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will 
## retrieve the inverse from the cached value matinv in makeCacheMatrix.
cacheSolve <- function(x, ...) {
  
  ## Retrieve and Check for existence of cached matrix inverse
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ##Calculate the inverse of the matrix
  m <- solve(data, ...)
  
  ## Update the cached value of the matrix inverse
  x$setinv(m)
  m
}
