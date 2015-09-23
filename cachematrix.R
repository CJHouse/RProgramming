## makeCacheMatrix and cacheSolve are functions created in R version 3.2.0 (2015-04-16)
## Author: Chris House
## Acknowledgements: R.D. Peng and support staff of R Programming, 
## Johns Hopkins Bloomberg School of Public Health.

## makeCacheMatrix is a function which contains a list containing the functions that set 
## and get the value of the matrix and the get and set the inverse of the matrix.
## You use makeCacheMatrix in the following way:
## 1) Create a square matrix (e.g., x <- rbind(c(1,0), c(-1,1)))
## 2) Set inv (e.g., inv <- makeCacheMatrix(x))
## 3) Solve the inverse (e.g., cacheMatrix(inv))

## NOTE: if x is not a square matrix the function will identify these matrices and return a message.
## e.g., try x <- rbind(c(1,0,1), c(1,1,0))

makeCacheMatrix <- function(x = matrix()) {
      
      ## test the matrix is square.  Non-square matrices return error message.
      msg <- try(solve(x), silent=TRUE)
      if(class(msg) == "try-error") { message("In order to return an inverse matrix the original matrix must be a square matrix.")}
      
      ## prepares variables
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      
      ## solve() calculates the inverse of a square matrix.
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function that calculates the inverse of a matrix created with makeCacheMatrix function.
## It first checks to see if the inverse of the matrix already exists.  If it does not exist, then the 
## function calculates the inverse of the matrix and caches this value for later use.  If present, it returns
## the value of the inverse of the matrix.

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      ## checks to see if the inverse matrix exists first
      inv <- x$getinv()
      
      ## if the inv matrix exists already - returns the inv value from cache
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## if the inv matrix does not exist - returns the inv matrix it calculates
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
