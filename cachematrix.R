## few functions that cache the inverse of a matrix
## makeCacheMatrix:creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

  ## to initialize the inverse property
  m<-NULL
  
  ## to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## to get the matrix
  get <- function() {
    ## will return the matrix
    x
  }
  ## to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## to get the inverse of the matrix
  getInverse <- function() {
    ## will return the inverse property
    m
  }
  ## will return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        ## will return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## will return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## to get the matrix from our object
        data <- x$get()
        ## inverse will be calculated using matrix multiplication
        m <- solve(data) %*% data
        ## will set the inverse to the object
        x$setInverse(m)
        ## will return the matrix
        m 
  
}
