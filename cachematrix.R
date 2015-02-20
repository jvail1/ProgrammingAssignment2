##Define a matrix and calculate inverse
##Then store inverse matrix in cache

## Define matrix and inverse and store both in cache for cache retrieval test

makeCacheMatrix <- function(x = matrix()) {
  #set this variable to null
  m <- NULL
  #this function creates matrix
  set <- function(y) {
    #store variable in cache
    x <<- y 
    #store this variable in cache as null
    m <<- NULL 
  }
  #this function finds matrix and calculates inverse
  get <- function() x 
  #inverse of original matrix
  #store inverse in cache
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Get inverse of Matrix defined in cacheMatrix
##Look in Cache, if null, then calculate and store in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##defined above and stored in cache
  
    #get inverse of 'x' from cache and check if null
    m <- x$getinverse()
    #if null then calculate using cached value for 'x'
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
