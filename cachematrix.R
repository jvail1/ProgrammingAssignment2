##Define a matrix and calculate inverse
##Then store both matrices in cache for cache retrieval test

##Function takes a matrix and caches it
makeCacheMatrix <- function(x = matrix()) {
        ##set this variable to null
        m <- NULL
        set <- function(y) {
                x <<- y 
                ##store x as cached
                m <<- NULL 
        }
        ##this section takes matrix from above and calculates inverse
        get <- function() x 
        ##inverse of original matrix
        ##store inverse in cache
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
        
        ##get inverse of 'x' from cache and check if null
        m <- x$getinverse()
        ##if null then calculate using cached value for 'x'
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##Set data to cached matrix
        data <- x$get()
        ##Get inverse of data
        m <- solve(data, ...)
        ##Set inverse in cache
        x$setinverse(m)
        m
}
