##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #begins by setting the inverse matrix to NULL as a placeholder for a future value
        m <- NULL
        set <- function(y) {
                x <<- y
                #store x as cached              
                m <<- NULL     
        }
        #defines a function to set the vector, x, to a new vector, y,
        #and resets the inverse, m, to NULL
        
        #returns the vector x
        get <- function() x 
        #sets the inverse matrix m
        setinverse <- function(solve) m <<- solve
        #returns the inverse matrix m
        getinverse <- function() m
        #returns the 'special vector' containing all of the functions just defined.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #assign to m the value from inverse matrix above
        m <- x$getinverse()
        #if null then calculate using cached value for 'x'
        if(!is.null(m)) {
                message("getting cached data")
                #if the inverse matrix stored under x is not NULL, return it.
                return(m)
        }
        #else if the inverse matrix is not stored then
        data <- x$get()
        #calculate the inverse matrix and assign it to m
        m <- solve(data, ...)
        #store the inverse matrix m in x
        x$setinverse(m)
        #return m
        return(m)
}