## The following functions aid matrix inversion. makeCacheMatrix wraps the
## default R matrix with a setter that allows cache invalidation for the
## inverse that it stores as cache. cacheSolve contains the logic to decide
## if cache is available or needs to be calculated.

## makeCacheMatrix takes 'x' as a matrix and returns the inverse of the supplied 
##  matrix. This will always create a new inverse and does not draw from cache.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize Xinverse where cached inverse will be stored.
    Xinverse <- NULL
    
    # set(matrix) - overrides standard setter in order to allow cache
    # invalitation of the Xinverse cache.
    set <- function(y) {
        x <<- y
        Xinverse <<- NULL
    }
    # get() - gets the matrix m
    get <- function() return(x)
    
    # getinverse() returns the cached "Xinverse"
    getinverse <- function() return(Xinverse)
    
    # setinverser(matrix) sets the cache xinverse of the matrix 
    setinverse <- function(inv) Xinverse <<- inv
    
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve takes a matrix 'x' and returns the inverse of the supplied matrix.
## uses the cached value from makeCacheMatrix or sets cache value if cache
## is null.
## See the "testCache" function below to see how to initialize and use these
## function.
cacheSolve <- function(x, ...) {
    # attempt to getinverse().
    inv <- x$getinverse()
    # if getinverse() is not null, then use the cache
    if(!is.null(inv))
    {
        print("Using cached inverse...")
        return(inv)
    }

    print("Calculating inverse (no cache)...")
    # calculate inverse and set cache.
    data <- x$get()
    inv <- x$setinverse(solve(data))
    return(inv)
    
}

# for testing the caching mechanism.
testCache <- function()
{
    # test uncached
    M <- makeCacheMatrix(diag(c(1,2,3)))
    print("testing uncached")
    print("matrix set")
    print(M$get())
    print(cacheSolve(M))
    
    # test cached
    print("testing cached")
    print(cacheSolve(M))
    
    # test cache invalidation by changing matrix
    print("test matrix reset")
    M <- makeCacheMatrix(diag(c(4,5,6)))
    print("matrix set")
    print(M$get())
    print(cacheSolve(M))
}
