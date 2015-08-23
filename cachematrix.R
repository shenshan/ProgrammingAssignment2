## Two functions caching the inverse of a matrix


## Creates a special "matrix" object that can cache its
## inverse
makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y){
            x <<- y
            invM <<- NULL
        }
        get <- function() x
        setInvMat <- function(invMat) invM <<- invMat
        getInvMat <- function() invM
        list(set = set, get = get, 
             setInvMat = setInvMat,
             getInvMat = getInvMat)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInvMat()
        if(!is.null(invM)){
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setInvMat(invM)
        invM
}
