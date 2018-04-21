## The makeCacheMatrix function receives a matrix object and either sets
## the inverse or gets the inverse from cache.
## The cacheSolve function performs the inverse of the matrix or if the
## matrix is already inverted, retrieves it from an object stored in memory.

## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(inverse) m <<- inverse
        getMatrixInverse <- function() m
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse, 
             getMatrixInverse = getMatrixInverse)
}


## return inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mCache <- x$getMatrixInverse()
        if (!is.null(mCache)){
                message("Inverse previously calculated...retrieving cached matrix")
                return(mCache)
        }
        mUninverted <- x$get()
        mInverted <- solve(mUninverted)
        x$setMatrixInverse(mInverted)
        mInverted
}

#Test case
m <- makeCacheMatrix()
m$set(matrix(c(1,2,3,4), 2, 2))
m$get()
cacheSolve(m)
cacheSolve(m)