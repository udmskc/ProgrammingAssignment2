## Finding the inverse of a square matrix is very expensive.
## If the matrix inverse has been already computed, it can be cached 
## instead of being computed repeatedly. The following functions achieve this

## Create a matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return a matrix that is the inverse of matrix 'x'
## If the inverse has already been computed, return the cached inverse
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    if(!is.null(m)) {
        message("Now, getting cached data ...")
        return(m)
    }
    
    message("This is the first computation.")
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m     
}

## To test use the following commands
## A <- matrix(c(1, 2, -3, -1), nrow=2, ncol=2)
## m <- makeCacheMatrix(A)
## cacheSolve(m)
