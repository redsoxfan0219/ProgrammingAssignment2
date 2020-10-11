## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x, ...) {
        inv = NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (set <<- inverse)
        getInverse <- function() (inv)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("cached data incoming...")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ... )
        x$setInverse(inv)
        inv
}