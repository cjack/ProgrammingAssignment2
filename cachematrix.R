## This function generates a special "matrix" object can cache its inverse
## Input: default value is a matrix object
## Ouput: the special "matrix"

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() return(x)
        setInverse <- function(solveValue) inverse <<- solveValue
        getInverse <- function() return(inverse)
        return(list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse))
        
}

## This function return th inverse of the special "matrix"
## The main point is to retrieve the target matrix object's inverse matrix
## If it is not a null value, then return it
## Otherwise, calculate the inverse matrix, cache and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result <- x$getInverse()
        
        if(!is.null(result)) {
                message("Getting cached data")
                return(result)
        } else{
                data <- x$get()
                result <- solve(data, ...)
                x$setInverse(result)
                return(result)
        }
}