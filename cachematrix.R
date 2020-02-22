## Caching the inverse of a matrix

## makeCacheMatrix is a function that creates a special "matrix", which is really
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Below function calculates the inverse of the matrix created with above function
## If the matrix has already been calculated, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
