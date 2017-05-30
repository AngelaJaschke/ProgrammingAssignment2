## First, a matrix is converted into a special data structure (which can cache its inverse) via makeCacheMatrix
## Then, the second function (cacheSolve) computes the inverse of a matrix in the special data structure, 
## retrieving it from the cache if it has already been computed.

## We convert an input matrix x into a special data structure with 4 attribute functions set, get, setinv and getinv:
## set sets (changes) the value of the matrix
## get obtains the value of the matrix
## setinv sets the variable inv to be the inverse of the matrix
## getinv returns the value of the inverse, inv, which is NULL by default. 
## the values x (the matrix itself) and inv (the inverse) are also part of this data structure
## and can be accessed later because of lexical scoping without being defined globally.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes as input the special data structure from above and checks whether the inverse has already been 
## calculated, i.e., is not NULL. If so, it returns the value. Otherwise, it calculates the inverse and saves it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
