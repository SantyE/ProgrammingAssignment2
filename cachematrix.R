## The function makeCacheMatrix and cacheSolve work together to compute the inverse
## of a given matrix and cache it for future use.


## makeCacheMatrix takes a matrix input. It returns a list of objects containing
## the function values of set, get, setinv, and getinv. Set sets the value of x.
## get retrieves the value of x. setinv sets the value for the inverse of x.
## getinv retrieves the value of the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve takes the list returned by makeCacheMatrix and checks the necessary objects
## to see if the inverse has already been calculated. If it has, the function outputs a 
## message notifying the user that the value has been cached previously and is being
## retrieved. If it hasn't, the function calculates the inverse and then caches it for 
## future use.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv      
}