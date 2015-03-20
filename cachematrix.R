## Use lexical scoping to create inverse matrix or use a cached one if it exists

## makeCacheMatrix: returns a "matrix" object that can cache its inverse.
## E.g.: matobj <- makeCacheMatrix(mat) where 'mat' is a matrix object
## matobj$set(): store / overwrite the original matrix 'x'
## matobj$get(): returns the stored original matrix 'x' (non-inverted) 
## matobj$setinv(): stores a matrix into 'inv'. Intended to be an inverted matrix.
## matobj$getinv(): returns the cached inverted matrix 'inv' if one has been cached using cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invmatrix) inv <<- invmatrix
    getinv <- function() inv
    
    # return a list of all the functions in the "matrix" object
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: Returns a the inverse of a special matrix object that has been created using the above function
## First time it's called, x$getinv() is NULL so it will call solve() on the matrix data. 
## on subsequent calls x$getinv() will have been be set, so it it will return 'inv'
## E.g. cacheSolve(matobj)
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

# Full example of how to use these functions:

# create a 4 x 4 matrix of random numbers
# mat <- matrix(rnorm(16), 4, 4)
# 
# cache the matrix in a matrix object called 'matobj'
# matobj <- makeCacheMatrix(mat)
#
# check that the matrix has been stored. Should return a matrix same as 'mat'
# matobj$get()
#
# Get the inverse (will be NULL because it's not set yet)
# matobj$getinv()
# 
# Get the inverse using cacheSolve() 
# First time, matobj$getinv() is NULL so it will call solve() on the matrix data. 
# on subsequent calls it will get from cached 'inv' because it's already set and 'getting cached data' should be displayed
# cacheSolve(matobj)
#
# To check, get the inverse again. This time it has been set by cacheSolve and should return the inverted matrix
# matobj$getinv()
