## both functions are based on the example function given in the 
## course description

## the makeCacheMatrix-Function takes a matrix as an argument
## and creates and returns a cacheMatrixobject

# default Value to makeCacheMatrix() is the empty matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # the inverse of the given matrix x
    
    # assigne a Value to our matrixobject
    set <- function(y){ 
        x <<- y
        inv <<- NULL
    }
    # get the Value from our matrixobject
    get <- function() x
    # set the inverse of our matrixobject, named "set_solve"
    # because solve() is the function to calculate the inverse
    set_solve <- function(inverse) inv <<- inverse
    # get the inverse of our matrixobject, named "get_solve"
    # because solve is the function to calculate the inverse
    get_solve <- function() inv
    # returns a list, so you can call the function of our matrixobject 
    # with the "$" notation, eg. makeCacheMatrix(normal_marix)$get_inverse()
    # to make one of our matrixobjects out of "normal_matrix" and then get
    # the inverse of that Matrix
    list(set = set,
         get = get,
         set_solve = set_solve,
         get_solve = get_solve )
}


## "cacheSolve" takes a Argument of type cacheMatrix, as returned
## from the function "makeCacheMatrix" and calculates the inverse
## of the matrix, if the inverse was calculated in the past and the 
## result was chached, no calculation happens, instead
## the cached result is returned

cacheSolve <- function(x, ...) {
    inv <- x$get_solve()
    # check if the inverse was already calculated
    if(!is.null(inv)){
        message("getting cached data")
        # if "yes" the cached result is returned
        return(inv)
    }
    # if no cached result:
    # get the standart matrixobject out of the cacheMatrixobject
    data <- x$get()
    # use the standart solve()-Function to calculate the inverse
    inv <- solve(data, ...)
    # set the result in the cacheMatrixobject
    x$set_solve(inv)
    # return the inverse of the matrix
    inv
}
