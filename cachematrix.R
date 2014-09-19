## functions that cache the inverse of a matrix


## Creates a matrix that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            x <<- matrix
            i <<- NULL
    }

    ## Method to get the matrix
    get <- function() {
    	## Return the matrix
    	x
    }

    ## Method to set the inverse
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated, 
## then the "cacheSolve" should retrieve the inverse from the cache
cacheSolve <- function(y, ...) {

    ## Return a matrix that is the inverse of y
    x <- y$getInverse()

    ## Return the inverse if its already set
    if( !is.null(x) ) {
            message("getting cached data")
            return(x)
    }

    ## Get the matrix from our data
    data <- y$get()

    ## Calculate the inverse using matrix multiplication
    x <- solve(data) %*% data

    ## Set the inverse to the data
    y$setInverse(x)

    ## Return the matrix
    x
}
