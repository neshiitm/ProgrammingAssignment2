## makeCatcheMatriix takes x as argument and provides Inverse of X
## InverseX is the object which gives the Inverse of X 
## y is the value used to set the value of x using set function
## get function gives the value of matrix x
## setInverse directly assigns the value of Inverse of X 
## getInverse return the value of the X inverse


makeCacheMatrix <- function(x = matrix()) {

        InverseX <- NULL
        set <- function(y) {
                x <<- y
                InverseX <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) InverseX <<- Inverse
        getInverse <- function() InverseX
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## If the inverse of X is already calculated the function getInverse 
## in x data set will return Non Null value and same will be returned 
## else the Solve function will valculate the inverse of R and set 
## the values for makeCatchMatrix as well

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        InverseX <- x$getInverse()
        if(!is.null(InverseX)) {
                message("getting cached data")
                return(InverseX)
        }
        data <- x$get()
        InverseX <- solve(data)
        x$setInverse(InverseX)
        InverseX
}



