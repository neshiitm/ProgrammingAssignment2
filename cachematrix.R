## Takes x as argument Matrix Type and provides Inverse of X
## Inv_Matrix is the object which gives the Inverse of X 
## y is the argument used to set the value of x using set function
## get function gives the value of matrix x
## setInverse directly assigns the value of Inverse of X 
## getInverse return the value of the X inverse


makeCacheMatrix <- function(x = matrix()) {

        Inv_Matrix <- NULL
        set <- function(y) {
                x <<- y
                Inv_Matrix <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse)  Inv_Matrix <<- Inverse
        getInverse <- function()  Inv_Matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## If the inverse of X is already calculated the function getInverse 
## in x dataset will return Non Null value and same will be returned 
## else the Solve function will calculate the inverse of R and set 
## the values for makeCatchMatrix as well

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        Inv_Matriks <- x$getInverse()
        if(!is.null(Inv_Matriks)) {
                message("getting cached data")
                return( Inv_Matriks)
        }
        data <- x$get()
        Inv_Matriks <- solve(data)
        x$setInverse(Inv_Matriks)
        Inv_Matriks
}
