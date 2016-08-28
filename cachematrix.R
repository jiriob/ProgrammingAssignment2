## makeCacheMatric:
# The function wil make a special list of function similair to the example
#   , only to matrices and with the inverse to. So,the function will return:
# set the matrix, get the matrix, set the inverse and get the inverse 


makeCacheMatrix <- function(x = matrix()) { ##  funtion with matrix as input
    mInv <- NULL ## matrix inverse, unassigned
    set <- function(y) {
        x <<- y # assign value in a different environment
        mInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mInv <<- inverse ## assign to different environment
    getinverse <- function() mInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cachsolve:
# function which first checkes if the inverse is already defined in the cach 
# if not calculates that inverse
# always returns the inverse of the matrix 

cacheSolve <- function(x, ...) {
    # check if the inverse is already calculated
    mInv <- x$getinverse()
    if(!is.null(mInv)) {
        message("getting inverse from cached data")
        return(mInv) # return the inverse of the matrix if it already exist in the cache
    }
    # if it's not the cache, the inverse of the matrix is calculated  
    data <- x$get() # get matrix
    
    mInv <- solve(data) # calculate inverse
    
    x$setinverse(mInv) # cache result
    
    mInv  ## Return a matrix that is the inverse of matrix 'x'
}
