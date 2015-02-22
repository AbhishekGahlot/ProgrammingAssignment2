## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates 4 function which set and get matrix and assign null to inverse using set and get
## Inverse is set to NULL by default
## set resets inverseMatrix to NULL and checks if same matrix is not passed twice
## get fetches original matrix
## getInverse fetches inverse of matrix
## setinverse sets inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(newmatrix){
      #Checks if same matrix is passed via set using identical function
      if (!identical(newmatrix, x)){
        x <<- newmatrix
        inverseMatrix <<- NULL
      }
    }
    get <- function() x
    getInverse <- function() inverseMatrix
    setInverse <- function(x) inverseMatrix <<- solve(x)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## cachesolve fetches original matrix and inverseMatrix (if present) using get function of object 
## calculates inverse of it using solve if inverse is set to NULL else get inverse cache using getInverse function
## get_matrix is getting original matrix
## solve function is used to get inverse of matrix
## set function of object is used to set its inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    ## Checks if inverse already exist then return it
    if(!is.null(inverseMatrix)) {
      message("getting cached data")
      return(inverseMatrix)
    }
    get_matrix <- x$get()
    inverse <- solve(get_matrix)
    x$setInverse(inverse)
    inverse
}

