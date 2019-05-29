## The pair of functions -- makeCacheMatrix() and cacheSolve(), cache the inverse of a matrix
## These functions take advantage of the scoping rules of the R language
## They show how scoping rules can be manipulated to preserve state inside of an R object
## For this, <<- operator has been used to assign a value to an object in an environment 
## that is different from the current environment 

## Done as part of R Programming Week 3 Assignment by Umme Juka Mobassera


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL   # at first, the inverse matrix is set to NULL
  # function to set the data matrix
  set <- function(y) {  
    # set the data matrix from the input of the function argument
    x <<- y       
    #set the inverse matrix as NULL, before any calculation
    invX <<- NULL 
  }
  get <- function() x                       # return the data matrix
  setinverse <- function(inv) invX <<- inv  # set inverse matrix as given, in an environment variable invX
  getinverse <- function() invX             # return the iverse matrix as was saved before
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated
## (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  invX <- x$getinverse()    # get the value of the inverse matrix from the function makeCacheMatrix 
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)            # as inverse matrix is already calculated before, return the cached value
  }
  data <- x$get()           # get the original data matrix
  invX <- solve(data, ...)  # calculate inverse matrix, as it was not calculated before
  x$setinverse(invX)        # set the inverse matrix, that was just calculated, to use afterwards
  invX                      # return inverse matrix
}

##   Test1
Z <- makeCacheMatrix(matrix(c(1:4),2,2))
Z$get()
Z$getinverse()
Z2 <- cacheSolve(Z)
Z2
