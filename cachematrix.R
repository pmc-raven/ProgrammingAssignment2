## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mtrx = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    mtrx <<- y
    inv <<- NULL
  }
  
  get <- function() {mtrx}
  
  setinverse <- function(inverse) {inv <<- inverse}
  
  getinverse <- function() {inv}
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(mtrx) {
  inv <- mtrx$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  
  inv <- solve(mtrx$get()) #solve: returns inverse of input matrix (assume input matrix is invertible)
  
  mtrx$setinverse(inv)
  
  inv
}

# TRIAL ----
mtrx_1 <- matrix(1:4, 2, 2)

MTRX_1 <- makeCacheMatrix(mtrx_1)

MTRX_1$get()

MTRX_1$getinverse()

cacheSolve(MTRX_1)

MTRX_1$getinverse()

cacheSolve(MTRX_1)



