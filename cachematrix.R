## Create cache for matrix and its inverse matrix
makeCacheMatrix <- function(matrixData = matrix()) {
  ## initialization of new instance
  inverseCache <- NULL

  #instance methods
  #set new matrix and invalidate cache
  set <- function(newMatrix) {
    matrixData <<- newMatrix
    ## Invalidate cache
    inverseCache <<- NULL
  }

  get <- function() matrixData

  # set cache value
  setInverse <- function(inverseValue) inverseCache <<- inverseValue

  getInverse <- function() inverseCache

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate inverse matrix based on matrix stored in matrixCache object.
## If matrixCache contains previously calculated and cached inverse matrix return it.
## Otherwise calculate it and store in matrixCache object.
cacheSolve <- function(matrixCache, ...) {
  inverseMatrix <- matrixCache$getInverse()

  ## Check if cached data exists.
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- matrixCache$get()
  inverseMatrix <- solve(data, ...)

  ## Store calculated inverse matrix into cache object.
  matrixCache$setInverse(inverseMatrix)
  inverseMatrix
}
