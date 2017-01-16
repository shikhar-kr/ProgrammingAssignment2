## Programming assignment 2 : Submitted by Shikhar
## First function makes a CachableMatrix, second one helps it cache
## Usage / Example :
## w1 = makeCacheMatrix(matrix(c(4,2,7,6),2,2)
## cacheSolve(w1)
## w1$get() %*% w1$getinverse()  ## outputs identity matrix

## Input : Matrix, Output : list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Input : makeCacheMatrix list, computes inverse of matrix and caches it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

