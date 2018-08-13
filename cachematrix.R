## This function creates a  "matrix" object that can cache its inverse

## set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y                                         ##setting the value of x
    m <<- NULL
  }

  get <- function() x                               ##getting the value of x from the parent environment
  setinverse <- function(inverse) m <<- inverse     ##setting the value of the matrix
  getinverse <- function() m                        ##getting m from the parent environment
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated and the matrix has not changed, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                  ##getting the value of m
  if (!is.null(m))                     ##if m has values get chached data
    {
    message("getting cached data")
    return(m)                          ##returning the value of m
    }

data <- x$get()
m <- solve(data, ...)                 ##calculating the inverse of the matrix
x$setinverse(m)                       ##setting the inverse in m
m                                     ##returning the value of m
}
