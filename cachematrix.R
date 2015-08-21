## These two functions below are designed for caching matrix inverse computation results which are potentially 
## time-consuming. The makeCacheMatrix() function returns a list of functions which can be used in cacheSolve()
## function so that cacheSolve can easily find the inverse of the matrix in the arguement if calculated before
## And if not calculated before, cacheSolve() would solve for inverse and set it using setInverse() function defined
## defined in makeCacheMatrix() function.

## The makeCacheMatrix() function defines four functions within its definition and returns a list of functions.
## The set() function defined is used for ###########
## The get() function defined is used for get the data of argument
## The setInverse() function is used to set the arguement
## The getInverse() function is used to get the Inverse of the matrix if the inverse already exists

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(Inverse) m <<- Inverse
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
