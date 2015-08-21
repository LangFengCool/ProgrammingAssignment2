## These two functions below are designed for caching matrix inverse computation results which are potentially 
## time-consuming. The makeCacheMatrix() function returns a list of functions which can be used in cacheSolve()
## function so that cacheSolve can easily find the inverse of the matrix in the arguement if calculated before
## And if not calculated before, cacheSolve() would solve for inverse and set it using setInverse() function defined
## defined in makeCacheMatrix() function.

## The makeCacheMatrix() function defines four functions within its definition and returns a list of functions.
## The set() function defined is used to set the value of the matrix
## The get() function defined is used to get the value of the matrix
## The setInverse() function is used to set the value of the inverse
## The getInverse() function is used to get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## set the value of the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## get the value of the matrix
      get <- function() x
      ## set the value of the inverse
      setInverse <- function(Inverse) m <<- Inverse
      ## get the value of the inverse
      getInverse <- function() m
      
      ## return a list of functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse) 
}


## The cacheSolve() function is used to calculate the inverse of the matrix.
## The function first checks whether the inverse has been calculated already, if so, it gets the inverse from the cache
## and skips the computation. Otherwise, it would calculate the inverse through solve() and sets the inverse via 
## setInverse() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## check if the inverse already exists, if so, gets the inverse from the cache
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        ## Otherwise, calculate the inverse and set it 
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        ## return the inverse 
        m
}
