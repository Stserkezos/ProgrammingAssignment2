## To be honest it's just the same function as the assigment, slightly changed to be for a matrix and insted
## of the mean to provide the inverse of a matrix

## You input a matrix and define an m that is Null (so there is nothing there) and a set function of y that pretty much defines y
##as the matrix in question and m it's NULL value
##it just makes a list to be used as input for the cacheSolve
#####

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverto) m <<- inverto
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## it takes the input from the above function, looks if m is null, if it is it displays a message, takes the data, 
##solves it (gets the inverse through solve function) and it displays it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
