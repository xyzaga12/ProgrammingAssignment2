## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv_m <- function(inv_m) inverse_matrix <<- inv_m
  get_inv_m <- function() inverse_matrix
  list(set = set, get = get,
       set_inv_m = set_inv_m,
       get_inv_m = get_inv_m)      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$get_inv_m()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data)
  x$set_inv_m(inverse_matrix)
  inverse_matrix
}
