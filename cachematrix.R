##makeCacheMatrix and cacheSolve work in tandem. makeCacheMatrix accepts a matrix x and returns 
##a list of 4 functions. cacheSolve, using makeCacheMatrix, checks whether the current input 
##matrix's inverse has been cached. if true, return the cached inverse matrix. else, calculate 
##the inverse matrix and cache it

## makeCacheMatrix takes an invertible matrix x and creates a list of functions to:
##set: sets the value of x and m into the larger environment
##get: gets the matrix x
##getmatrix: gets the inverse of the matrix x 
##setmatrix: sets the inverse of the matrix x


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, 
    get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve accepts the list of functions from makeCacheMatrix and checks if the m variable, 
## assigned null in the first iteration or if matrix is new, is null. if not null, it displays
##a message and returns cached m. if null, it calculates the inverse of the matrix x that was fed into
##makeCacheMatrix and assigns m as that matrix

cacheSolve <- function(x, ...) {
   m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##solve for inverse of a matrix
  m <- solve(data)
  x$setmatrix(m)
  m
}
