## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 ## variable for matrix invert
  m_invert <- NULL   
  ##set the matrix
  set <- function(y) {
    x <<- y
    m_invert <<- NULL
  }
  ##get the matrix
  get <- function() x
  ##functions interfaces
  setinvert <- function(solve) m_invert <<- solve
  getinvert <- function() m_invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_invert <- x$getinvert()
  ##if x is old , just return the valeur
  if(!is.null(m_invert)) {
    message("getting cached matrix")
    return(m_invert)
  }
  ##if x is new , do calculate
  data <- x$get()
  m_invert <- solve(data, ...)
  x$setinvert(m_invert)
  ## and return
  m_invert

}
