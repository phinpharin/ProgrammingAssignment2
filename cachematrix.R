## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 ## variable for matrix invert
  m_invert <- NULL  
  
  ##set the matrix for calcul
  set <- function(y) {
    x <<- y
    m_invert <<- NULL  ## do nothing on variable for matrix invert
  }
  ##get the matrix for calcul
  get <- function() x
  
  ##functions interfaces
  
  ##interface for doing the calcul
  setinvert <- function(solve) m_invert <<- solve
  
  ##interface for geting the calcul
  getinvert <- function() m_invert
  
  ## just administrative code in objet R
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 
  ## Return a matrix that is the inverse of 'x'
  m_invert <- x$getinvert()
  
  ##if x is old , just return the valeur with "message"
  if(!is.null(m_invert)) {
    message("just getting cached matrix")
    return(m_invert)
  }
  ##if x is new , do calculate
  
  data <- x$get()                   ##getting matrix for calculate
  m_invert <- solve(data, ...)      ##do  calculate matrix invert
  x$setinvert(m_invert)             ## and  set  the matrix invert to the interface
  ## and return
  m_invert

}
