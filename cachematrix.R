## the makeCacheMatrix function takes a matrix as an input, then caches its inverse in another environment
## the cacheSolve function accesses the makeCacheMatrix object and returns the inverse saved there

##The makeCacheMatrix makes a cache of the given matrix and its inverse, where there inverse is set to NULL by default
#the cached inverse in the returned object is created by the next function

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#this takes a makeCacheMatrix object and populates its im attribute to be the inverse of the original given matrix if no inverse exists
#the solve() function is used to find the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached matrix")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}

