#Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialisation of two objects : x and inverse
  # inverse is set to NULL - initialising it as an object within makeCacheMatrix to be used by later code in the function 
  inverse <- NULL
  # Define behaviour or functions for objects of type makeCacheMatrix (set, get, getInverse, setInverse)
  set <- function(y) {
    x <<- y #assign input argument to the x object in the parent environment 
    inverse <<- NULL #assign NULL to the inverse object in the parent environment. This line of code clears any value of inverse that had been cached by a prior execution of cacheSolve() function.
    # <<- assings the value on the right side of the operator to an object in the parent environment named by the object on the left side of the operator
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#computes the inverse of the special matrix returned by makecacheMatrix. if the inverse has already been calculated (and the matrx has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  # attempts to retrieve a solve (inverse the matrix) from the object passed in as the argument. First, it calls the getInverse() function on the input object 
  inverse <- x$getInverse()
  # It then checks to see whether the result is NULL. makeCacheMatrix() sets the cached solve calculation to NULL whenever a new vector is set into the object, if the value here is not equal to NULL, it means it has a valid, cached inverse matrix value and can return it to the parent environment. 
  # if !is.null(inverse) is FALSE, cacheSolve() gets the vector from the input object, calculates solve(), uses setInverse() function on the input object to set the solve in the input object, and then return the value of the inverse matrix to the parent environment by printing the inverse object
  if(!is.null(inverse)) {
    message("getting cached inverse matrix data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}


