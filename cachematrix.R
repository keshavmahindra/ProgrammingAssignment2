# These two functions are used to create a special matrix to be cached
# and checks if the inverse is already held in memory or not

# The purpose of this is to reduce the amount of computation required 
# to find the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      # This function creates a special matrix to be cached 
      #
      # Args:
      #  x: The matrix you want to find the inverse of
      # Returns:
      #  The list of functions that can be used to manipulate the x matrix
      
      
      inv <- NULL # inverse set to empty
      
      set <- function(y) { 
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x # returns original matrix
      
      setinv <- function(solve) inv <<- solve    
      getinv <- function() inv
      
      # list of functions
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


cacheSolve <- function(x, ...) {
      # This function calculates the matrix which is the inverse of x
      #
      # Args:
      #  x: The matrix you want to find the inverse of.
      # Returns:
      #  The inverse of the x matrix
      
      inv <- x$getinv() 
      
      # If inv is not null, it holds the inverse already
      # This means the inverse does not have to be calculated
      if(!is.null(inv)) {
            message("retrieving cached inverse") # Notifies cached inverse exists
            return(inv)
      }
      
      data <- x$get()
      inv <- solve(data, ...) # calculates inverse
      x$setinv(inv)
      inv
      
}
