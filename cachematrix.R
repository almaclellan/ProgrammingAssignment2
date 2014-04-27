## The functions will take in a matrix, and if it has already created
## the inverese for that matrix, it will return the cached value.
## If there is no cached value or the matrix has changed, it will return
## an empty matrix so a new value can be calculated.

## Creates a list with the functions for handling the matrix
## set & setsolve will take the value passed in and save it in the cache
## get & getsolve will return the value in the cache

makeCacheMatrix <- function(x = matrix()) {

  # Create an empty matrix for when the matrixes are not equal
  ne = matrix(0,nrow=0,ncol=0)
  
  # Create an empty matrix for saving the solve value
  m = matrix(0,nrow=0,ncol=0)
  
  #Set the matrix to the pass being passed in 
  set <- function(y) {
    m <<-y
  }
  setsolve <- function(y) 
  {
    m <<- y 
  }
  
  # Get the values from the cache if the matrix has not changed.
  # Check for change from the value passed in when the function 
  # makeCacheMatrix is instantiated. If the matrix matches, return 
  # the inverted matrix from the cache. If the matrix has changed, 
  # reset the value x to the new matrix and return an empty matrix
  # so it will invert the new matrix.
  
  get <- function(y) {
    if (is.matrix(y) && is.matrix(x) && dim(y) == dim(x) && all(y == x))
    {
      m # The cached inverse of the matrix
    }
    else
    {
      x <<- y # The cached value of the input matrix
      ne # The empty matrix to force a rerun of solve()
    }
  }
  getsolve <- function(y) {

    if (is.matrix(y) && is.matrix(x) && dim(y) == dim(x) && all(y == x))
    {
      m  # The cached inverse of the matrix
    }
    else
    {
      x <<- y # The cached value of the input matrix
      ne # The empty matrix to force a rerun of solve()
    }
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This takes a matrix and the list created from makeCacheMatrix to
## invert the matrix if it has not been inverted, or read it from the
## cache if it has been.

cacheSolve <- function(x, y) {
 
  m <- y$getsolve(x)  # returns the cache value if the matrix has not changed
                      # or an empty matrix if it has changed
  
  if (!nrow(m) == 0)  # not an empty matrix, so it was in the cache
  {
    message("getting cached data")
    return(m)
  }
  else                # either the cache was empty or the matrix has changed
  {
    message("calculating inverse")
  }
  
  m <- solve(x) # inverse the matrix
  y$setsolve(m) # store the value in the cache
  m             # return the value for printing
}

## Testing the functions
## Create the matrix to invert.

m1 <- matrix(c(1,2, 11,12), nrow = 2, ncol = 2, byrow = TRUE,
             dimnames = list(c("row1", "row2"),
                             c("C.1", "C.2")))

## instantiate the list of functions and save the matrix value
y<-makeCacheMatrix(m1)

for (z in c(1:4))
{
  a<-cacheSolve(m1,y)
  print(a)
  if (z == 2) # force the matrix to change to test reset
  {
    m1[1,1] <- 5
  }
}
