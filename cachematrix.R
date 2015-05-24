##  function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) 
{ #store the data in x
    x <<- y
    i <<- NULL
  }
  get <- function() x #function to retrieve the data from x
  setinv <- function(inv) i <<- inv #function to store the inverse
  getinv <- function() i #retrieve the stored inverse 
  list(set = set, get = get, setinv = setinv,getinv = getinv)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() #store the inverse of matrix in m
  if(!is.null(i)) #if m is not NULL i.e inverse is in the cache
  {
      message("getting cached data :")
      return(i)
  }
  data <- x$get() #get data from the list 
  i <- solve(data,...) #calculate the inverse
  x$setinv(i) #cache the inverse in the list
  i #return inverse
  
}
