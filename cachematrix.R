## The functions below calculates the Inverse of a Matrix  and also stores the inverse in cache.
##Next time this same matrix is passed in to the function the calculation is not done afresh but is pulled from the cache.
## This is achieved using two functions....
##1.makeCacheMatrix---which takes in the Matrix(whose Inverse is to be calculated) as 
##input and associates it to a customised vector(which has four sub functions...set(),get(),setInverse() and getInverse().
##2.cacheSolve---which takes in this above vectorised form of function as an input object and calculates the Inverse of the 
##Input Matrix.If the inverse is already calculated and stored in cache,this this function also fetches it from cache 
##and serves the Inverse without a recalculation

## makeCacheMatrix takes in the Matrix(whose Inverse is to be calculated) as 
##input and associates it to a customised vector(which has four sub functions...set(),get(),setInverse() and getInverse().

makeCacheMatrix <- function(x = matrix()) {
inverMat <- NULL

  set <- function(y)    ##define function set
    {
    x <<- y
    inverMat <<- NULL
    }
  get <- function() x    ##define function get
  
  
  setInverse <- function(calInverse) inverMat <<- calInverse   ##define function set the Inverse of Matrix
  
  
  getInverse <- function() inverMat  ##define function get the Inverse of Matrix
  
  ###list(elementName is set =svalue is  function set,elementName is get=value is  function get,
  ###elementName is setInverse=value is  function setInverse,elementName is getInverse=value is  function getInverse)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes in this above vectorised form of function as an input object and calculates the Inverse of the 
##Input Matrix.If the inverse is already calculated and stored in cache,this this function also fetches it from cache 
##and serves the Inverse without a recalculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inverMat <- x$getInverse()
  print(inverMat)
  if(!is.null(inverMat))
    {
    message("getting cached data")
    return(inverMat)
    }
  data <- x$get()
  inverMat <- solve(data)
  x$setInverse(inverMat)
  inverMat         
        
}
