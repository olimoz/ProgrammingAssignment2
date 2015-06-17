
## This function creates a special "matrix" object that can cache its inverse.
## It has four methods:
##    set - to set the contents of the matrix
##    get - to get the contents of the matrix, if it has been set
##    setinv - to get the inverse of the matrix
##    getinv - to get the inverse, if it has been set

## To use the function, first create some data and then create an instance of 'CacheMatrix' like this:
##    MatrixData <- rbind(c(1, 2), c(3, 4))
##    myexample <- makeCacheMatrix(MatrixData)

## Alternatively, instantiate a blank 'CacheMatrix' object like this:
##    myexample <- makeCacheMatrix()
## And then set the contents using the 'set' method.
##    myexample$set(rbind(c(1, -1/4), c(-1/4, 1)))

## With an instance of 'CacheMatrix' in existence, you can call methods like this:
##    -This sets the contents of the matrix, overwriting any previous contents and nullifying the inverse
##      myexample$set(MatrixData)
##
##    -This returns the contents of the matrix
##      myexample$get()
##
##    -This sets the inverse of the matrix (But does not test whether the inverse is correct)
##      myexample$setinv(InverseOfMatrixData)
##
##    -This returns the inverse of the matrix which has been set (But does not test whether the inverse is correct) 
##      myexample$getinv()

makeCacheMatrix <- function(x = matrix()) 
{
  ## On instantiation, set the inverse to null
  inv <- NULL
  
  ## set method, takes a matrix as its args
  set <- function(y) 
  {
    ## Set the matrix to whatever matrix data was provided
    ## Note, if non-matrix data is provided, this function will error
    x <<- y
    ## Everytime the matrix is set, the inverse is reset to null.
    ## This will force recalculation of the inverse in the 'cacheSolve' function.
    inv <<- NULL 
  }
  
  ## get method, simply returns contents of matrix
  get <- function() 
  {
    x
  }
  
  ## setinv method, simply sets the property 'inv' to whatever is provided in the args (ie newinv). 
  ## Does NOT test for correctness of inverse matrix
  setinv <- function(newinv)
  {
    inv <<- newinv
  }
  
  ## get method, simply displays the contents of the property 'inv'
  getinv <- function() 
  {
    inv
  }

  ## publish the methods
  list(
       set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv
       )
}

## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## IF 
##    the inverse has already been calculated AND the matrix has not changed 
## THEN 
##    the cachesolve retrieves the inverse from the cache
## ELSE 
##    it creates a new inverse using R's solve() function
## Note, if the instance of the matrix contains NA data, then the solve() function throws an error.

## The function is called like this:
##    cacheSolve(myexample) 
## where myexample is a 'CacheMatrix' object instantiated using createCacheMatrix()

cacheSolve <- function(x, ...) 
{
  ## Get the current contents of 'inv', i.e. the inverse, for x
  inv <- x$getinv()
  
  ## IF a value for the inverse 'inv' already exists
  if(!is.null(inv)) 
  ## Remember that everytime the matrix is 'set' the inverse 'inv' is reset to null.
  ## So by testing for 'inv is not null' we have confirmed there has not been a change in the matrix
  ## However, we have NOT checked for the correctness of the inverse, as that is computationally expensive
  {
         message("getting cached data")
         return(inv)
  }
  
  ## If the inverse is null then either the matrix changed or its a new creation
  ## Eitherway, we need to create the inverse. So first, we 'get' the data from the matrix
  data <- x$get()
  
  ## Now we use the 'R' solve() function to calculate the inverse
  ## Note the use of the ... for the optional parameters of solve().
  inv <- solve(data,...)
  
  ## And we set the inverse 'inv' to be this calculated inverse matrix called 'inv'
  x$setinv(inv)
  
  ## Finally, we'll display the results to screen
  inv
}
