

## Function containing definitions for cache functions to store inverse and the relevant working data

makeCacheMatrix <- function(x = matrix()) {
  inverse_var <- NULL ##initialize variable storing inverse of matrix
  
  set <- function(y){   
      x<<-y
      inverse_var <<-NULL
  }
  
  get <- function() x
  
  
  setInverse<- function(inverse) inverse_var<<- inverse ##cache store function
  
  getInverse <- function() inverse_var   ##Cache access function
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  

}


## Function to solve for inverse if needed and invoke cache via makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse_var<-x$getInverse()
      if(!is.null(inverse_var)){        ##conditional check if cached value exists or not
        message("Getting cached data")
        return(inverse_var)
      }
    mat_data <- x$get()
    inverse_var <- solve(mat_data)
    x$setInverse(inverse_var)
    inverse_var
  
}
