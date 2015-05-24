#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#The assignment includes the following  functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: 
#1.This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#2.If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#3.Computing the inverse of a square matrix can be done with the solve function in R. 
#4.For example, if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
	if(!is.matrix(x))
	{
		stop("Not a matrix!!");
        }
        cacheMatrixInverse <- NULL
        set <- function(y) #y is the newValue to be defined
        {
	  if(!is.matrix(y))
       	  {
		stop("Not a matrix!!");
          }
	  x <<- y
	  cacheMatrixInverse <<- NULL; #setting inverse to default value as matrix is initalized
        }
        get <- function() 
        {
                return (x);
        }

        setInverse <- function(y) 
	{
		cacheMatrixInverse <<- y
	}
        getInverse <- function() 
	{
                return(cacheMatrixInverse)
	}
        list(set = set, get = get, 
             setInverse  = setInverse, 
             getInverse =  getInverse);
}       
        

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
        cacheMatrix <- x$get()
	#Call an R object using a character string.
	if(!is.matrix(cacheMatrix))
	{
		stop("Not a matrix ")
		return(cacheMatrix)
        }
	inverseValue <- x$getInverse();
        #Case 1 : Inverse already exists, therefore return value
	if(!is.null(inverseValue))
	{
		print("calculated Inverse Matrix");
		return(inverseValue);
	}
	#Case 2 : Not a squared Matrix
	squared_matrix <- (ncol(cacheMatrix) == nrow(cacheMatrix));
        if(!squared_matrix)
	{
		 print("Not a squared matrix!!");
	         x$setInverse(NULL);
		 return(NULL);
	}	
	options(warn = 2) #Convert warnings to errors
	#https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html
        tryCatch(
        #case 3
        {
           print("invertible matrix has been solved");
           x$setInverse(solve(data,...));
        },
	#case 4
        error = function(error_cond)
	{
		error_msg <- paste("cacheSolve error: <<",error_cond,">>");
		error_msg <- paste(error_msg, "set the inverse to NULL");
		print(error_msg);
		message(error_msg);
		x$setInverse(NULL);
	},
	finally =
        {
		options(warn = 0) # If warn is zero (the default) warnings are stored until the top–level function returns.
		return(x$getInverse());
	}
 }

}
