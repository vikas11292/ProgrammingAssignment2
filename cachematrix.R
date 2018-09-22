## Programming Assignment 2: Lexical Scoping
# Author : Vikas Singh - 22-September-2018

# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss here).

# There are two functions part of this program. 
# First function (makeCacheMatrix) will 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of matrix
# 4. get the value of the inverse of matrix
# There are two checks to validate that input is a square and 
# non-singular matrix.
makeCacheMatrix <- function(x = matrix()) 
{
    if(dim(x)[1]!=dim(x)[2])
    {
        message("The matrix is not invertible - Please provide a square matrix!!!")
        return()
    }
    
    if(det(x)==0)
    {
        message("The matrix is not invertible - This is a singular matrix!!!")
        return()
    }
    
    invmatrix <- NULL
    setmatrix <- function(y) 
    {
        x <<- y
        invmatrix <<- NULL
    }
    
    getmatrix <- function() x
    
    set_invmatrix <- function(cached_invmatrix) invmatrix <<- cached_invmatrix
    get_invmatrix <- function() invmatrix
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, set_invmatrix = set_invmatrix, get_invmatrix = get_invmatrix)
}


## ---------------------Start of Documentation---------------------
# This is the second function.
# The following function calculates the inverse of the special "vector" created 
# with the above function. However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
# in the cache via the set_invmatrix function.
# ---------------------End of Documentation---------------------
cacheSolve <- function(x,...) 
{
    invmatrix <- x$get_invmatrix()
    if(!is.null(invmatrix)) 
    {
        message("getting cached data")
        return(invmatrix)
    }
    
    inputdata <- x$getmatrix()
    
    error_message <- NULL
    error_message <- errhndlr_invmatrix(solve(inputdata))
    
    if(!is.null(error_message))
        {
             print(paste("Computational Error : ",error_message)) 
        }
        
    else
        {
            invmatrix <- solve(inputdata)
            x$set_invmatrix(invmatrix)
            return(invmatrix)
        }
}

## ---------------------Start of Documentation---------------------
# This is an additional function to handle error in function #2 which was
# was not caught in function #1 matrix validation.
# This will ensure majorities of computational error.
# ---------------------End of Documentation---------------------

errhndlr_invmatrix <- function(expr) 
    {
        computeErr <- NULL
        errhndlr <- function(e) 
            {
                computeErr <<- e$message
                NULL
            }
        tryCatch(withCallingHandlers(expr), error = errhndlr)
        computeErr
}
