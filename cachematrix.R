## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        # set inv to NULL
        inv <- NULL
        
        # set matrix data and set inverse to NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get matrix data
        get = function() x
        
        # set inverse value
        setinv = function(inverse) inv <<- inverse
        
        # get inverse
        getinv = function() inv
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), retrieve inverse from the cache. Otherwise,
## calculate inverse and cache
cacheSolve <- function(x, ...) {
        
        ## get inverse from cache
        inv <- x$getinv()
        
        # Does inv have a value?
        if (!is.null(inv)){
                
                # return value from cache 
                message("inverse from cached data")
                return(inv)
                
        }
        
        # no value from cache, get input matrix
        input_matrix <- x$get()
        
        # calculate inverse of input matrix
        inv <- solve(input_matrix,...)
        
        # cache inverse value
        x$setinv(inv)
        
        # return inverse of input matrix
        return(inv)
        
}
