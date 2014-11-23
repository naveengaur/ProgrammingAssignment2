## makeCacheMatrix creates a cache Matrix 
##
## usage example:
## c=rbind(c(3, 3.2), c(3.5, 3.6))   
## C <-makeCacheMatrix(c)
## cacheSolve(M)
## cacheSolve(M) -- Second time and onwards prints "getting cached data"


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	   
	    #set the vector to be cached
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x

	    #set Inverse of Matrix
            setInv <- function(inv) m <<- inv
	    
	    #get Inverse of the Matrix, If its set it setvalue would be returned. Else NULL will be returned.
            getInv <- function() m
            
	    list(set = set, get = get,
                 setInv = setInv,
                 getInv = getInv)
}



## computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
## If the inverse has already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	    
	    #get Inverse of the the Matrix. If already computed and set, the cached value will be returned, else NULL will be returned.
            m <- x$getInv()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
	    # incase NULL value is return, compute the inverse using solve funtion.
            data <- x$get()
            m <- solve(data, ...)
            x$setInv(m)
            m
}
