# This function stores the matrix and creates somewhere to hold the inverted matrix 
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve # invert the matrix passed at the top level
        getinverse <- function() m #get the saved version
        
	# the return value of makeMatrix is a list
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}

# the cacheinvert function tests for the presence of a cached inverted matrix
# if there is a value in the cache, return the cached value
# otherwise compute the inverted matrix, save it to the cache and also return it
cacheSolve <- function(x, ...) {
        m <- x$getinverse() #query the cache from x created with makeVector
        if(!is.null(m)) {
		message(">>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<") 
                message(">>> getting cached inverted matrix <<<")
		message(">>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<") 
                return(m) # return cached value, no computation required
        }
        data <- x$get() #if there's no cache this gets run
        m <- solve(data, ...) #computation of inverted matrix
        x$setinverse(m) #save result to cache
        m  # return m
}


