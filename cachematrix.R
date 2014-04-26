## makeCacheMatrix and cacheSolve functions computes inverse of a Matrix
##      and stores the inverse matrix in cache so that inverse need not be
##      calculated repeatedly and thereby, saving computation time 

## makeCacheMatrix function returns a list object comprising of functions (set, 
##      get, setinverse, getinverse). setinverse and getinvesre functions 
##      calculates inverse of the matrix and returns inversed matrix 
##      respectively

makeCacheMatrix <- function(x = matrix()) {

    ## Variable 'i' to cache the inverse value of the "matrix"
    i <- NULL
    
    ## Cache the "matrix" x in the "parent environment"
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Upon call to function get(), return "matrix" x
    get <- function() x
    
    ## Assign inversed "matrix" passed as an argument to 'i'
    setinverse <- function(inverse) i <<- inverse
    
    ## Retrieve inverse value of the "matrix", set by the setinverse function
    getinverse <- function() i
    
    ## Return list comprising of functions (set, get, setinverse, getinverse)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix
##      and returns inverse of the matrix 

cacheSolve <- function(x) {
    
    ## calls up "getinverse()" function that is part of list returned to 
    ##  makeCacheMatrix. retrieves inverse of "matrix" x and assigns to 'i'
    
    i <- x$getinverse()
    
    ## checks whether inverse matrix already exists in cache. If inverse matrix 
    ##      exist then return it to cacheSolve 
    if(!is.null(i)) {
        message("getting inverse matrix from cache")
        return(i)
    }
    
    ## Gets matrix value of 'x' and assigns to data
    data <- x$get()
    
    ## calculates inverse of matrix and assigns to 'i'
    i <- solve(data)
    
    ## assigns inverse matrix in the "parent environment"
    x$setinverse(i)
    
    ## return inverse matrix
    i
}