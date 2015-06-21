## Below are two functions (makeCacheMatrix and cacheSolve) to solve inverse matrices problems. 

## makeCacheMatrix creates the nessassary vectors and functions in order to inverse the matrix 

##CacheSolve uses the functions created by makeCacheMatrix to inverse the matrix and complete 
#problem


makeCacheMatrix<- function( x = matrix()){
        ## set i as inverse vector
        i <- NULL
        
        ## Create the vector for the matrix
        set <- function(y){ 
                x<<- y
                i<<- NULL
        }
        ## functions which get and set the matrix and inverse vectors
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse<- function() i
        
        ## Creates the matrix 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
        ## Assign vector to i
        i <- x$getinverse()
        
        ## check to see if i has a computed value
        if (!is.null(i)){
                message("getting cached data")
                return (i)
        }
        
        ## gathers the data to be inversed
        data<- x$get()
        i <- solve(data)
        
        ## inverse is cached
        x$setinverse(i)
        
        ## return the inversed matrix 
        i
}
