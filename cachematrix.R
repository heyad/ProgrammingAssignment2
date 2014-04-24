## makeCacheMatrix creates a special object matrix 
## to cache its inverse matrix 

makeCacheMatrix <- function (x=matrix()){
        
        ##inv_m will hold the matrix inverse
        ## <<- was used to create the variable
        ## if not already created in the environmet
        inv_m <<- NULL
        
        ##set the special matrix 
        set <- function (y) {
                x <<- y
                inv_m <- NULL
                
        }
        
        ##get() the 'special' matrix 
        get <- function ()  (x)
        
        ##set and get inverse matrix 
        setInverse <- function (inv) inv_m <<- inv 
        getInverse <- function () inv_m 
        list (set=set, get=get, 
              setInverse = setInverse, 
              getInverse = getInverse)
        
}

## The cacheSolve will make use of the makeCacheMatrix()
## It is designed to only compute the inverse of a matrix 
## if not already computed ....

cacheSolve <- function (x, ...){
        
        ##get the inverse matrix 
        inv_m <- x$getInverse()
        
        ##if the inv_m is not null then just return it
        ##and don't do any computation 
        if(!is.null(inv_m)) {
                
                message("getting cached inverse of the matrix")
                return(inv_m)
                
        }  
        
        ##else get the matrix 
        ##compute its inverse using the solve() function 
        data <- x$get()
        inv_m <- solve(data)  
        x$setInverse(inv_m)  
        inv_m
        
}
