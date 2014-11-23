## Put comments here that give an overall description of what your
## functions do
## SB 11-22-14 9:15pm

## makeCacheMatrix function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {                 
        m <- NULL                                           #set m to null
        set <- function(matrix) {                           #set the value of the matrix
                x <<- matrix
                m <<- NULL
        }
        get <- function() x                                 #get the value of the matrix
        setinverse <- function(inverse)  {m <<- inverse}    #set the value of the inverse
        getinverse <- function() {m}                        #get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)       
}


## cacheSolve calculates the mean fo the special matrix from teh makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {                                   #check if matrix has been made
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)                                    #invert the matrix
        x$setinverse(m)
        m    
}

#Test the functions to make sure they work
mat <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)        #create a matrix
mat                                                         #Check the original matrix
cacheSolve(mat2)                                            #The inverse matrix output
