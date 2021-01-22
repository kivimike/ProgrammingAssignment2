## These functions optimize the calculation of the inverse of matricies
## 

## The makeCacheMatrix function returns the list of getters and setters
#set() allows to save the matrix and sets the inverse <- NULL
#get() gets the saved matrix
#setinv() saves the inverse
#getinv() returns the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get<-function() x
    
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks if the inverse was already calculated
#it returns the inverse if it was
#else it calculates the inverse and calls the setinv() function to save the result

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<- solve(data)
    x$setinv(inv)
    inv
}

