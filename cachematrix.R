## Put comments here that give an overall description of what your
## functions do: 
## this function will create a matrix,set and get the value of the matrix
## and then set the inverse and get the inverse of it. 


##makeCacheMatrix will create a matrix object which will cache its inverse 
makeCacheMatrix <- function(x = matrix()) {

        inver <- NULL     #this will later hold value of the inverse
        set <- function(y) {
            x <<- y
            inver <<- NULL
    } 
    get <- function() x  #get the value of original matrix
    setinverse <- function(inverse) inver <<- inverse  #set the inverse value 
    getinverse <- function() inver   #get the inversed value
    list (set = set, get= get,
          setinverse = setinverse,
          getinverse = getinverse)
}

##cacheSolve  will computes the inverse of what's created by makeCacheMatrix function
## the if statement will seperate the situation: when the inverse was calculated 
#it will return the already calculated one, if not it will caculate it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if(!is.null(inver)){
            message("getting cached data")
            return (inver)
    }
    matrx <-  x$get()
    inver <- solve(matrx,...)
    x$setinverse(inver)
    inver
}
