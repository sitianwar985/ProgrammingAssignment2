#### FUNCTION 1: make cache matrix - create "matrix" object that can cache its inverse
##assume matrix is invertible
# LINE 1: assign Null to inv
# LINE 2: set value of matrix 
          # <- : works on current level
          # <<-: modify variables on parent level
# LINE 3: get value of matrix 
# LINE 4: set value of inverse - use <<-
# LINE 5: get value of inverse
# LINE 6: create a list 

#### FUNCTION 2: cachesolve - computes inverse of "matrix" - 
#if inverse has been calculated & matrix hasnt been changed- retrieve inverse from cache
# LINE 1: returns a matrix of inverse of X,and assign to inv
# LINE 2: check if inverse if already been calculated, 
#can get inverse from cache, can skip computation, return inverse
# otherwise compute inverse of matrix, by using Solve
# LINE 3: solve using SOLVE
# LINE 4: Set value of the inverse in the cache using setInverse function


makeCacheMatrix <- function(x = matrix ()) {
        invrs <- NULL
        set <- function(y){
                x <<- y
                invrs <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {invrs <<- inverse}
        getInverse <- function() {invrs}
        list(set =set, get =get, setInverse = setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
        invrs <- x$getInverse()
        if(!is.null(invrs)){
                message("use cached data")
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat,...)
        x$setInverse(invrs)
        invrs
}