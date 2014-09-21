#' Pair of functions that together provide for the creation of a list 
#' that will cache the inverse of a matrix once it has been calculated

#' Creates list containing passed in matrix that will be used by cacheSolve.
#' 
#' Will stop process if passed matrix is not square
#'
#' @param mtrx A matrix
#' @return a list containing getters and setters for contained matrix and
#' getters and setters for inverse of contained matrix
#' @example mcm <- makeCacheMatrix(matrix(rnorm(25), nrow=5, ncol=5))

makeCacheMatrix <- function(mtrx = matrix()) {
    if(nrow(mtrx) != ncol(mtrx)){
      stop("matrix must be square")
    }
    
    inverse <- NULL
    
    set <- function(newMatrix){
        if(nrows(mtrx) != ncols(mtrx)){
          stop("matrix must be square")
        }
        mtrx <<- newMatrix
        inverse <<- NULL
    }
    
    get <- function()mtrx
    
    setInverse <- function(inverted) inverse <<- inverted
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#' Takes list created by makeCacheMatrix and returns cached inverse of matrix
#' contained in the list if it exists.  Otherwise, it will compute the inverse
#' of the matrix,store it in the cache and then return it..
#' 
#' Will stop process if matrix contained in list is singular.
#' 
#' @param matrixList list created by makeCacheMatrix function
#' @return matrix that is inverse of matrix contained in matrixList
#' @example cacheSolve(mcm)

cacheSolve <- function(matrixList) {
    inverse <- matrixList$getInverse()
    
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    matrix <- matrixList$get()
    
    if(!require("matrixcalc", character.only = TRUE)){
      install.packages("matrixcalc")
      if(!require("matrixcalc",character.only = TRUE)) error("matrixcalc package not found")
    }
    suppressPackageStartupMessages(library(matrixcalc))
    
    if(is.singular.matrix(matrix)){
      stop("matrix is singular and cannot have inverse taken")  
    }
    
    inverse <- solve(matrix)
    matrixList$setInverse(inverse)
    inverse  
}
