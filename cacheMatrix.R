# Name      : makeCacheMatrix
# Arguments : Matrix
# Return    : List of the following functions 
#               get -> Used to get the current matrx variable
#               setInverseMatrix->Used to set the inverse matrix variable
#               getInverseMatrix->used to get the inverse matrix variable
makeCacheMatrix <- function(matrxVar = matrix()) {
    set <- function(y) {
        matrx <<- y
        invMatrx <<- NULL
    }
    # The get functions is used to retrieve the current value of matrx.
    get <- function() matrx
    # The setInverseMatrix function sets the invMatrx variable with the 
    # calculated inverse matrix value
    setInverseMatrix <- function(inverseMatrix) invMatrx <<- inverseMatrix
    # getInverseMatrix function is used to retrieve the cached value of the
    # inverse Matrix
    getInverseMatrix <- function() invMatrx
    # Check whether the global variable matrx exists. If it does, check whether
    # the new matrix is same as that. If its the same, then dont make any changes
    # If its not the same, then set matrx to the new value and inverseMatrix to 
    # NULL.
    if (exists("matrx")) {
        if (!identical(matrx,matrxVar)) {
            set(matrxVar)
        }
    } else {
        set(matrxVar)        
    }
    # Return List.
    list(get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}
# Name      : cacheSolve
# Arguments : Matrix, additional arguments
# Return    : Inverse matrix of the input matrix
cacheSolve <- function(matrx, ...) {
    # Gets the current inverse matrix.
    invMatrx <- matrx$getInverseMatrix()
    # If the current inverse matrix is NULL, it implies that
    # the matrix being submitted is different from the previous
    # submission.
    if(!is.null(invMatrx)) {
        message("INFO: Proceeding to get cached data")
        return(invMatrx)
    }
    #Get the matrix
    data <- matrx$get()
    # Get the inverse matrix using the solve function. 
    tryCatch(invMatrx <- solve(data),error=function(e){
        invMatrx=NULL
        message("ERROR: Inverse matrix calculation for input matrix not possible")
        })
    matrx$setInverseMatrix(invMatrx)
    invMatrx
}