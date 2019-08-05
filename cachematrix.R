## OBJECT CONTAINING METHODS FOR GET/SET/INVERSE AND MATRIX OBJECT
makeCacheMatrix <- function(mat = matrix()) {
  
  # INITIALIZE LIKE IN TUTORIAL
  invMat <- NULL
  
  # SET METHOD
  set <- function(new) {
    mat <<- new
  }
  
  # GET METHOD
  get <- function() {
    # LOADING FROM DIFF ENV
    return(mat)
  }
  
  # SET INVERSION OF MATRIX
  setInverse <- function(inverse) {
    ## THIS IS FOR SAVING INTO DIFF ENV
    invMat <<- inverse
  }
  
  # RETURN CACHED INVERTED MATRIX
  getInverse <- function() {
    return(invMat)
  }
  
  methods <- list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  return (methods)
}

## RETURN EITHER CACHED INVERSION MATRIX OF THE MATRIX, OR COMPUTE THE NEW ONE
cacheSolve <- function(mat, ...) {
  
  # TRY RESTORE CACHED DATA:
  inversion <- mat$getInverse()
  if (!is.null(inversion)) {
    return(inversion)
  }
  
  # NEW DATA:
  data <- mat$get()
  # GET INV OF MAT
  inversion <- solve(data, ...)
  # SET IN FOR NEW DATA
  mat$setInverse(inversion)
  
  return(inversion)
}


## TEST CASES:
testVector <- c(5,4,3,7,11,23,45,678,211)
testDim <- sqrt(length(testVector))
testMatrix <- matrix(testVector,testDim, testDim)
testMatrixAsParam <- makeCacheMatrix(testMatrix)

## BOTH OBJECTS RETURNED ARE THE SAME, ONE IS NEW, SECOND IS CACHED OLD :)
beforeCacheProcess <- cacheSolve(testMatrixAsParam) 
print(beforeCacheProcess)
afterCacheProcess <- cacheSolve(testMatrixAsParam)
print(afterCacheProcess)
