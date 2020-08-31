makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInversa <- function(inversa) i <<- inversa
  getInversa <- function() i 
  list(set = set, get = get, 
       setInversa = setInversa, 
       getInversa = getInversa)
}


cacheSolve <- function(x, ...) {
  ## Devuelve la matriz inversa de x
  j <- x$getInversa()
  if(!is.null(j)){
    message("inversa en caché")
    return(j)
  }
  matriz <- x$get()
  j <- solve(matriz,...)
  x$setInversa(j)
  j
}


