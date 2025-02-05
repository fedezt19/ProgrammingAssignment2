## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

mmakeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Inicializa la variable para almacenar la inversa
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Borra la inversa almacenada cuando la matriz cambia
  }
  
  get <- function() x  # Devuelve la matriz original
  
  setInverse <- function(inverse) inv <<- inverse  # Guarda la inversa en caché
  
  getInverse <- function() inv  # Obtiene la inversa almacenada en caché
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Obteniendo datos almacenados en caché")
    return(inv)  # Devuelve la inversa almacenada en caché
  }
  
  data <- x$get()  # Obtiene la matriz original
  inv <- solve(data, ...)  # Calcula la inversa
  x$setInverse(inv)  # Almacena la inversa en caché
  
  inv
}
