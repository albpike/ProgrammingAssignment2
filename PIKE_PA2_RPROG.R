## Audrey Lei Pike

create <- function(m = matrix()) {
	input <- NULL

 	put <- function(n) {
  	m <<- n
 	input <<- NULL
  }

 	pick <- function() m

 	putinv <- function(inv) 
 	input <<- inv
 	
	pickinv <- function() input

 	list(put = put, pick = pick, putinv = putinv, pickinv = pickinv)
}


compute <- function(m, ...) {
	input <- m$pickinv()
	
 	if (!is.null(input)) {
 	  message("Extracting Cached Matrix")
	  return(input)
 	}
	
 	o <- m$pick()
 	input <- solve(o, ...)
	m$putinv(input)
 	input
}
