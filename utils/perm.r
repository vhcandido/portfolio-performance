#' @title Generate all permutations of vectors
#'
#' @description
#' Generate all permutations of vectors. This function is strongly based on base::expand.grid
#' but simpler and it respects the orders of vectors passed as arguments. It also has a
#' better explanation for myself, so I can know whats happens inside it.
#'
#' @param ... vectors, factors or a list containing these
#'
#' @return A matrix containing one row for each combination of the supplied factors. The latest factors vary fastest.
#' The columns are not labelled.
#' 
#' @details
#' Explanation
#' The inner rep.int checks how many times each vector element will repeat
#' 	idx <- rep.int(orep, nx)
#' Second one repeates the index of each vector position this many times
#' 	idx <- rep.int(seq_len(nx), idx)
#' The outer one repeates this whole array 'rep.fac' times to match the number of permutations
#' 	idx <- rep.int(idx, rep.fac)
#' 	
#' Example
#' y = c(10, 20, 30); z = c('a', 'b')
#' i = 1
#' 	idx <- rep.int(2, 3) => c(2, 2, 2)
#' 	idx <- rep.int(1:3, idx) => c(1, 1, 2, 2, 3, 3)
#' 	idx <- rep.int(idx, 1) => c(1, 1, 2, 2, 3, 3)
#' 	x <- x[idx] => c(10, 10, 20, 20, 30, 30)
#' i = 2
#' 	idx <- rep.int(1, 2) => c(1, 1)
#' 	idx <- rep.int(1:2, idx) => c(1, 2)
#' 	idx <- rep.int(idx, 3) => c(1, 2, 1, 2, 1, 2)
#' 	x <- x[idx] => c('a', 'b', 'a', 'b', 'a', 'b')
#'
#' @examples
#' x <- 1:5
#' y <- c(10, 20, 30)
#' z <- c('a', 'b')
#' 
#' perm(x,y,z)
#' 
perm <- function(...) {
	nargs <- length(args <- list(...))
	if (nargs == 1L && is.list(a1 <- args[[1L]]))
		nargs <- length(args <- a1)
	rep.fac <- 1L
	orep <- prod(lengths(args))
	cargs <- list()
	for(i in seq_len(nargs)) {
		x <- args[[i]]
		nx <- length(x)
		orep <- orep/nx
		
		# Both ways produce the same result
		x <- rep.int(rep.int(x, rep.int(orep, nx)), rep.fac)
		#x <- x[rep.int(rep.int(seq_len(nx), rep.int(orep, nx)), rep.fac)]
		
		rep.fac <- rep.fac * nx
		cargs[[i]] <- x
	}
	do.call('cbind', cargs)
}
