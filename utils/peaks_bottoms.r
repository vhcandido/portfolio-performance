#' @title Peaks and bottoms 
#'
#' @description
#' Finds peaks and bottoms of series
#'
#' @param x Series to find its peaks and bottoms
#' @param n Neighbour offset to assure that it's a peak/bottom (default: 2)
#'
#' @return A list containing \code{x}'s peaks and bottoms
#'
#' @examples
#' # Generating a random series
#' x <- cumsum(rnorm(100))
#'
#' # Finding its peaks and bottoms
#' pb <- peaks.bottoms(x, n = 2)
#'
#' # Plotting the computed data
#' plot(x, type = 'l')
#' 
#' peaks <- bottoms <- x
#' peaks[which(!pb$peaks)] <- NA
#' bottoms[which(!pb$bottoms)] <- NA
#' 
#' points(peaks, col = 3, pch = 2)
#' points(bottoms, col = 2, pch = 6)
peaks.bottoms <- function(x, n = 2) {
	x <- as.numeric(x)
	p <- rep(TRUE, length(x))
	b <- rep(TRUE, length(x))
	for(i in 1:n) {
		p.right <- x[(1+i):(length(x)-i)] > x[(1+2*i):length(x)]
		p.left <- x[(1+i):(length(x)-i)] > x[1:(length(x)-2*i)]
		p.sub <- p.right & p.left
		p <- p & c(rep(FALSE, i), p.sub, rep(FALSE, i))
		
		b.right <- x[(1+i):(length(x)-i)] < x[(1+2*i):length(x)]
		b.left <- x[(1+i):(length(x)-i)] < x[1:(length(x)-2*i)]
		b.inner <- b.right & b.left
		b <- b & c(rep(FALSE, i), b.inner, rep(FALSE, i))
	}
	ret <- list(peaks = p, bottoms = b)
}