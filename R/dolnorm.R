dolnorm <- function(x, y, meanlog, sdlog, log=FALSE) { 
	if (log == TRUE) stop("LOG=TRUE not implemented.")
	suppressWarnings(
		o <- 1/(sqrt(2*pi)*sdlog*(x-y))*exp(-((log(x-y)-meanlog)^2)/(2*sdlog^2)) 
	)
	o[!is.finite(o)] <- 0
	return(o)
}

