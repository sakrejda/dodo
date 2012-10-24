normal_lpd_test <- function(x) {
	noms <- names(x)
	if (!('x' %in% noms) | !is.numeric(x[['x']])) {
		stop("List element 'x' must be a numeric vector of data.")
	}
	if (!('mu' %in% noms) | (length(x[['mu']])!=1) | !is.numeric(x[['mu']])) {
		stop("List element 'mu' must be a numeric vector of length 1.")
	}
	if (!('sd' %in% noms) | (length(x[['sd']])!=1) |
			!is.numeric(x[['sd']])) {
		stop("List element 'sd' must be a numeric vector of length 1.")
	}
	if (!('mu_init' %in% noms) | (length(x[['mu_init']])!=1) |
			!is.numeric(x[['mu_init']])) {
		stop("List element 'mu_init' must be a numeric vector of length 1.")
	}
	if (!('sd_init' %in% noms) | (length(x[['sd_init']])!=1) |
			!is.numeric(x[['sd_init']])) {
		stop("List element 'sd_init' must be a numeric vector of length 1.")
	}
	.Call("normal_lpd_test", x=x, PACKAGE="mcmc")
}

normal_ar_test <- function(x, n) {
	noms <- names(x)
	if (!('x' %in% noms) | !is.numeric(x[['x']])) {
		stop("List element 'x' must be a numeric vector of data.")
	}
	if (!('mu' %in% noms) | (length(x[['mu']])!=1) | !is.numeric(x[['mu']])) {
		stop("List element 'mu' must be a numeric vector of length 1.")
	}
	if (!('sd' %in% noms) | (length(x[['sd']])!=1) |
			!is.numeric(x[['sd']])) {
		stop("List element 'sd' must be a numeric vector of length 1.")
	}
	if (!('mu_init' %in% noms) | (length(x[['mu_init']])!=1) |
			!is.numeric(x[['mu_init']])) {
		stop("List element 'mu_init' must be a numeric vector of length 1.")
	}
	if (!('sd_init' %in% noms) | (length(x[['sd_init']])!=1) |
			!is.numeric(x[['sd_init']])) {
		stop("List element 'sd_init' must be a numeric vector of length 1.")
	}
	if (!is.numeric(n) | (length(n)!=1) | !is.integer(as.integer(n)) | n<1) {
		stop("Argument 'n' must be a positive integer vector of length 1.")
	}
	.Call("normal_ar_test", x=x, n=n, PACKAGE="mcmc")
}

normal_chain_test  <- function(x, n, out.file) {
	noms <- names(x)
	if (!('x' %in% noms) | !is.numeric(x[['x']])) {
		stop("List element 'x' must be a numeric vector of data.")
	}
	if (!('mu' %in% noms) | (length(x[['mu']])!=1) | !is.numeric(x[['mu']])) {
		stop("List element 'mu' must be a numeric vector of length 1.")
	}
	if (!('sd' %in% noms) | (length(x[['sd']])!=1) |
			!is.numeric(x[['sd']])) {
		stop("List element 'sd' must be a numeric vector of length 1.")
	}
	if (!('mu_init' %in% noms) | (length(x[['mu_init']])!=1) |
			!is.numeric(x[['mu_init']])) {
		stop("List element 'mu_init' must be a numeric vector of length 1.")
	}
	if (!('sd_init' %in% noms) | (length(x[['sd_init']])!=1) |
			!is.numeric(x[['sd_init']])) {
		stop("List element 'sd_init' must be a numeric vector of length 1.")
	}
	if (!is.numeric(n) | (length(n)!=1) | !is.integer(as.integer(n)) | n<1) {
		stop("Argument 'n' must be a positive integer vector of length 1.")
	}
	if (!is.character(out.file)) {
		stop("Argument 'outfile' must be a valid filename.")
	}
	.Call("normal_chain_test", x=x, n=n, o=out.file, PACKAGE="mcmc")
}


