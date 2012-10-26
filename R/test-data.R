load_get_compare_K <- function(times_of_surveys, times_of_recaptures) {
	ptr <- data_wrapper(times_of_surveys, times_of_recaptures)
	K <- max(times_of_surveys)
	test <- ( K == get_K(ptr))
	if (!test) {
		fn <- sys.call()[[1]]
		msg <- cat(
			"Function '", fn, "' failed: check occasion counting.\n",
			"\tOriginal K: ", K, "\n",
			"\tReturned K: ", get_K(ptr), "\n",
			"\n"
		)
		warning(msg)
		return(ptr)
	}
	return(TRUE)
}


load_get_compare_N <- function(times_of_surveys, times_of_recaptures) {
	ptr <- data_wrapper(times_of_surveys, times_of_recaptures)
	N <- length(times_of_recaptures)
	test <- ( N == get_N(ptr))
	if (!test) {
		fn <- sys.call()[[1]]
		msg <- cat(
			"Function '", fn, "' failed: check individual counting.\n",
			"\tOriginal N: ", N, "\n",
			"\tReturned N: ", get_N(ptr), "\n",
			"\n"
		)
		warning(msg)
		return(ptr)
	}
	return(TRUE)
}

load_get_compare_surveys <- function(times_of_surveys, times_of_recaptures) {
	ptr <- data_wrapper(times_of_surveys, times_of_recaptures)
	tos <- get_surveys(ptr)
	test <- all(tos == times_of_surveys)
	if (!test) {
		fn <- sys.call()[[1]]
		msg <- cat(
			"Function '", fn, "' failed: check surveys.\n",
			"\tOriginal surveys: ", times_of_surveys, "\n",
			"\tReturned surveys: ", get_surveys(ptr), "\n",
			"\n"
		)
		warning(msg)
		return(ptr)
	}
	return(TRUE)
}


load_get_compare_births <- function(times_of_surveys, times_of_recaptures) {
	ptr <- data_wrapper(times_of_surveys, times_of_recaptures)
	original_births <- sapply( times_of_recaptures, min )
	returned_births <- as.vector(get_births(ptr))
	test <- all(original_births == returned_births)
	if (!test) {
		fn <- sys.call()[[1]]
		msg <- cat(
			"Function '", fn, "' failed: check births.\n",
			"\tOriginal births: ", original_births, "\n",
			"\tReturned births: ", returned_births, "\n",
			"\n"
		)
		warning(msg)
		return(ptr)
	}
	return(TRUE)
}

load_get_compare_first_obs <- function(times_of_surveys, times_of_recaptures) {
	ptr <- data_wrapper(times_of_surveys, times_of_recaptures)
	original_first_obs <- sapply( times_of_recaptures, min )
	returned_first_obs <- as.vector(get_first_obs(ptr))
	test <- all(original_first_obs == returned_first_obs)
	if (!test) {
		fn <- sys.call()[[1]]
		msg <- cat(
			"Function '", fn, "' failed: check first_obs.\n",
			"\tOriginal first_obs: ", original_first_obs, "\n",
			"\tReturned first_obs: ", returned_first_obs, "\n",
			"\n"
		)
		warning(msg)
		return(ptr)
	}
	return(TRUE)
}

load_get_compare_last_obs <- function(times_of_surveys, times_of_recaptures) {
	ptr <- data_wrapper(times_of_surveys, times_of_recaptures)
	original_last_obs <- sapply( times_of_recaptures, max )
	returned_last_obs <- as.vector(get_last_obs(ptr))
	test <- all(original_last_obs == returned_last_obs)
	if (!test) {
		fn <- sys.call()[[1]]
		msg <- cat(
			"Function '", fn, "' failed: check last_obs.\n",
			"\tOriginal last_obs: ", original_last_obs, "\n",
			"\tReturned last_obs: ", returned_last_obs, "\n",
			"\n"
		)
		warning(msg)
		return(ptr)
	}
	return(TRUE)
}

load_get_compare_recaptures <- function(times_of_surveys, times_of_recaptures) {
	ptr <- data_wrapper(times_of_surveys, times_of_recaptures)
	recaps <- list()
	w_recaps <- list()
	for ( i in 1:length(times_of_recaptures)) {
		recaps[[i]] <- get_recaptures(ptr,i) 
		w_recaps[[i]] <- which(recaps[[i]] == 1)
		test <- all(w_recaps[[i]] == times_of_recaptures[[i]])
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- cat(
				"Function '", fn, "' failed: check data loading/unloading.\n",
				"\tIndividual: ", i, "\n",
				"\tOriginal recaptures: ", times_of_recaptures[[i]], "\n",
				"\tReturned recaptures: ", w_recaps[[i]], "\n",
				"\n"
			)
			warning(msg)
			return(ptr)
		}
	}
	return(TRUE)
}





