setMethod(
	f = "compare_K",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		K <- max(.Object@times_of_surveys)
		test <- ( K == get_K(.Object))
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- cat(
				"Function '", fn, "' failed: check occasion counting.\n",
				"\tOriginal K: ", K, "\n",
				"\tReturned K: ", get_K(.Object), "\n",
				"\n"
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)


setMethod(
	f = "compare_N",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		N <- length(.Object@times_of_recaptures)
		test <- ( N == get_N(.Object))
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- cat(
				"Function '", fn, "' failed: check individual counting.\n",
				"\tOriginal N: ", N, "\n",
				"\tReturned N: ", get_N(.Object), "\n",
				"\n"
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_surveys",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		tos <- get_surveys(.Object)
		test <- all(tos == .Object@times_of_surveys)
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- cat(
				"Function '", fn, "' failed: check surveys.\n",
				"\tOriginal surveys: ", times_of_surveys, "\n",
				"\tReturned surveys: ", get_surveys(.Object), "\n",
				"\n"
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_births",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		original_births <- sapply( .Object@times_of_recaptures, min )
		returned_births <- as.vector(get_births(.Object))
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
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_deaths",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		original_deaths <- sapply( .Object@times_of_recaptures, min )
		returned_deaths <- as.vector(get_deaths(.Object))
		test <- all(original_deaths == returned_deaths)
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- cat(
				"Function '", fn, "' failed: check deaths.\n",
				"\tOriginal deaths: ", original_deaths, "\n",
				"\tReturned deaths: ", returned_deaths, "\n",
				"\n"
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_first_obs",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		original_first_obs <- sapply(.Object@times_of_recaptures, min )
		returned_first_obs <- as.vector(get_first_obs(.Object))
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
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_last_obs",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		original_last_obs <- sapply( .Object@times_of_recaptures, max )
		returned_last_obs <- as.vector(get_last_obs(.Object))
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
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_recaptures",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		recaps <- list()
		w_recaps <- list()
		for ( i in 1:length(times_of_recaptures)) {
			recaps[[i]] <- get_recaptures(.Object,i) 
			w_recaps[[i]] <- which(recaps[[i]] == 1)
			test <- all(w_recaps[[i]] == .Object@times_of_recaptures[[i]])
			if (!test) {
				fn <- sys.call()[[1]]
				msg <- cat(
					"Function '", fn, "' failed: check state loading/unloading.\n",
					"\tIndividual: ", i, "\n",
					"\tOriginal recaptures: ", .Object@times_of_recaptures[[i]], "\n",
					"\tReturned recaptures: ", w_recaps[[i]], "\n",
					"\n"
				)
				warning(msg)
				return(.Object)
			}	
		}
		return(TRUE)
	}
)




