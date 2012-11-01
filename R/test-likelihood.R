setMethod(
	f = "compare_K",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		K <- max(.Object@times_of_surveys)
		test <- ( K == get_K(.Object))
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- paste(
				"Function '", fn, "' failed: check occasion counting.\n",
				"\tOriginal K: ", K, "\n",
				"\tReturned K: ", get_K(.Object), "\n",
				"\n", sep=''
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)


setMethod(
	f = "compare_N",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		N <- length(.Object@times_of_recaptures)
		test <- ( N == get_N(.Object))
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- paste(
				"Function '", fn, "' failed: check individual counting.\n",
				"\tOriginal N: ", N, "\n",
				"\tReturned N: ", get_N(.Object), "\n",
				"\n", sep=''
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_surveys",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		tos <- get_surveys(.Object)
		test <- all(tos == .Object@times_of_surveys)
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- paste(
				"Function '", fn, "' failed: check surveys.\n",
				"\tOriginal surveys: ", times_of_surveys, "\n",
				"\tReturned surveys: ", get_surveys(.Object), "\n",
				"\n", sep=''
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_births",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		original_births <- sapply( .Object@times_of_recaptures, min )
		returned_births <- as.vector(get_births(.Object))
		test <- all(original_births == returned_births)
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- paste(
				"Function '", fn, "' failed: check births.\n",
				"\tOriginal births: ", original_births, "\n",
				"\tReturned births: ", returned_births, "\n",
				"\n", sep=''
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_deaths",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		original_deaths <- .Object@times_of_deaths
		returned_deaths <- as.vector(get_deaths(.Object))
		test <- all(original_deaths == returned_deaths)
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- paste(
				"Function '", fn, "' failed: check deaths.\n",
				"\tOriginal deaths: ", original_deaths, "\n",
				"\tReturned deaths: ", returned_deaths, "\n",
				"\n", sep=''
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_first_obs",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		original_first_obs <- sapply(.Object@times_of_recaptures, min )
		returned_first_obs <- as.vector(get_first_obs(.Object))
		test <- all(original_first_obs == returned_first_obs)
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- paste(
				"Function '", fn, "' failed: check first_obs.\n",
				"\tOriginal first_obs: ", original_first_obs, "\n",
				"\tReturned first_obs: ", returned_first_obs, "\n",
				"\n", sep=''
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_last_obs",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		original_last_obs <- sapply( .Object@times_of_recaptures, max )
		returned_last_obs <- as.vector(get_last_obs(.Object))
		test <- all(original_last_obs == returned_last_obs)
		if (!test) {
			fn <- sys.call()[[1]]
			msg <- paste(
				"Function '", fn, "' failed: check last_obs.\n",
				"\tOriginal last_obs: ", original_last_obs, "\n",
				"\tReturned last_obs: ", returned_last_obs, "\n",
				"\n", sep=''
			)
			warning(msg)
			return(.Object)
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_recaptures",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		recaps <- list()
		w_recaps <- list()
		for ( i in 1:length(.Object@times_of_recaptures)) {
			recaps[[i]] <- get_recaptures(.Object,i) 
			w_recaps[[i]] <- which(recaps[[i]] == 1)
			test <- all(w_recaps[[i]] == .Object@times_of_recaptures[[i]])
			if (!test) {
				fn <- sys.call()[[1]]
				msg <- paste(
					"Function '", fn, "' failed: check likelihood loading/unloading.\n",
					"\tIndividual: ", i, "\n",
					"\tOriginal recaptures: ", .Object@times_of_recaptures[[i]], "\n",
					"\tReturned recaptures: ", w_recaps[[i]], "\n",
					"\n", sep=''
				)
				warning(msg)
				return(.Object)
			}	
		}
		return(TRUE)
	}
)

setMethod(
	f = "compare_ll_phi_components",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object, PHI) {
		PHI <- set_PHI(.Object, PHI)
		cpp_ll_phi <- get_ll_phi_components(.Object)
		R_ll_phi <- R_ll_phi_FUN(.Object)
		test <- isTRUE(all.equal(cpp_ll_phi,R_ll_phi))
		if (!test) {
			for ( i in 1:get_N(.Object) ) {
				test_i <- isTRUE(all.equal(cpp_ll_phi[i], R_ll_phi[i]))
				fn <- sys.call()[[1]]
				msg <- paste(
					"Function '", fn, "' failed: check phi likelihood calculation.\n",
					"\tIndividual: ", i, "\n",
					"\tR\t\tphi components: ", R_ll_phi[i], "\n",
					"\tC++\t\t phi components: ", cpp_ll_phi[i], "\n",
					"\n", sep=''
				)
				warning(msg)
				return(.Object)
			}
		}
		return(TRUE)
	}
)


setMethod(
	f = "compare_ll_p_components",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object, P) {
		P <- set_P(.Object, P)
		cpp_ll_p <- get_ll_p_components(.Object)
		R_ll_p <- R_ll_p_FUN(.Object)
		test <- isTRUE(all.equal(cpp_ll_p,R_ll_p))
		if (!test) {
			for ( i in 1:get_N(.Object) ) {
				test_i <- isTRUE(all.equal(cpp_ll_p[i], R_ll_p[i]))
				fn <- sys.call()[[1]]
				msg <- paste(
					"Function '", fn, "' failed: check p likelihood calculation.\n",
					"\tIndividual: ", i, "\n",
					"\tR\t\tp components: ", R_ll_p[i], "\n",
					"\tC++\t\tp components: ", cpp_ll_p[i], "\n",
					"\n", sep=''
				)
				warning(msg)
				return(.Object)
			}
		}
		return(TRUE)
	}
)


setMethod(
	f = "compare_ll_phi_getters",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		ll_phi_complete <- get_ll_phi_components(.Object)
		ll_phi_from_partials <- ll_phi_complete
		for ( i in 1:get_N(.Object)) {
			ll_phi_from_partials[i] <- get_ll_phi_components(.Object,i)
		}
		test <- isTRUE(all.equal(ll_phi_complete, ll_phi_from_partials))
		if (!test) {
			for ( i in 1:get_N(.Object) ) {
				test_i <- isTRUE(all.equal(ll_phi_complete[i], ll_phi_from_partials[i]))
				fn <- sys.call()[[1]]
				msg <- paste(
					"Function '", fn, "' failed: check partial likelihood getters for phi.\n",
					"\tIndividual: ", i, "\n",
					"\tComplete phi components: ", ll_phi_from_partials[i], "\n",
					"\tPartial phi components:  ", ll_phi_complete[i], "\n",
					"\n", sep=''
				)
				warning(msg)
				return(.Object)
			}
		}
		return(TRUE)
	}
)


setMethod(
	f = "compare_ll_p_getters",
	signature = signature(.Object = "likelihood_wrapper"),
	definition = function(.Object) {
		ll_p_complete <- get_ll_p_components(.Object)
		ll_p_from_partials <- ll_p_complete
		for ( i in 1:get_N(.Object)) {
			ll_p_from_partials[i] <- get_ll_p_components(.Object,i)
		}
		test <- isTRUE(all.equal(ll_p_complete, ll_p_from_partials))
		if (!test) {
			for ( i in 1:get_N(.Object) ) {
				test_i <- isTRUE(all.equal(ll_p_complete[i], ll_p_from_partials[i]))
				fn <- sys.call()[[1]]
				msg <- paste(
					"Function '", fn, "' failed: check partial likelihood getters for p.\n",
					"\tIndividual: ", i, "\n",
					"\tComplete p components: ", ll_p_from_partials[i], "\n",
					"\tPartial p components:  ", ll_p_complete[i], "\n",
					"\n", sep=''
				)
				warning(msg)
				return(.Object)
			}
		}
		return(TRUE)
	}
)
