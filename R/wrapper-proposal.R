proposal_wrapper <- setClass(
	Class = "proposal_wrapper",
	representation = representation(
		pointer = "externalptr",
		times_of_surveys = "numeric",
		times_of_recaptures = "list",
		times_of_deaths = "numeric",
		known_deaths = "logical"
	),
	prototype = prototype(
		pointer = NULL,
		times_of_surveys = 0,
		times_of_recaptures = list(0),
		times_of_deaths = 0,
		known_deaths = FALSE
	),
	validity = function(object) {
		if ( !is.null(object@pointer) || 
				class(object@pointer) != "externalptr" ) return(FALSE)
		if ( !is.numeric(object@times_of_surveys) ) return(FALSE)
		if ( !all(sapply(object@times_of_recaptures,is.numeric)) ) return(FALSE)
		if ( !is.numeric(object@times_of_deaths) ) return(FALSE)
		if ( !is.logical(object@known_deaths) ) return(FALSE)
		return(TRUE)
	},
	sealed = TRUE
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "proposal_wrapper"),
	definition = function(.Object, 
		times_of_surveys, times_of_recaptures,
		times_of_deaths, known_deaths) {
	  if ( !is.vector(times_of_surveys) || !is.integer(as.integer(times_of_surveys))) {
    	stop("'times_of_surveys' must be convertible to a vector of integers.")
  	}
  	if ( !is.list(times_of_recaptures) || !all(sapply(times_of_recaptures, is.vector)) ||
       !all(sapply(times_of_recaptures, function(x) {is.integer(as.integer(x))}))
  	) {
    	stop("'times_of_recaptures' must be a list of vectors of integers.")
  	}
		if ( !is.vector(times_of_deaths) || 
				 !is.integer(as.integer(times_of_deaths)) ||
				 length(times_of_deaths) != length(times_of_recaptures) ) {
			stop("'times_of_deaths' must be a convertible to a vector of
					 integers and the same length as 'times_of_recaptures'.")
		}
		if ( !is.vector(known_deaths) || 
				 !is.logical(known_deaths) ||
				 length(known_deaths) != length(times_of_recaptures) ) {
			stop("'known_deaths' must be a convertible to a vector of
					 integers and the same length as 'times_of_recaptures'.")
		}


		.Object@times_of_surveys = times_of_surveys
		.Object@times_of_recaptures = times_of_recaptures
		.Object@times_of_deaths = times_of_deaths
		.Object@known_deaths = known_deaths
  	x <- list(  ## "-1" shifts to C/C++ indexing.
    	times_of_surveys = as.integer(times_of_surveys) - 1,
    	times_of_recaptures = lapply(
      	X = times_of_recaptures,
      	FUN = function(x) { return(as.integer(x) - 1) }),
			times_of_deaths = as.integer(times_of_deaths) - 1,
			known_deaths = known_deaths
  	)
  	.Object@pointer <- .Call("load_recapture_proposal", x=x, PACKAGE="gaga")
  	return(.Object)
	}
)

setMethod(
	f = "get_N",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		N <- .Call("get_N_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(N)
	}
)


setMethod(
	f = "get_K",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		N <- .Call("get_K_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(N)
	}
)

setMethod(
	f = "get_recaptures",
	signature = signature(.Object = "proposal_wrapper", id = "numeric"),
	definition = function(.Object, id) {
		if (!is.numeric(id) || (length(id) != 1) || 
			!is.integer(as.integer(id))) 
		{ 
			stop("Argument 'id' should be a numeric vector of length 1.")
		}
		if (id < 1) {
			stop("Argument 'id' should be a positive integer.")
		}
		N <- get_N(.Object)
		id <- as.integer(id)
		if (id > N) stop(paste("There are only ", N, " individuals.\n", sep=''))
		if (id < 1) stop(paste("The first id is '1'.\n", sep=''))
		id <- id - 1 ## "-1" shifts to C/C++ indexing.
		recaptures <- .Call("get_recaptures_proposal", 
												xp=.Object@pointer, id=id, PACKAGE="gaga")
		return(as.vector(recaptures))
	}
)


setMethod(
	f = "get_recaptures_matrix",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) { 
		recaptures_matrix <- matrix(
			data=NA, nrow=get_N(.Object), ncol=get_K(.Object))
		for ( i in 1:get_N(.Object)) {
			recaptures_matrix[i,] <- as.vector(get_recaptures(.Object, i))
		}
		return(recaptures_matrix)
	}
)

setMethod(
	f = "get_surveys",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		surveys <- .Call("get_surveys_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(surveys + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_births",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		births <- .Call("get_births_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(births + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_first_obs",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		first_obs <- .Call("get_first_obs_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(first_obs + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_last_obs",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		last_obs <- .Call("get_last_obs_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(last_obs + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_sampled",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		sampled <- .Call("get_sampled_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(sampled) 
	}
)

setMethod(
	f = "get_deaths",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		deaths <- .Call("get_deaths_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(deaths + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "set_deaths",
	signature = signature(.Object = "proposal_wrapper", 
												id = "numeric", times_of_deaths = "numeric"),
	definition = function(.Object, id, times_of_deaths) {
		if ( !is.vector(times_of_deaths) || 
				 !is.integer(as.integer(times_of_deaths)) ||
				 length(times_of_deaths) != length(id) ) {
			stop("'times_of_deaths' must be a convertible to a vector of
					 integers and the same length as 'times_of_recaptures'.")
		}
		
		if (!is.numeric(id) || !is.integer(as.integer(id))) 
		{ 
			stop("Argument 'id' should be a numeric vector of length 1.")
		}
		if (any(id < 1)) {
			stop("Argument 'id' should be a positive integer.")
		}
		N <- get_N(.Object)
		id <- as.integer(id)
		if (any(id > N)) stop(cat("There are only ", N, " individuals.\n", sep=''))
		if (any(id < 1)) stop(cat("The first id is '1'.\n", sep=''))

		id <- id - 1 ## "-1" shifts to C/C++ indexing.
		times_of_deaths <- times_of_deaths - 1  ## "-1" shifts to C/C++ indexing.
		deaths <- .Call("set_deaths_proposal", xp=.Object@pointer, 
										id = id, td = times_of_deaths)
		return(as.vector(deaths + 1)) ## "+1" shifts to R indexing.
	}
)


setMethod(
	f = "get_PHI",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		PHI <- .Call("get_PHI_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(PHI)
	}
)

setMethod(
	f = "get_P",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		P <- .Call("get_P_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(P)
	}
)

setMethod(
	f = "set_PHI",
	signature = signature(.Object = "proposal_wrapper", PHI = "matrix"),
	definition = function(.Object, PHI) {
		PHI_o <- .Call("set_PHI_proposal", xp=.Object@pointer, PHI_ = PHI, PACKAGE="gaga")
		return(PHI_o)
	}
)

setMethod(
	f = "set_P",
	signature = signature(.Object = "proposal_wrapper", P = "matrix"),
	definition = function(.Object, P) {
		P_o <- .Call("set_P_proposal", xp=.Object@pointer, P_ = P, PACKAGE="gaga")
		return(P_o)
	}
)

setMethod(
	f = "get_ll_phi_components",
	signature = signature(.Object = "proposal_wrapper", id="missing"),
	definition = function(.Object) {
		ll_phi_components <- .Call("get_ll_phi_components_proposal", 
															 xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(ll_phi_components))
	}
)

setMethod(
	f = "get_ll_phi_components",
	signature = signature(.Object = "proposal_wrapper", id="numeric"),
	definition = function(.Object, id) {
		if (!is.numeric(id) || !is.integer(as.integer(id))) 
		{ 
			stop("Argument 'id' should be a numeric vector.")
		}
		if (any(id < 1)) {
			stop("Argument 'id' should be a positive integer.")
		}
		N <- get_N(.Object)
		id <- as.integer(id)
		if (any(id > N)) stop(paste("There are only ", N, " individuals.\n", sep=''))
		if (any(id < 1)) stop(paste("The first id is '1'.\n", sep=''))
		id <- id - 1 # Shift to C/C++ indexing.
		ll_phi_components <- .Call("get_some_ll_phi_components_proposal", 
															 xp=.Object@pointer, indices=id, PACKAGE="gaga")
		return(as.vector(ll_phi_components))
	}
)

setMethod(
	f = "get_ll_p_components",
	signature = signature(.Object = "proposal_wrapper", id="missing"),
	definition = function(.Object) {
		ll_p_components <- .Call("get_ll_p_components_proposal", 
															 xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(ll_p_components))
	}
)


setMethod(
	f = "get_ll_p_components",
	signature = signature(.Object = "proposal_wrapper", id="numeric"),
	definition = function(.Object, id) {
		if (!is.numeric(id) || !is.integer(as.integer(id))) 
		{ 
			stop("Argument 'id' should be a numeric vector.")
		}
		if (any(id < 1)) {
			stop("Argument 'id' should be a (vector of) positive integer(s).")
		}
		N <- get_N(.Object)
		id <- as.integer(id)
		if (any(id > N)) stop(paste("There are only ", N, " individuals.\n", sep=''))
		if (any(id < 1)) stop(paste("The first id is '1'.\n", sep=''))
		id <- id - 1 # Shift to C/C++ indexing.
		ll_p_components <- .Call("get_some_ll_p_components_proposal", 
															 xp=.Object@pointer, indices=id, PACKAGE="gaga")
		return(as.vector(ll_p_components))
	}
)



setMethod(
	f = "get_log_posterior",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		lpd <- .Call("get_log_posterior_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(lpd)
	}
)


setMethod(
	f = "propose_deaths",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		log_asymmetry <- .Call("new_deaths_proposal", xp=.Object@pointer,
													 PACKAGE="gaga")
		return(log_asymmetry)
	}
)

setMethod(
	f = "get_last_pd",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		last_pd <- .Call("get_last_pd_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(last_pd)
	}
)
setMethod(
	f = "get_last_pd_vector",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		last_pd_vec <- .Call("calc_log_proposal_density", xp=.Object@pointer, PACKAGE="gaga")
		return(last_pd_vec)
	}
)

setMethod(
	f = "get_pd",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		pd <- .Call("get_pd_proposal", xp=.Object@pointer,
										 PACKAGE="gaga")
		return(pd)
	}
)

setMethod(
	f = "get_proposed_deaths",
	signature = signature(.Object = "proposal_wrapper"),
	definition = function(.Object) {
		deaths <- .Call("get_proposed_deaths_proposal", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(deaths + 1)) ## "+1" shifts to R indexing.
	}
)
