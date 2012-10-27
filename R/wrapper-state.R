state_wrapper <- setClass(
	Class = "state_wrapper",
	representation = representation(
		pointer = "externalptr",
		times_of_surveys = "numeric",
		times_of_recaptures = "list"
	),
	prototype = prototype(
		pointer = NULL,
		times_of_surveys = 0,
		times_of_recaptures = list(0)
	),
	validity = function(object) {
		if ( !is.null(object@pointer) || 
				class(object@pointer) != "externalptr" ) return(FALSE)
		if ( !is.numeric(object@times_of_surveys) ) return(FALSE)
		if ( !all(sapply(object@times_of_recaptures,is.numeric)) )
			return(FALSE)
		return(TRUE)
	},
	sealed = TRUE
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "state_wrapper"),
	definition = function(.Object, times_of_surveys, times_of_recaptures) {
	  if ( !is.vector(times_of_surveys) || !is.integer(as.integer(times_of_surveys))) {
    	stop("Times of surveys must be convertible to a vector of integers.")
  	}
  	if ( !is.list(times_of_recaptures) || !all(sapply(times_of_recaptures, is.vector)) ||
       !all(sapply(times_of_recaptures, function(x) {is.integer(as.integer(x))}))
  	) {
    	stop("Times of recaptures must be a list of vectors of integers.")
  	}
		.Object@times_of_surveys = times_of_surveys
		.Object@times_of_recaptures = times_of_recaptures
  	x <- list(  ## "-1" shifts to C/C++ indexing.
    	times_of_surveys = as.integer(times_of_surveys) - 1,
    	times_of_recaptures = lapply(
      	X = times_of_recaptures,
      	FUN = function(x) { return(as.integer(x) - 1) })
  	)
  	.Object@pointer <- .Call("load_recapture_state", x=x, PACKAGE="gaga")
  	return(.Object)
	}
)

setMethod(
	f = "get_N",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		N <- .Call("get_N_state", xp=.Object@pointer, PACKAGE="gaga")
		return(N)
	}
)


setMethod(
	f = "get_K",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		N <- .Call("get_K_state", xp=.Object@pointer, PACKAGE="gaga")
		return(N)
	}
)

setMethod(
	f = "get_recaptures",
	signature = signature(.Object = "state_wrapper", id = "numeric"),
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
		if (id > N) stop(cat("There are only ", N, " individuals.\n", sep=''))
		if (id < 1) stop(cat("The first id is '1'.\n", sep=''))
		id <- id - 1 ## "-1" shifts to C/C++ indexing.
		recaptures <- .Call("get_recaptures_state", 
												xp=.Object@pointer, id=id, PACKAGE="gaga")
		return(as.vector(recaptures))
	}
)


setMethod(
	f = "get_recaptures_matrix",
	signature = signature(.Object = "state_wrapper"),
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
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		surveys <- .Call("get_surveys_state", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(surveys + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_births",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		births <- .Call("get_births_state", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(births + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_first_obs",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		first_obs <- .Call("get_first_obs_state", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(first_obs + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_last_obs",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		last_obs <- .Call("get_last_obs_state", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(last_obs + 1)) ## "+1" shifts to R indexing.
	}
)

setMethod(
	f = "get_deaths",
	signature = signature(.Object = "state_wrapper"),
	definition = function(.Object) {
		deaths <- .Call("get_deaths_state", xp=.Object@pointer, PACKAGE="gaga")
		return(as.vector(deaths + 1)) ## "+1" shifts to R indexing.
	}
)









