load_recapture_data <- function(times_of_surveys, times_of_recaptures) {
	if ( !is.vector(times_of_surveys) || !is.integer(as.integer(times_of_surveys))) {
		stop("Times of surveys must be convertible to a vector of integers.")	
	}
	if ( !is.list(times_of_recaptures) || !all(sapply(times_of_recaptures, is.vector)) ||
			 !all(sapply(times_of_recaptures, function(x) {is.integer(as.integer(x))}))
	) {
		stop("Times of recaptures must be a list of vectors of integers.")
	}
	x <- list(  ## "-1" shifts to C/C++ indexing.
		times_of_surveys = as.integer(times_of_surveys) - 1, 
		times_of_recaptures = lapply(
			X = times_of_recaptures, 
			FUN = function(x) { return(as.integer(x) - 1) } )
	)
	ptr <- .Call("load_recapture_data", x=x, PACKAGE="gaga")
	return(ptr)
}

get_N <- function(ptr) {
	N <- .Call("get_N_data", ptr=ptr, PACKAGE="gaga")
	return(N)
}

get_K <- function(ptr) {
	K <- .Call("get_K_data", ptr=ptr, PACKAGE="gaga")
	return(K)
}

get_recaptures <- function(ptr, id) {
	if (!is.numeric(id) || (length(id) != 1) || 
			!is.integer(as.integer(id))) 
	{ 
		stop("Argument 'id' should be a numeric vector of length 1.")
	}
	if (id < 1) {
		stop("Argument 'id' should be a positive integer.")
	}
	N <- get_N(ptr)
	id <- as.integer(id)
	if (id > N) stop(cat("There are only ", N, " individuals.\n", sep=''))
	if (id < 1) stop(cat("The first id is '1'.\n", sep=''))
	id <- id - 1 ## "-1" shifts to C/C++ indexing.
	recaptures <- .Call("get_recaptures_data", ptr=ptr, id=id)
	return(recaptures)  
}

get_recaptures_matrix <- function(ptr) { 
	recaptures_matrix <- matrix(data=NA, nrow=get_N(ptr), ncol=get_K(ptr))
	for ( i in 1:get_N(ptr)) {
		recaptures_matrix[i,] <- as.vector(get_recaptures(ptr, i))
	}
	return(recaptures_matrix)
}

get_surveys <- function(ptr) {
	surveys <- .Call("get_surveys_data", ptr=ptr)
	return(surveys + 1 ) ## "+1" shifts to R indexing.
}

get_births <- function(ptr) {
	births <- .Call("get_births_data", ptr=ptr)
	return(births + 1 ) ## "+1" shifts to R indexing.
}

get_first_obs <- function(ptr) {
	first_obs <- .Call("get_first_obs_data", ptr=ptr)
	return(first_obs + 1 ) ## "+1" shifts to R indexing.
}

get_last_obs <- function(ptr) {
	last_obs <- .Call("get_last_obs_data", ptr=ptr)
	return(last_obs + 1 ) ## "+1" shifts to R indexing.
}

### STATE or children only:
get_deaths <- function(ptr) {
	deaths <- .Call("get_deaths_state", ptr=ptr)
	return(deaths + 1 ) ## "+1" shifts to R indexing.
}



