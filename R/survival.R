R_ll_phi_FUN <- function(.Object) {
	PHI <- get_PHI(.Object)
	births <- get_births(.Object)
	deaths <- get_deaths(.Object)
	survived <- as.list(1:get_N(.Object))
	survived <- lapply(survived, function(id, births, deaths) {
		last_alive <- deaths[id] - 2
		if ( births[id] < last_alive ) {
			return( births[id]:last_alive )
		} else {
			return(NA)
		}
	}, births = births, deaths = deaths)
	death_intervals <- deaths - 1
	ll_phi <- vector(mode="numeric", length=get_N(xpl))
	for ( i in 1:get_N(xpl) ) {
		ll_phi[i] <- sum(log(PHI[i, survived[[i]] ])) + log(1-PHI[i,death_intervals[i]])
	}
	return(ll_phi)
}

R_ll_p_FUN <- function(.Object) {
	P <- get_P(.Object)
	available <- as.list(1:get_N(.Object))
	available <- lapply(available, function(id, births, deaths) {
		last_available <- deaths[id] - 1
		if ( births[id] == last_available ) {
			return(NA)
		} else if (births[id] > last_available){
			stop("Inconsistent state, check births/deaths.")
		} else {
			return((births[id]+1):last_available)
		}
	}, births = get_births(.Object), deaths = get_deaths(.Object))
	ll_p <- vector(mode="numeric", length=get_N(xpl))
	for ( i in 1:get_N(xpl) ) {
		recaps  <- which(get_recaptures(.Object,i)==1)
		recaps  <- recaps[recaps %in% available[[i]] &
											recaps %in% get_surveys(.Object)]
		noncaps <- which(get_recaptures(.Object,i)==0)
		noncaps <- noncaps[noncaps %in% available[[i]] & 
											 noncaps %in% get_surveys(.Object)]
		ll_p[i] <- sum(log(P[i,recaps])) + sum(log(1-P[i,noncaps]))
	}
	return(ll_p)
}
