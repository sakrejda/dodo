setMethod(
	f = "pool",
	signature = signature(
		l = "popList"
	),
	definition = function(l) {
		####### Check compatability, maybe with hash generated at creation
		####### and propagated by inheritance?
		szs <- lapply(l, function(x) x@sizes)
		szm <- matrix(data=unlist(szs), nrow=l[[1]]@n_bins)
		## Saving a round of allocation:
		l[[1]]@sizes <- rowSums(szm)
		return(l[1])
	}
)
