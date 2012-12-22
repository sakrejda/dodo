setMethod(
	f = "stage_names",
	signature = signature(.Object = "population"),
	definition = function(.Object) {
		return(stage_names(.Object@life_cycle))
	}
)

setMethod(
	f = "+",
	signature = signature(e1 = "population", e2 = "size_distribution"),
	definition = function(e1, e2) {
		if (e2@stage_name %in% stage_names(e1)) {
			e1@promoted[[e2@stage_name]] <-
				c(e1@promoted[[e2@stage_name]], e2)
			
		} else {
			msg <- paste(
				"'", e2@stage_name, ", does not belong in the population.",
				sep = '')
			stop(msg)
		}
		return(e1)
	}
)

setMethod(
	f = "+",
	signature = signature(e1 = "size_distribution", e2 = "population"),
	definition = function(e1, e2) e2 + e1
)

setMethod(
	f = "sync",
	signature = signature(p = "population"),
	definition = function(p) {
		pool <- function(l) {
			if (is.list(l) && length(l) == 0) return(l)
			####### Check compatability, maybe with hash generated at creation
			####### and propagated by inheritance?
			szs <- lapply(l, function(x) x@sizes)
			szm <- matrix(data=unlist(szs), nrow=l[[1]]@n_bins)
			## Saving a round of allocation:
			l[[1]]@sizes <- rowSums(szm)
			return(l[1])
		}

		noms <- stage_names(p)
		### Parallelize:
		for ( nom in noms) {
			p@space[[nom]] <-
				c(p@space[[nom]],p@promoted[[nom]])
			p@space[[nom]] <- pool(p@space[[nom]])
			p@promoted[[nom]] <- list()
		}
		return(p)
	}
)

setMethod(
	f = "grow",
	signature = signature(p = "population"),
	definition = function(p) {
		noms <- stage_names(p)
		### Parallelize:
		for ( nom in noms ) {
			p@space[[nom]] <- grow(p@space[[nom]])
		}
		return(p)
	}
)

setMethod(
	f = "smolt",
	signature = signature(p = "population"),
	definition = function(p) {
		noms <- stage_names(p)
		### Parallelize:
		for ( nom in noms ) {
			p@space[[nom]] <- smolt(p@space[[nom]])
		}
		return(p)
	}
)

setMethod(
	f = "survive",
	signature = signature(p = "population"),
	definition = function(p) {
		noms <- stage_names(p)
		### Parallelize:
		for ( nom in noms ) {
			p@space[[nom]] <- survive(p@space[[nom]])
		}
		return(p)
	}
)

setMethod(
	f = "age",
	signature = signature(p = "population"),
	definition = function(p) {
		noms <- stage_names(p)
		### Parallelize:
		for ( nom in noms ) {
			p@space[[nom]] <- age(p@space[[nom]])
		}
		return(p)
	}
)
