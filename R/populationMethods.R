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
			model <- get_model(p = p, stage = nom, type = "grow")
			p <- grow(p = p, stage = nom, model = model)
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
			model <- get_model(p = p, stage = nom, type = "smolt")
			p <- smolt(p = p, stage = nom, model = model)
		}
		return(p)
	}
)

setMethod(
	f = "survive",
	signature = signature(p = "population"),
	definition = function(p) {
		edge_noms <- edges(p@life_cycle@graph)
		### Parallelize:
		for ( from in names(edges_noms) ) {
			for ( to in edge_noms[[from]] ) {
				model <- get_model(p, from, to, "survive")
				p <- survive(p = p, from = from, to = to, model = model)
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

setMethod(
	f = "add_lc_model",
	signature = signature(
		.Object = "population",
		from = "character",
		to = "character",
		type = "character",
		model = "lc_lm"
	),
	definition = function(.Object, from, to, type, model) {
		.Object@life_cycle <- add_lc_model(
			.Object@life_cycle, from, to, type, model)
		return(.Object)
	}
)
