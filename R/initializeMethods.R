setMethod(
	f = "initialize",
	signature = signature(
		.Object = "size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object@sizes = vector(mode="numeric", length=n_bins)
		.Object@n_bins = n_bins
		.Object@limits = limits
		h <- (limits[['max']] - limits[['min']]) / n_bins
		.Object@midpoints = limits[['min']] + ((1:n_bins)-0.5) * h
		estimator <- function(x, seed, bw) {
			1/(length(seed)*bw) * sum(dnorm(x=x, mean=seed, sd=1))
		}
		.Object@sizes = sapply(
			X=.Object@midpoints, 
			FUN=estimator,
			seed=seed_sample, bw=bw
		)
		.Object@sizes <- length(seed_sample) * (.Object@sizes/sum(.Object@sizes))
  	return(.Object)
	}
)

###########################################################################
## Initializer FACTORY function.
###########################################################################

setInits <- function(stage_name, where = .GlobalEnv) {
	init_method <- setMethod(
		f = "initialize",
		signature = signature(
			.Object = paste(stage_name, "size_distribution", sep='_')
		),
		definition = function(
				.Object, 
				seed_sample = rnorm(100), 
				n_bins = length(seed_sample)/10, 
				limits = c(
					min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
					max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
				),
				bw=as.integer(length(seed_sample)/15)+1
		) {
			.Object <- callNextMethod(.Object, seed_sample,
																n_bins, limits, bw)
	  	return(.Object)
		},
		where = where
	)
	return(init_method)
}
###########################################################################
###########################################################################
	
setMethod(
	f = "initialize",
	signature = signature(
		.Object = "life_cycle"
	),
	definition = function(
		.Object,
		stages = NULL,
		parents = NULL
	) {
		setPackageName(pkg = "dodo", env = .Object@stages)
		### short-circuit if stages/parents missing:
		if (is.null(stages) || is.null(parents)) return(.Object)  
		.Object@stage_data = stages
		.Object@parent_data = parents
		.Object@graph <- graphNEL(
			nodes=stages[['stage_name']], 
			edgemode = "directed")
		for ( i in 1:nrow(parents) ) {
			.Object@graph <- addEdge(
				from = parents[i,'parent_stage'], 
				to = parents[i, 'stage_name'], 
				graph = .Object@graph)
		}
		.Object@adjMatrix <- Matrix(data=as(.Object@graph,Class='graphAM')@adjMat)
		classes <- mapply(
			FUN = staged_size_distribution,
				stage_name = stages[['stage_name']],
				stage = 1:nrow(stages),
				age = stages[['age_in_months']],
			MoreArgs = list(where = .Object@stages)
		)
		inits <- sapply(
			X = stages[['stage_name']],
			FUN = setInits,
			where = .Object@stages
		)

		return(.Object)
	}
)

