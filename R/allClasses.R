SEAL = FALSE

size_distribution <- setClass(
	Class = "size_distribution",
	representation = representation(
		sizes = "numeric",
		n_bins = "numeric",
		limits = "numeric",
		midpoints = "numeric"
	),
	prototype = prototype(
		sizes = vector(mode="numeric", length=0),
		n_bins = 0,
		limits = vector(mode="numeric", length=2),
		midpoints = vector(mode="numeric", length=0)
	),
	validity = function(object) {
		# Write fail conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

life_cycle <- setClass(
	Class = "life_cycle",
	representation = representation(
		graph = "graphNEL",
		stage_data = "data.frame",
		parent_data = "data.frame",
		adjMatrix = "Matrix",
		stages = "environment"
	),
	prototype = prototype(
		graph = graphNEL(),
		stage_data = data.frame(),
		parent_data = data.frame(),
		adjMatrix = Matrix(),
		stages = new.env()
	),
	validity = function(object) {
		# Write fail conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

	
###########################################################################
## Class FACTORY function
###########################################################################

staged_size_distribution <- function(
	stage_name,
	stage, 
	age,
	where = .GlobalEnv
) { 
	### This factory sets the derived class, sets its init method, then
	### defines the reference class with its initialize method (which
	### just passes on the call.

	staged_size_distribution <- setClass(
		Class = paste(stage_name, "size_distribution", sep='_'),
		contains = "size_distribution",
		representation = representation(
			id = "character",
			stage = "numeric",
			stage_name = "character",
			month = "numeric"
		),
		prototype = prototype(
			id = "",
			stage = stage,
			stage_name = stage_name,
			age = age
		),
		validity = function(object) {
			# Write fails conditions which return(FALSE)
			return(TRUE)
		},
		where = where,
		sealed = SEAL
	)

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
			.Object <- callNextMethod()
	  	return(.Object)
		},
		where = where
	)
	return(staged_size_distribution)	
}

###########################################################################
###########################################################################

