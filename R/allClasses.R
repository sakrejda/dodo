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
	staged_size_distribution <- setClass(
		Class = paste(stage_name, "size_distribution", sep='_'),
		contains = "size_distribution",
		representation = representation(
			stage = "numeric",
			stage_name = "character",
			month = "numeric"
		),
		prototype = prototype(
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
	return(staged_size_distribution)	
}

###########################################################################
###########################################################################

#setOldClass("data.frame")

#popList <- setClass(
#	Class = "popList",
#	contains = "list",
#	representation = representation(
#		stage_name = "character"
#	),
#	prototype = prototype(
#		stage_name = "A"
#	)
#)

population <- setClass(
	Class = "population",
	representation = representation(
		life_cycle = "life_cycle",
		space = "list",
		promoted = "list"
	),
	prototype = prototype(
		life_cycle = new('life_cycle'),
		space = list(),
		promoted = list()
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

