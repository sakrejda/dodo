population <- setRefClass(
	Class = "population",
	fields = list(
		life_cycle = "life_cycle",
		sub_pops = "list"
	),
	methods = list(
		initialize = function(											### CONSTRUCTOR
			stages = NULL, parents = NULL,
			life_cycle = NULL, sub_pops = NULL
		) {
			if (!is.null(stages) && !is.null(parents) && is.null(life_cycle)) {
				life_cycle <<- new('life_cycle', stages = stages, parents = parents) 
			} else {
				if (is.null(stages) && is.null(parents) && !is.null(life_cycle)) {
					life_cycle <<- life_cycle
				} else {
					msg <- "Life cycle must be specified as parents/stages or object."
					stop(msg)
				}
			}

			if (is.null(sub_pops)) {
				sub_pops <<- list()
			} else {
				sub_pops <<- sub_pops
			}
			return(.self)
		},
		immigrate = function(population) {						### Have another pop immigrate to this one.
			if (identical(.self$life_cycle, population$life_cycle)) {
				sub_pops <<- c(.self$sub_pops,population$sub_pops)
			} else {
				stop("Populations are not compatible.")
			}
		},
		stage_names = function() {										### Accessor for stage names in this population.
			return(stage_names(.self$life_cycle))
		},
		grow = function(population) {									### Call the models, under 'grow' attribute which shift stage distribution.
			### Check implementation in life_cycle first, for adding models...	
		}
	)
)

################################################################################
## Binary operators have to be outside of the class (as usual)
################################################################################

setMethod(
	f = "+",
	signature = signature(e1 = "population", e2 = "size_distribution"),
	definition = function(e1, e2) {
		e2 <- population$new(life_cycle = e1$life_cycle, sub_pops = list(e2))
		e1$immigrate(e2)
		return(e1)
	}
)

setMethod(
	f = "+",
	signature = signature(e1 = "size_distribution", e2 = "population"),
	definition = function(e1, e2) e2 + e1
)



