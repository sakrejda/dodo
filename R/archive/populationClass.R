population <- setRefClass(
	Class = "population",
	fields = list(
		life_cycle = "life_cycle",
		env = "list",
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

			for ( stage in stage_names(.self$life_cycle) ) {
				env[[stage]] <<- new.env()
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
		survive = function() {								
			sub_pops <<- mclapply(
				X = sub_pops, 
				FUN = survive, 
					model = life_cycle,
					covariates = env
			)
		},
		grow = function() {							
			env <<- mcmapply(
				FUN = function(x,y) {
					y$sizes <- x@sizes   ### Cache sizes for use by other functions.
					return(y)
				},
				x = sub_pops,
				y = env
			)
			sub_pops <<- mclapply(
				X = sub_pops, 
				FUN = grow, 
					model = life_cycle,
					covariates = env
			)
		},
		smolt = function() {   #### CAREFUL: this will split a single stage
													 ####			     into two, it will be important
													 ####					 in recombining them to avoid
													 ####   			 creating nested lists.
			sub_pops <<- unlist(mclapply(
				X = sub_pops, 
				FUN = smolt, 
					model = life_cycle,
					covariates = env
			))
		},
		age = function() {		#### CAREFUL: this will change the identity of
													####					each stage... should be ok...
			sub_pops <<- mclapply(
				X = sub_pops, 
				FUN = age, 
					model = life_cycle,
					covariates = env
			)
		},
		sync = function() {
			known_stages <- stage_names(.self$life_cycle)
			present_stages <- sapply(X=sub_pops, FUN=function(x) {x@stage_name})
			known_stages <- known_stages[known_stages %in% present_stages]
			o <- list()
			for ( stage in known_stages ) {
				o <- c(o,do.call(what=pool, args=sub_pops[ present_stages %in% stage ]))
			}
			sub_pops <<- o
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



