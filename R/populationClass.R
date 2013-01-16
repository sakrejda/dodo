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
			transformations = NULL,
			life_cycle = NULL, sub_pops = NULL, ...
		) {
			if (is.null(stages) && is.null(parents) && 
					is.null(life_cycle) && is.null(sub_pops)) return(.self)

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

			if (!is.null(transformations)) {
				life_cycle <<- add_transformations(.self$life_cycle, transformations)
			}

			if (is.null(sub_pops)) {
				sub_pops <<- list()
			} else {
				sub_pops <<- sub_pops
			}

			for ( stage in stage_names(.self$life_cycle) ) {
				env[[stage]] <<- new.env()
			}
			callSuper()
			return(.self)
		},
		immigrate = function(population) {						### Have another pop immigrate to this one.
			if (identical(.self$life_cycle, population$life_cycle)) {
				sub_pops <<- c(.self$sub_pops,population$sub_pops)
			} else {
				stop("Populations are not compatible.")
			}
		},
		add_model = function(stage, transformation, model) {
			life_cycle <<- add_lc_node_model(life_cycle, stage, transformation, model)
		},
		transform = function(node, model, sub_pop, env) {
			## ONLY using node models, b/c the "model" string
			## is related to whether we get back a single sub_pop,
			## or a list of them to replace the original.  Then
			## we have to conditionally unlist to get the flat list
			## again.  from/to models not needed.
			f <- get_lc_node_model(.self$life_cycle, node, model)

			## A little awkward because there might be multiple
			## instances of a particular stage in the sub_pops list,
			## but they all use the same environment (if not, they
			## ought to be a separate stage).  Will find out what
			## sort of overhead this introduces, only relevant if the
			## "env" list gets BIG:
			return(f(sub_pop, env[[node]]))
		},
		step = function() {								
#     THIS WAS WRONG:
#			env <<- mcmapply(
#				FUN = function(x,y) {
#					y$sizes <- x@sizes   ### Cache sizes for use by transformations
#					return(y)
#				},
#				x = sub_pops,
#				y = env
#			)
#			In an IPM, the densities change, but the transformation is
			#			calculated across all coordinates (midpoints) so that
			#     doesn't matter!
			trans <- get_transformations(.self$life_cycle)
			for (i in 1:nrow(trans)) {
				sub_pops <<- mcmapply(
					FUN = .self$transform,
					node = sapply(X=sub_pops, FUN=function(x) {x@stage_name}),
					sub_pop = sub_pops,
					MoreArgs = list(
						model = trans[['model']][i],
						env = env
					)
				)
				if (any(lapply(sub_pops,length)>1)) 
					sub_pops <<- unlist(sub_pops, recursive=FALSE, use.names=FALSE)
			}
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
		e1 <- e1$copy(shallow=FALSE)
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



