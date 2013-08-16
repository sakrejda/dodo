
population <- setRefClass(
	Class = "population",
	fields = list(
		life_cycle = "life_cycle",
		env = "list",
		projection = "block_projection",
		distribution = "staged_block_distribution",
		the_eigens = "list"
	),
	methods = list(
		initialize = function(											### CONSTRUCTOR
			stages = NULL,
			bins = NULL,
			minima = NULL,
			maxima = NULL,
			projections = NULL,
			traits = NULL
		) {
			if (is.null(projections)) projections <- ''
			life_cycle <<- new('life_cycle', 
												 stages=stages, allowed_projections=projections)
			for ( stage in stages) {
				env[[stage]] <<- new.env()
			}
			projection <<- new('block_projection',
												 stages=stages, bins = bins)
			distribution <<- new('staged_block_distribution',
													stages=stages, n_bins=bins, 
													minima=minima, maxima=maxima,
													traits=traits)
			return(.self)
		},
		add_transition = function(from=NULL, to=NULL, projections=NULL) {
			.self$life_cycle$add_transition(
				from=from, to=to, projections=projections)
		},
		step = function() {							
			make_matrix()
			distribution <<- distribution %*% projection
		},
		make_matrix = function() {							
			ft <- life_cycle$get_transitions()
			for ( i in 1:nrow(ft)) {
				projection[ft[i,'from'],ft[i,'to']] <<-
					life_cycle$get_matrix(
						from = ft[i,'from'], to = ft[i,'to'], 
						covariates = as.list(env[[ ft[i,'from'] ]]), 
						distribution = distribution) 
			}
		},
		get_eigens = function() {
			make_matrix()
			the_eigens <<- eigen(projection)
		},
		set_env = function(e) {
			if (is.environment(e)) {
				o <- replicate(n=length(env), expr=e)
				names(o) <- names(env)
				env <<- o
				return(env)
			} else if (is.list(e)) {
				## Single element list:
				if (!any(sapply(e,is.list))) {
					o <- replicate(n=length(env), expr=list.2.environment(e))
					names(o) <- names(env)
					env <<- o
					return(env)
				}
				all_elements <- !any(is.na(match(x=names(e), table=names(env))))
				if (!all_elements) { 
					stop("Some elements of 'env' not defined in argument 'e'.")
				}
				all_environments <- all(sapply(e,is.environment))
				all_lists <- all(sapply(e,is.list))
				if (!all_environments && !all_lists) {
					stop("Not all elements of argument 'e' are environments.")
				}
				## List of environments:
				if (all_environments) {
					env <<- e
					return(env)
				}
				## List of lists:
				if (all_lists) {
					e <- lapply(e,list.2.environment)
					env <<- e
					return(env)
				}
			}
		},
		run = function(e = new.env(), n = 1, o = NULL) {
			if (is.data.frame(e)) 
				e <- lapply(1:nrow(e),function(i) as.list(e[i,]))
			## What to do with output:
			if (!is.null(o)) o_type <- file.info(o)

			## OMG R I love your type system!
			is_env <- is.environment(e)
			is_list <- is.list(e) && !is.list(e[[1]])
			if (!is_env && !is_list) {
				env_list <- all(sapply(e,is.environment))
				list_list <- all(sapply(e,is.list))
			} else { env_list <- FALSE; list_list <- FALSE }

			if (is_env || is_list) {
				set_env(e)
				for ( i in 1:n ) {
					if (!is.null(o) && o_type$isdir) output(path=o, iteration=i)
					step()
				}
				if (!is.null(o)) save(path=o)
			}

			if (env_list || list_list) {
				for ( i in 1:n ) {
					set_env(e[[i]])
					if (!is.null(o) && o_type$isdir) output(path=o, iteration=i)
					step()
				}
				if (!is.null(o)) save(path=o)
			}
		},
		output = function(path=NULL, iteration=NULL) {
			projection$output(path=path, iteration=iteration)
			distribution$output(path=path, iteration=iteration)
		}
	)
)

################################################################################
## Binary operators have to be outside of the class (as usual)
################################################################################

#setMethod(
#	f = "+",
#	signature = signature(e1 = "population", e2 = "size_distribution"),
#	definition = function(e1, e2) {
#		e1 <- e1$copy(shallow=FALSE)
#		e2 <- population$new(life_cycle = e1$life_cycle, sub_pops = list(e2))
#		e1$immigrate(e2)
#		return(e1)
#	}
#)
#
#setMethod(
#	f = "+",
#	signature = signature(e1 = "size_distribution", e2 = "population"),
#	definition = function(e1, e2) e2 + e1
#)
#
#

