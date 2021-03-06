
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
			padding = NULL,
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
		get_transitions(from=NULL, to=NULL) {
			reg <- do.call(what=rbind, args=pop$life_cycle$transition_register)
			names(reg) <- c('from','to')

			if ( is.null(from) &&  is.null(to)) {
				return(reg)
			} else if (!is.null(from) && !is.null(to)) {
				return(reg[reg[['from']] == from & reg[['to']] == to,])
			} else if (!is.null(from) &&  is.null(to)) {
				return(reg[reg[['from']] == from,])
			} else if ( is.null(from) && !is.null(to)) {
				return(reg[                        reg[['to']] == to,])
			} else {
				stop("Nonsensical 'from/to' args.")
			}
		}
		step = function() {							
			make_matrix()
			distribution <<- distribution %*% projection
		},
		make_matrix = function() {							
			ft <- life_cycle$get_transitions()
			for ( i in 1:nrow(ft)) {
				lhs_dim <- dim(projection[ft[i,'from'],ft[i,'to']])
				rhs <- life_cycle$get_matrix(
						from = ft[i,'from'], to = ft[i,'to'], 
						covariates = as.list(env[[ ft[i,'from'] ]]), 
						distribution = distribution) 
				if (all(lhs_dim == dim(rhs))) {
					projection[ft[i,'from'],ft[i,'to']] <<- rhs
				} else {
					msg <- paste(
						"Dimensions of projection from '", 
						ft[i,'from'], "' to '", ft[i,'to'], "'",
						"\n\tshould be ", lhs_dim[1], " row(s), and ", lhs_dim[2], " column(s).",
						"\n\tNow they are ", dim(rhs)[1], " row(s), and ", dim(rhs)[2], " column(s).",
						"\n\tPlease check your models for this transition.", 
						sep=''
					)
					stop(msg)
				}
			}
		},
		get_eigens = function() {
			make_matrix()
			the_eigens <<- eigen(projection)
		},
		set_env = function(e) {
			if (is.list(e)) {
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
			} else {
				stop("Argument 'e' must be a list of environments.")
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

