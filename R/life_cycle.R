################################################################################
### Class which holds the information on how to fill up the projection
### matrix based on some linear models, and allows you to add linear
### models.
################################################################################

life_cycle <- setRefClass(
	Class = 'life_cycle',
	fields = list(
		stage_names = "character",
		j = "numeric",
		transitions = "list",
		transition_register = "list",
		projections_allowed = "character"
	),
	methods = list(
		initialize = function(stages = '', allowed_projections='' ) {
			stage_names <<- stages
			j <<- 1:length(stages)
			projections_allowed <<- allowed_projections
			transitions <<- sapply(
				X = stage_names,
				FUN = function(x) {return(list())}, USE.NAMES=TRUE)
			transition_register <<- list()
		},
		add_transition = function(from=NULL, to=NULL, projections=NULL) {
			if (any(!(names(projections) %in% projections_allowed))) {
				msg <- paste("Only the following projections are allowed:\n",
										 paste(projections_allowed, collapse="\n"), sep='')
				stop(msg)
			}
			if (!(from %in% stage_names)) {
				msg <- paste("Stage ", from, " unknown.  All stages must be",
										 " defined at object initialization.\n", sep='')
				stop(msg)
			}
			if (!(to %in% stage_names)) {
				msg <- paste("Stage ", to, " unknown.  All stages must be",
										 " defined at object initialization.\n", sep='')
				stop(msg)
			}
			f <- function(p) all(formalArgs(p) %in% c('.Object', 'stage', 'covariates')) 
			test <- sapply(X=projections, FUN = f)
			if (!all(test) || (length(all(test)) != 1)) {
				msg <- paste(
					"Each projection function must have the arguments: ", 
					"'.Object', 'stage', and 'covariates'\n",
					"Problem with: \n",
					paste( names(projections)[which(!test)], collapse="\n")
				)
				stop(msg)
			}
			transitions[[from]][[to]][['projections']] <<- projections
			transition_register <<- c(transition_register, list(c(from,to)))
		},
		reducer = function(x) {
			lx <- length(x)
		  if (lx == 1)
		    return(x[[1]])
		  else {
		    x[[2]] <- x[[2]] %*% x[[1]]
		    return(reducer(x[2:lx]))
		  }
		},
		get_matrix = function(from, to, covariates, distribution) {
			transitions[[from]][[to]][['matrices']] <<- lapply(
				X = transitions[[from]][[to]][['projections']],
				FUN = function(f, obj, stage, covariates) f(obj, stage, covariates),
				obj = distribution, 
				stage = from,
				covariates = as.list(covariates)
			)
			return(reducer(transitions[[from]][[to]][['matrices']]))
		},
		get_transitions = function() {
			o <- t(sapply(X = transition_register, FUN = as.matrix))
			colnames(o) <- c('from', 'to')
			return(o)
		}
	)
)




