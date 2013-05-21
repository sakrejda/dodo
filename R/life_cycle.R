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

		},
		add_transition = function(from=NULL, to=NULL, projections=NULL) {
			if (any(!(names(projections) %in% projections_allowed))) {
				msg <- paste("Only the following projections are allowed:\n",
										 paste(projections_allowed, collapse="\n"), sep='')
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
		},
		reducer = function(x) {
		  if (length(x) == 1)
		    return(x)
		  else if (length(x) == 2)
		    return (x[[1]] %*% x[[2]])
		  else {
		    x[[2]] <- x[[1]] %*% x[[2]]
		    return(f(x[2:length(x)]))
		  }
		},
		get_matrix = function(.Object, from, to, covariates) {
			transitions[[from]][[to]][['matrices']] <<- lapply(
				X = transitions[[from]][[to]][['projections']],
				FUN = function(f, obj, stage, covariates) f(obj, stage, covariates)
			)
			return(reducer(transitions[[from]][[to]][['matrices']]))
		}
	)
)




