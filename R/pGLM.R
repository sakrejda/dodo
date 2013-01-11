setOldClass(Classes="family", prototype=gaussian())

setRefClass(Class = "pGLM",
	fields = list(
		formula = "formula",
		family = "family",
		coefficients = "numeric",
		epsilon = "function",
		covariates = "list",
		data = "data.frame",
		model_frame = "data.frame",
		mm = "matrix",
		mf = "data.frame",
		dat = "data.frame"
	),
	methods = list(
		initialize = function(
			formula = ~ 1, 
			family = gaussian(),
			coefficients = list(),
			epsilon = function() {0}
		) {
			formula 			<<- formula
			family 				<<- family
			coefficients 	<<- coefficients
			epsilon 			<<- epsilon
			return(.self)
		},
		model_matrix = function(
			new_data = NULL, 
			covariates = NULL,
			n = NULL 
		) {
			if (!is.null(new_data)) n <- nrow(new_data)
			if (is.null(new_data) && !is.null(covariates) && !is.null(n)) {
		    new_data <- data.frame(row = 1:n)
		    for ( nom in names(covariates) ) {
    		  ### Relies on recycling to get the right number of entries:
		      new_data[[nom]] <- covariates[[nom]]
		    }
			} 
			if (!is.null(new_data) && !is.null(n)) {
				mf <<- model.frame(formula = formula, data = new_data)
			} else { stop("Bad call to model_matrix method.") }
			mm <<- model.matrix(object = formula, data = mf)
		},
		predict = function(
			newdata = NULL, 
			covariates = NULL,
			n = NULL, 
			...
		) {
			ml <- as.list(match.call())
			if (!is.null(newdata) || !is.null(covariates))
				model_matrix(newdata, covariates, n)
			if (ncol(.self$mm) == length(.self$coefficients)) {
				pred <- .self$mm %*% coefficients + epsilon(n=n)
				pred <- family()$linkinv(pred)
				### Add inverse link function using $family field.
			} else {
				stop("Object not ready for predictions, model matrix not formed.\n\n")
			}
			return(pred)
		},
		errors = function(n = 100) epsilon(n=n)
	)
)
