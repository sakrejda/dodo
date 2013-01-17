setOldClass(Classes="family", prototype=gaussian())

setRefClass(Class = "pGLM",
	fields = list(
		samp = "logical",
		formula = "formula",
		family = "family",
		coefficients = "matrix",
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
			coefficients = list(intercept=0),
			epsilon = function() {0},
			samp = FALSE
		) {
			formula 			<<- formula
			family 				<<- family

			nc <- length(coefficients)
			coef_tmp <- matrix(unlist(coefficients), nrow=nc, byrow=TRUE)
			rownames(coef_tmp) <- names(coefficients)
			coefficients 	<<- coef_tmp

			epsilon 			<<- epsilon
			samp 					<<- samp
			return(.self)
		},
		model_matrix = function(
			new_data = NULL, 
			covariates = NULL,
			n = NULL 
		) {
			if (is.null(new_data))   got_data <- FALSE else got_data <- TRUE
			if (is.null(covariates)) got_cov  <- FALSE else got_cov  <- TRUE
			if (is.null(n))					 got_n    <- FALSE else got_n    <- TRUE

			if ( got_data && got_cov) stop("Call model matrix with data OR covariates")
			if ( got_data) 								n <- nrow(new_data)
			if (!got_data && !got_n ) 		n <- 1
			if (!got_data) 				 new_data <- data.frame(row = 1:n)

			if (!got_data && got_cov) {
		    for ( nom in names(covariates) ) {
    		  ### Relies on recycling to get the right number of entries:
		      new_data[[nom]] <- covariates[[nom]]
		    }
			} 
			mf <<- model.frame(formula = formula, data = new_data)
			mm <<- model.matrix(object = formula, data = mf)
		},
		predict = function(
			newdata = NULL, 
			covariates = NULL,
			n = NULL, 
			...
		) {
			if (!is.null(covariates)) covariates <<- covariates
			ml <- as.list(match.call())
			draw <- 1
			if (samp) {
				draw <- sample(x=1:ncol(.self$coefficients), size=1)
			}
			model_matrix(newdata, covariates, n)
			if (ncol(.self$mm) == length(.self$coefficients[,draw])) {
				pred <- .self$mm %*% coefficients[,draw] + epsilon(n=nrow(.self$mm))
				pred <- family$linkinv(pred)
				### Add inverse link function using $family field.
			} else {
				stop("Object not ready for predictions, model matrix not formed.\n\n")
			}
			return(pred)
		},
		errors = function(n = 100) epsilon(n=n)
	)
)
