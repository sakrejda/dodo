SEAL = FALSE

life_cycle <- setClass(
	Class = "life_cycle",
	representation = representation(
		graph = "graphNEL",
		stage_data = "list",
		adjMatrix = "Matrix",
		stages = "environment",
		transformation_order = "data.frame"
	),
	prototype = prototype(
		graph = graphNEL(),
		stage_data = list(),
		adjMatrix = Matrix(),
		stages = new.env(),
		transformation_order = data.frame()
	),
	validity = function(object) {
		# Write fail conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

	
###########################################################################
## Factory functions:
###########################################################################

	
setMethod(
	f = "initialize",
	signature = signature(
		.Object = "life_cycle"
	),
	definition = function(
		.Object,
		stages = NULL
	) {
		setPackageName(pkg = "dodo", env = .Object@stages)
		### short-circuit if stages/parents missing:
		if (is.null(stages)) return(.Object)  
		.Object@stage_data = stages
		.Object@graph <- graphNEL(
			nodes=stages[['stage_name']], 
			edgemode = "directed")
		.Object@adjMatrix <- Matrix(data=as(.Object@graph,Class='graphAM')@adjMat)

		## Massage stage data for mapply:
		lengths <- sapply(stages, length)
		one_lengths <- lengths == 1
		full_lenghts <- lengths == max(lengths)
		if (sum(one_lengths | full_lengths) != length(stages)) {
			stop("All elements of 'stages' argument must either be length 1 or specified per-stage.")
		}
		one_lengths_names <- names(stages)[one_lengths]
		full_lengths_names <- names(stages)[full_lengths]
		stages <- stages[full_lengths_names]
		for (nom in one_lengths_names) {
			if (is.function(stages[[nom]])) {
				stages[[nom]] <- lapply(1:max(lengths), function(x) return(stages[[nom]]))
			} else {
				stages[[nom]] <- as.list(rep(stages[[nom]], max(lengths)))
			}
		}
		traits_ <- list()
		for ( i in 1:max(lengths) ) {
			traits_[[i]] <- lapply(stages, `[`, i=i)
		}

		## Create classes:
		classes <- mapply(
			FUN = staged_size_distribution,
				stage_name = stages[['stage_name']],
				stage = 1:nrow(stages),
				traits = (stages),
			MoreArgs = list(where = .Object@stages)
		)
		return(.Object)
	}
)

###########################################################################
###########################################################################

setMethod(
	f = "stage_names",
	signature = signature(
		.Object = "life_cycle"
	),
	definition = function(.Object) {
		return(.Object@stage_data[['stage_name']])
	}
)

setMethod(
	f = "add_transformations",
	signature = signature(.Object = "life_cycle", data = "data.frame", file = "missing"),
	definition = function(.Object, data) {
		trans <- data[order(data[['order']]), c('order','model')]
		.Object@transformation_order = trans
		return(.Object)
	}
)

setMethod(
	f = "add_transformations",
	signature = signature(.Object = "life_cycle", data = "missing", file = "character"),
	definition = function(.Object, file) {
		info <- file.info(file)
		if (is.na(info$size)) stop(paste("File/path at '", file, "' does not exist."))
		if (info$isdir) stop(paste("path '", file, "' points to a directory."))
		.Object <- add_transformations(.Object, read.table(file))
		return(.Object)
	}
)

setMethod(
	f = "get_transformations",
	signature = signature(.Object="life_cycle", stage="missing", type="missing"),
	definition = function(.Object) {
		return(.Object@transformation_order)
	}
)

###########################################################################
###########################################################################

setMethod(
	f = "add_lc_node_model",
	signature = signature(
		.Object = "life_cycle",
		node = "character",
		type = "character",
		model = "function"
	),
	definition = function(.Object, node, type, model) {
		current_nodes <- names(.Object@graph@nodeData@data)
		current_types <- names(.Object@graph@nodeData@defaults)
		attr <- paste('F', type, sep='_')
		if (is.null(current_nodes) || !(node %in% current_nodes ) ||
				is.null(current_types) || !(attr %in% current_types)) {
			nodeDataDefaults(
				self = .Object@graph,
				attr = attr 
			) <- FALSE
		}
		nodeData(
			self = .Object@graph,
			n = node,
			attr = attr
		) <- model
		return(.Object)
	}
)

setMethod(
	f = "get_lc_node_model",
	signature = signature(
		.Object = "life_cycle",
		node = "character",
		type = "character"
	),
	definition = function(.Object, node, type) {
		a <- paste('F', type, sep='_')
		o <- nodeData(self=.Object@graph, n=node, attr=a)[[node]]
		return(o)
	}
)


setMethod(
	f = "add_lc_transition_model",
	signature = signature(
		.Object = "life_cycle",
		from = "character",
		to = "character",
		type = "character",
		model = "function"
	),
	definition = function(.Object, from, to, type, model) {
		edge_name <- paste("from","to",sep="|")
		if (!(edge_name %in% names(.Object@graph@edgeData))) {
			edgeDataDefaults(
				self = .Object@graph,
				attr = paste('F', type, sep='_')
			) <- FALSE
		}
		edgeData(
			self = .Object@graph,
			from = from,
			to = to,
			attr = paste('F', type, sep = '_')
		) <- model
		return(.Object)
	}
)

setMethod(
	f = "get_lc_transition_model",
	signature = signature(
		.Object = "life_cycle",
		from = "character",
		to = "character",
		type = "character"
	),
	definition = function(.Object, from, to, type) {
		e <- paste(from,to,sep='|')
		o <- edgeData( self = .Object@graph, 
			from = from,
			to = to,
			attr = paste('F', type, sep='_'))[[e]]
		return(o)
	}
)
