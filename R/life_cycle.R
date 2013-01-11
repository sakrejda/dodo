SEAL = FALSE

life_cycle <- setClass(
	Class = "life_cycle",
	representation = representation(
		graph = "graphNEL",
		stage_data = "data.frame",
		parent_data = "data.frame",
		adjMatrix = "Matrix",
		stages = "environment",
		transformation_order = "data.frame"
	),
	prototype = prototype(
		graph = graphNEL(),
		stage_data = data.frame(),
		parent_data = data.frame(),
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

setInits <- function(stage_name, where = .GlobalEnv) {
	init_method <- setMethod(
		f = "initialize",
		signature = signature(
			.Object = paste(stage_name, "size_distribution", sep='_')
		),
		definition = function(
				.Object, 
				seed_sample = rnorm(100), 
				n_bins = length(seed_sample)/10, 
				limits = c(
					min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
					max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
				),
				bw=as.integer(length(seed_sample)/15)+1
		) {
			.Object <- callNextMethod(.Object, seed_sample,
																n_bins, limits, bw)
	  	return(.Object)
		},
		where = where
	)
	return(init_method)
}
	
setMethod(
	f = "initialize",
	signature = signature(
		.Object = "life_cycle"
	),
	definition = function(
		.Object,
		stages = NULL,
		parents = NULL
	) {
		setPackageName(pkg = "dodo", env = .Object@stages)
		### short-circuit if stages/parents missing:
		if (is.null(stages) || is.null(parents)) return(.Object)  
		.Object@stage_data = stages
		.Object@parent_data = parents
		.Object@graph <- graphNEL(
			nodes=stages[['stage_name']], 
			edgemode = "directed")
		for ( i in 1:nrow(parents) ) {
			.Object@graph <- addEdge(
				from = parents[i,'parent_stage'], 
				to = parents[i, 'stage_name'], 
				graph = .Object@graph)
		}
		.Object@adjMatrix <- Matrix(data=as(.Object@graph,Class='graphAM')@adjMat)
		classes <- mapply(
			FUN = staged_size_distribution,
				stage_name = stages[['stage_name']],
				stage = 1:nrow(stages),
				age = stages[['age_in_months']],
			MoreArgs = list(where = .Object@stages)
		)
		inits <- sapply(
			X = stages[['stage_name']],
			FUN = setInits,
			where = .Object@stages
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
		current_nodeData <- names(.Object@graph@nodeData)
		if (is.null(current_nodeData) || !(node %in% current_nodeData)) {
			nodeDataDefaults(
				self = .Object@graph,
				attr = paste('F', type, sep='_')
			) <- function() {return(NULL)}
		}
		nodeData(
			self = .Object@graph,
			n = node,
			attr = paste('F', type, sep = '_')
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
			) <- function() {return(NULL)}
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
