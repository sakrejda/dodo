setClass("pGLM")


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
	f = "add_lc_node_model",
	signature = signature(
		.Object = "life_cycle",
		node = "character",
		type = "character",
		model = "pGLM"
	),
	definition = function(.Object, node, type, model) {
		if (!(node %in% names(.Object@graph@nodeData))) {
			.Object@graph <- nodeDataDefaults(
				self = .Object@graph,
				attr = paste('pGLM', type, sep='_')
			) <- new('pGLM')
		}
		nodeData(
			self = .Object@graph,
			n = n,
			attr = paste('pGLM', type, sep = '_')
		) <- model
		return(.Object)
	}
)

setMethod(
	f = "add_lc_transition_model",
	signature = signature(
		.Object = "life_cycle",
		from = "character",
		to = "character",
		type = "character",
		model = "pGLM"
	),
	definition = function(.Object, from, to, type, model) {
		edge_name <- paste("from","to",sep="|")
		if (!(edge_name %in% names(.Object@graph@edgeData))) {
			.Object@graph <- edgeDataDefaults(
				self = .Object@graph,
				attr = paste('pGLM', type, sep='_')
			) <- new('pGLM')
		}
		edgeData(
			self = .Object@graph,
			from = from,
			to = to,
			attr = paste('pGLM', type, sep = '_')
		) <- model
		return(.Object)
	}
)
