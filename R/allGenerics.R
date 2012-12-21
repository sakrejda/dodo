
# Examples...
#setGeneric(name="get_N", 
#					 def=function(.Object) standardGeneric("get_N"), 
#					 valueClass = "numeric")
#setGeneric(name="get_recaptures", 
#					 def=function(.Object, id) standardGeneric("get_recaptures"),
#					 valueClass = "numeric")
#setGeneric(name="set_deaths",
#					 def=function(.Object, id, times_of_deaths) standardGeneric("set_deaths"),
#					 valueClass = "numeric")
#setGeneric(name="get_sampled",
#					 def=function(.Object) standardGeneric("get_sampled"),
#					 valueClass = "logical")
#
setGeneric(name="make_life_cycle",
					 def=function(stages, parents, ...)
						 standardGeneric("make_life_cycle"),
					 	valueClass = "life_cycle")
setGeneric(name="survive",
					 def=function(.Object, model, covariates)
						 standardGeneric("survive"),
						valueClass = "size_distribution")
setGeneric(name="grow",
					 def=function(.Object, model, covariates)
						 standardGeneric("grow"),
						valueClass = "size_distribution")
setGeneric(name="stage_names",
					 def=function(.Object)
						 standardGeneric("stage_names"),
						 valueClass = "character")
