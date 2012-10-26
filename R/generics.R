
setGeneric(name="get_N", 
					 def=function(.Object) standardGeneric("get_N"), 
					 valueClass = "numeric")
setGeneric(name="get_K", 
					 def=function(.Object) standardGeneric("get_K"), 
					 valueClass = "numeric")
setGeneric(name="get_recaptures", 
					 def=function(.Object, id) standardGeneric("get_recaptures"),
					 valueClass = "numeric")
setGeneric(name="get_recaptures_matrix",
					 def=function(.Object) standardGeneric("get_recaptures_matrix"),
					 valueClass = "matrix")



