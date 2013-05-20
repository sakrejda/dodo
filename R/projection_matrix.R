###########################################################################
## Blocked Projection matrices:
###########################################################################

block_projection <- setRefClass(
  Class = "block_projection",
  fields = list(
    A = "Matrix",
    stage_names = "character",
    j = "numeric",
    n_bins = "numeric",
    start_index = "numeric",
    stop_index = "numeric"
  ),
  methods = list(
    initialize = function(
      stages = "A",
      bins = 1
    ) {
      if (length(stages) != length(bins)) {
        stop("Please provide the number of bins for each stage.")
      }
			stage_names <<- stages
			n_bins <<- bins
      A <<- Matrix(data=0, nrow=sum(bins), ncol=sum(bins), sparse=TRUE)
      n_bins <<- bins
      j <<- 1:length(stages)
      start_index <<- sapply(
        X = j,
        FUN = function(j, bins) {
          if (j == 1) return(1) else {
          return(sum(n_bins[1:j-1]) + 1)
          }
        }
      )
      stop_index <<- start_index + (n_bins[j] - 1)

      return(.self)
    },
		write = function(x=NULL, from=NULL, to=NULL) {
			if (is.null(x)) stop("Must specify what to write.")
			if (is.null(from)) stop("Must specify stage name.")
			if (is.null(to)) to <- from

			cols <- (start_index[stage_names == from]):(stop_index[stage_names == from])
			rows <- (start_index[stage_names == to]):(stop_index[stage_names == to])
			A[rows,cols] <<- x
			return(A[rows,cols,drop=FALSE])	
		},
		read = function(from=NULL, to=NULL) {
			if (is.null(from)) stop("Must specify stage name.")
			if (is.null(to)) to <- from

			cols <- (start_index[stage_names == from]):(stop_index[stage_names == from])
			rows <- (start_index[stage_names == to]):(stop_index[stage_names == to])
			return(A[rows,cols,drop=FALSE])	
		}
  )
)



setMethod(  
  f="[", 
  signature = signature(x='block_projection', i='character',j='character'), 
  definition = function(x, i, j) {
    return(x$read(from=i, to=j))
  }
)

setMethod(  
  f="[<-",
  signature = signature(x='block_projection', i='character',j='character'), 
  definition = function(x, i, j, value) {
    x$write(x=value, from=i, to=j)
    return(x)
  }
)


