###########################################################################
## Blocked distribution:
###########################################################################

block_distribution <- setRefClass(
  Class = "block_distribution",
  fields = list(
    densities = "Matrix",
		midpoints = "Matrix",
    stage_names = "character",
    j = "numeric",
    n_bins = "numeric",
    start_index = "numeric",
    stop_index = "numeric",
		minima = "numeric",
		maxima = "numeric"
  ),
  methods = list(
    initialize = function(
      stages = "A",
      n_bins = 1,
			minima = 0,
			maxima = 1,
			FUN = NULL,
			...
    ) {
      if (length(unique(c(
				length(n_bins), length(minima),
				length(stages),length(maxima)))) != 1
			) {
				msg <- paste(
					"Arguments must be of equal length. ",
					"Lengths are: \n",
					"\tstages: ", length(stages),
					"\tn_bins: ", length(n_bins),
					"\tminima: ", length(minima),
					"\tmaxima: ", length(maxima), sep='')
				stop(msg)
      }
			if (!all(minima < maxima)) {
				stop("All minima must be less than the respective maxima.")
			}

			stage_names <<- stages
      j <<- 1:length(stages)
      n_bins <<- n_bins
			minima <<- minima
			maxima <<- maxima

      densities <<- Matrix(data=0, nrow=sum(n_bins), ncol=1, sparse=TRUE)
      midpoints <<- Matrix(data=0, nrow=sum(n_bins), ncol=1, sparse=TRUE)
      start_index <<- sapply(
        X = j,
        FUN = function(j, bins) {
          if (j == 1) return(1) else {
          return(sum(n_bins[1:j-1]) + 1)
          }
        }
      )
      stop_index <<- start_index + (n_bins[j] - 1)
			for ( i in j ) {
				h <- (maxima[j] - minima[j]) / n_bins[j]
				midpoints[start_index[j]:stop_index[j],1] <<- 
					minima[j] + ((1:n_bins[j])-0.5) * h
				if (!is.null(FUN[[j]])) {
					d <- sapply(X=midpoints[start_index[j]:stop_index[j],1], FUN=FUN, ...)
					if (is.numeric(d) && is.vector(d)) 
						densities[start_index[j]:stop_index[j],1] <<- d
				}
			}

      return(.self)
    },
		write = function(x=NULL, stage=NULL) {
			if (is.null(x)) stop("Must specify what to write.")
			if (is.null(stage)) stop("Must specify stage name.")

			rows <- (start_index[stage_names == stage]):(stop_index[stage_names == stage])
			densities[rows,1] <<- x
			return(densities[rows,1,drop=FALSE])	
		},
		read = function(stage=NULL) {
			if (is.null(stage)) stop("Must specify stage name.")

			rows <- (start_index[stage_names == stage]):(stop_index[stage_names == stage])
			return(densities[rows,1,drop=FALSE])	
		},
		get_midpoints = function(stage=NULL) {
			if (is.null(stage)) stop("Must specify stage name.")
			rows <- (start_index[stage_names == stage]):(stop_index[stage_names == stage])
			return(midpoints[rows,1,drop=FALSE])	
		}
  )
)



setMethod(  
  f="[", 
  signature = signature(x='block_distribution', i='character',j='character'), 
  definition = function(x, i, j) {
    return(x$read(from=i, to=j))
  }
)

setMethod(  
  f="[<-",
  signature = signature(x='block_distribution', i='character',j='character'), 
  definition = function(x, i, j, value) {
    x$write(x=value, from=i, to=j)
    return(x)
  }
)


###########################################################################
## Staged blocked distribution:
###########################################################################

staged_block_distribution <- setRefClass(
  Class = "staged_block_distribution",
	contains = "block_distribution",
  fields = list(
		traits = "list"
	),
	methods = list(
		initialize = function(traits, ...) {
			traits <<- traits
			callSuper(...)
			return(.self)
		},
		newdata = function(stage, covariates=list()) {
			k <- which(stage == 'stage_names')
			nd <- data.frame(
				row = 1:(n_bins[k]),
				sizes = midpoints[k],
				covariates,
				traits[[k]][!sapply(traits[[k]],is.function)]   ### FIXED traits.
			)

			for (f in traits[[k]][sapply(traits[[k]],is.function)]) {
				nd <- f(nd)
			}   ### FUNCTION value traits
			return(nd)
		}
	)
)

