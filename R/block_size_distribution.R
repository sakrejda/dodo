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
		maxima = "numeric",
		summary = function(x=NULL) {
			if (is.null(x)) {
				o <- summary_internal()
				return(o)
			}
		}
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
      stop_index <<- start_index + (n_bins - 1)
			h <- (maxima - minima) / n_bins
			for ( i in j ) {
				midpoints[start_index[i]:stop_index[i],1] <<- 
					minima[i] + ((1:n_bins[i])-0.5) * h[i]
				if (!is.null(FUN[[i]])) {
					d <- sapply(X=midpoints[start_index[i]:stop_index[i],1], FUN=FUN, ...)
					if (is.numeric(d) && is.vector(d)) 
						densities[start_index[i]:stop_index[i],1] <<- d
				}
			}
			o <- summary_internal()
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
		},
		summary_internal = function() {
			summary_data <- data.frame(
				index = j,
				stages = stage_names,
				bins = n_bins,
				start = start_index,
				stop = stop_index,
				minima = minima, 
				maxima = maxima
			)
			stg <- mapply(
				FUN = rep,
				x = stage_names,
				times = n_bins,
				USE.NAMES=FALSE
			)

			density_data <- data.frame(
				midpoint = midpoints[,1],
				density = densities[,1],
				stage = unlist(stg)
			)

			density_plot <- ggplot(
				data = density_data,
				aes(x=midpoint, y=density)
			) + geom_point() + facet_wrap( ~ stage)
			return(list(data = summary_data, density=density_plot))
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
		initialize = function(traits = NULL, ...) {
			if (is.null(traits)) {
				traits <<- sapply(X=list(...)$stages, FUN=function(x) list())
			} else {
				traits <<- traits
			}
			callSuper(...)
			return(.self)
		},
		newdata = function(stage, covariates=list()) {
			k <- which(stage == stage_names)
			nd <- data.frame(
				row = 1:(n_bins[k]),
				sizes = midpoints[start_index[k]:stop_index[k]]
			)
			if (length(covariates) != 0) {
				nd <- data.frame(nd,covariates)
			}
			if (length(sapply(traits[[k]],is.function)) != 0) {
				trFixed <- traits[[k]][!sapply(traits[[k]],is.function)]   ### FIXED traits.
				nd <- data.frame(nd,trFixed)

				for (f in traits[[k]][sapply(traits[[k]],is.function)]) {
					nd <- f(nd)
				}   ### FUNCTION value traits
			}
			return(nd)
		}
	)
)


#
#f <- function(...) {
#	mc <- match.call()
#	mc2 <- match.call(expand.dots=FALSE)
#	ma <- match(x='a', table=names(mc))
#	return(list(mc = mc, mc2 = mc2, ma = ma, a = mc[ma], list(...)))
#}
#
#

