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
    stop_index = "numeric",
		plot = function(x=NULL) {
			if (is.null(x)) {
				pl <- .self$plot_internal()
			}
			return(pl)
		},
		dim = function(x=NULL) {
			if (is.null(x)) {
				return(c(nrows=nrow(A), ncol=ncol(A)))
			}
		}
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

			# set up calling as field:
			o <- plot_internal();
      return(.self)
    },
		write = function(x=NULL, from=NULL, to=NULL) {
			if (is.null(x)) stop("Must specify what to write.")
			if (is.null(from)) stop("Must specify stage name.")
			if (is.null(to)) to <- from

			cols <- (start_index[stage_names == from]):(stop_index[stage_names == from])
			rows <- (start_index[stage_names == to]):(stop_index[stage_names == to])
			A[rows,cols] <<- x
			return(read(from=from, to=to))	
		},
		read = function(from=NULL, to=NULL) {
			if (is.null(from)) stop("Must specify stage name.")
			if (is.null(to)) to <- from

			cols <- (start_index[stage_names == from]):(stop_index[stage_names == from])
			rows <- (start_index[stage_names == to]):(stop_index[stage_names == to])
			TMP <- A[rows,cols,drop=FALSE]
			colnames(TMP) <- cols
			rownames(TMP) <- rows
			return(TMP)	
		},
		plot_internal = function() {
			pl <- visualizeBlockMatrix(.self)
			return(pl)
		},
		output = function(path=NULL, iteration=NULL) {
			stop("Not implemented.")
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


setMethod(  
  f="[", 
  signature = signature(x='block_projection', i='numeric',j='numeric'), 
  definition = function(x, i, j) {
    return(x$A[i,j])
	}
)

setMethod(  
  f="[<-",
  signature = signature(x='block_projection', i='numeric',j='numeric'), 
  definition = function(x, i, j, value) {
    x$A[i,j] <- value
    return(x)
  }
)

setMethod(
	f="%*%",
	signature = signature(x='block_projection', y='block_distribution'),
	definition = function(x, y) {
		y$densities <- x$A %*% y$densities
		return(y)
	}
)



visualizeBlockMatrix <- function(M) {
	
	
	combinations <- as.matrix(expand.grid(M$stage_names,M$stage_names))
	
	dat_list <- apply(
	  X=combinations, MARGIN=1,
	  FUN=function(row, M) {
	    nom <- paste(row[1], "->", row[2], sep='')
	    mat <- M[row[1],row[2]]
	    cols_start <- min(as.numeric(colnames(mat)))
	    cols_stop  <- max(as.numeric(colnames(mat)))
	    rows_start <- min(as.numeric(rownames(mat)))
	    rows_stop  <- max(as.numeric(rownames(mat)))
	    ul <- as.numeric(c(cols_start-1 ,  (rows_start-1)))
	    ll <- as.numeric(c(cols_start-1 ,   rows_stop))
	    ur <- as.numeric(c(cols_stop    ,  (rows_start-1)))
	    lr <- as.numeric(c(cols_stop    ,   rows_stop))
	    dat <- data.frame(
	      id = rep(nom,4),
	      from = rep(row[1],4),
	      to = rep(row[2],4),
	      x = c(ul[1], ll[1], lr[1], ur[1]),
	      y = c(ul[2], ll[2], lr[2], ur[2])
	    )
	    return(dat)
	  },
	  M = M
	)
	
	dat_list <- do.call(what=rbind, args=dat_list)
	
	values <- data.frame(
	            id = unique(dat_list[,c('id')]),
	            value = 1:nrow(combinations))
	
	middles <- do.call(what=rbind, args=by(
	  data=dat_list,
	  INDICES=dat_list['id'],
	  FUN=function(dat) {
	    o <- data.frame(
	      id = unique(dat[['id']]),
	      ctr_x = mean(dat[['x']]),
	      ctr_y = mean(dat[['y']]))
	    return(o)
	  },
	  simplify=TRUE
	))
	
	
	values <- merge(x=values, y=middles, by='id')
	dat_list[['y']] <- dat_list[['y']] * -1
	
	pl <- ggplot(values, aes(fill = value)) + scale_y_reverse() +
	  geom_map(aes(map_id = id), map = dat_list) +
	# expand_limits(dat_list) + 
	  geom_text(aes(x=ctr_x, y=ctr_y, label=id, value=id)) +
	  xlab('') + ylab('') + theme_bw()
	
	return(pl)

}





